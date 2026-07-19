# Portable deterministic scheduling layer for analysis execution.

#' Create an analysis execution engine
#'
#' @param workers Maximum number of concurrently executing modules.
#' @param backend Execution backend. `"sequential"` executes in the calling R
#'   process, `"multicore"` uses forked workers on non-Windows systems, and
#'   `"multisession"` uses portable PSOCK workers.
#' @param resource_limits Named integer vector giving the maximum concurrent
#'   modules for `light`, `standard`, `heavy`, and `external` resource classes.
#' @param fail_fast Stop immediately after the first failed module.
#' @return A validated `PopgenVCFExecutionEngine` object.
#' @export
new_execution_engine <- function(workers = 1L,
                                 backend = c("sequential", "multisession", "multicore"),
                                 resource_limits = NULL,
                                 fail_fast = TRUE) {
  backend <- match.arg(backend)
  workers <- as.integer(workers)[1]
  if (is.na(workers) || workers < 1L) {
    stop("workers must be a positive integer", call. = FALSE)
  }
  if (identical(backend, "multicore") && identical(.Platform$OS.type, "windows")) {
    stop("the multicore backend is not available on Windows", call. = FALSE)
  }
  if (is.null(resource_limits)) {
    resource_limits <- c(
      light = workers,
      standard = max(1L, workers %/% 2L),
      heavy = 1L,
      external = 1L
    )
  }
  required <- c("light", "standard", "heavy", "external")
  if (is.null(names(resource_limits)) || !all(required %in% names(resource_limits))) {
    stop("resource_limits must name light, standard, heavy, and external", call. = FALSE)
  }
  resource_limits <- as.integer(resource_limits[required])
  names(resource_limits) <- required
  if (anyNA(resource_limits) || any(resource_limits < 1L)) {
    stop("resource limits must be positive integers", call. = FALSE)
  }
  structure(
    list(
      workers = workers,
      backend = backend,
      resource_limits = resource_limits,
      fail_fast = isTRUE(fail_fast)
    ),
    class = "PopgenVCFExecutionEngine"
  )
}

#' Print an analysis execution engine
#' @param x A `PopgenVCFExecutionEngine` object.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.PopgenVCFExecutionEngine <- function(x, ...) {
  cat("<PopgenVCFExecutionEngine>\n")
  cat("  backend:", x$backend, "\n")
  cat("  workers:", x$workers, "\n")
  cat("  fail fast:", x$fail_fast, "\n")
  cat("  resource limits:", paste(names(x$resource_limits), x$resource_limits, sep = "=", collapse = ", "), "\n")
  invisible(x)
}

run_scheduled_engine_module <- function(name, analysis, context, registry) {
  module <- registry$modules[[name]]
  started <- Sys.time()
  t0 <- proc.time()[["elapsed"]]
  value <- tryCatch(
    run_stage(name, module$run(analysis, context)),
    error = function(e) structure(list(error = e), class = "PopgenVCFEngineFailure")
  )
  finished <- Sys.time()
  list(
    name = name,
    value = value,
    elapsed = proc.time()[["elapsed"]] - t0,
    started_at = format(started, tz = "UTC", usetz = TRUE),
    finished_at = format(finished, tz = "UTC", usetz = TRUE),
    finished_numeric = as.numeric(finished),
    worker_pid = Sys.getpid()
  )
}

run_execution_batch <- function(eligible, analysis, context, registry, engine) {
  if (length(eligible) <= 1L || identical(engine$backend, "sequential")) {
    return(lapply(
      eligible, run_scheduled_engine_module,
      analysis = analysis, context = context, registry = registry
    ))
  }
  if (identical(engine$backend, "multicore")) {
    return(parallel::mclapply(
      eligible, run_scheduled_engine_module,
      analysis = analysis, context = context, registry = registry,
      mc.cores = min(engine$workers, length(eligible)),
      mc.preschedule = FALSE
    ))
  }

  worker_count <- min(engine$workers, length(eligible))
  cluster <- parallel::makePSOCKcluster(worker_count)
  on.exit(parallel::stopCluster(cluster), add = TRUE)
  parallel::clusterSetRNGStream(cluster, iseed = 1L)
  parallel::parLapply(
    cluster,
    eligible,
    function(name, analysis, context, registry) {
      popgenVCF:::run_scheduled_engine_module(name, analysis, context, registry)
    },
    analysis = analysis,
    context = context,
    registry = registry
  )
}

scheduler_sequence <- function(executions, field, planned_order) {
  if (!length(executions)) return(stats::setNames(integer(), character()))
  values <- vapply(executions, `[[`, numeric(1), field)
  names(values) <- vapply(executions, `[[`, character(1), "name")
  ordered <- names(sort(values, method = "radix"))
  ties <- duplicated(values[ordered]) | duplicated(values[ordered], fromLast = TRUE)
  if (any(ties)) {
    tied <- ordered[ties]
    ordered[ties] <- tied[order(match(tied, planned_order))]
  }
  stats::setNames(seq_along(ordered), ordered)
}

ensure_scheduler_ledger <- function(ledger) {
  n <- nrow(ledger)
  ledger[, `:=`(
    dispatch_sequence = rep(NA_integer_, n),
    completion_sequence = rep(NA_integer_, n),
    merge_sequence = rep(NA_integer_, n),
    worker_pid = rep(NA_integer_, n)
  )]
  ledger
}

#' Execute an analysis plan with deterministic portable scheduling
#'
#' Parallel-safe modules in the same dependency wave may execute concurrently.
#' Worker completion timing is recorded, but validation and result merging always
#' follow the deterministic plan order.
#'
#' @param analysis A `PopgenVCFAnalysis` object.
#' @param context Runtime context shared by module runners.
#' @param registry A `PopgenVCFRegistry` object.
#' @param plan A plan returned by [plan_analysis_execution()].
#' @param engine A `PopgenVCFExecutionEngine` object.
#' @return A deterministic execution result with scheduler provenance.
#' @export
execute_analysis_plan <- function(analysis, context, registry, plan,
                                  engine = new_execution_engine()) {
  if (!inherits(engine, "PopgenVCFExecutionEngine")) {
    stop("engine must be a PopgenVCFExecutionEngine", call. = FALSE)
  }
  if (!inherits(plan, "PopgenVCFExecutionPlan")) {
    stop("plan must be a PopgenVCFExecutionPlan", call. = FALSE)
  }
  validate_analysis(analysis, "ordination")
  artifacts <- new_artifact_manifest()
  batches <- execution_batches(plan, registry, engine)
  ledger <- ensure_scheduler_ledger(new_execution_ledger(plan, registry, batches))
  completed <- character()
  dispatch_counter <- 0L
  merge_counter <- 0L
  scheduler_events <- list()

  for (batch_index in seq_along(batches)) {
    batch <- batches[[batch_index]]
    eligible <- character()
    for (name in batch) {
      requirements <- intersect(registry$modules[[name]]$requires, plan$order)
      requirement_status <- ledger_status(ledger, requirements)
      blocked_by <- names(requirement_status)[requirement_status %in% c("failed", "blocked")]
      if (length(blocked_by)) {
        ledger <- update_execution_ledger(ledger, name, "blocked", blocked_by = blocked_by)
        analysis <- record_analysis_message(
          analysis, "WARNING", name,
          paste("blocked by unsuccessful prerequisite(s):", paste(blocked_by, collapse = ", "))
        )
      } else {
        eligible <- c(eligible, name)
      }
    }
    if (!length(eligible)) next

    snapshot_batch <- length(batch) > 1L
    rows <- match(eligible, ledger$module)
    dispatch_ids <- seq.int(dispatch_counter + 1L, dispatch_counter + length(eligible))
    dispatch_counter <- max(dispatch_ids)
    ledger$dispatch_sequence[rows] <- dispatch_ids
    for (name in eligible) ledger <- update_execution_ledger(ledger, name, "running")

    executions <- run_execution_batch(eligible, analysis, context, registry, engine)
    completion <- scheduler_sequence(executions, "finished_numeric", eligible)
    for (execution in executions) {
      row <- match(execution$name, ledger$module)
      ledger$completion_sequence[[row]] <- unname(completion[[execution$name]])
      ledger$worker_pid[[row]] <- as.integer(execution$worker_pid)
    }

    executions <- executions[match(eligible, vapply(executions, `[[`, character(1), "name"))]
    for (execution in executions) {
      validated <- tryCatch(
        validate_engine_module_output(execution, analysis, context, registry),
        error = function(e) e
      )
      if (inherits(validated, "error")) {
        ledger <- update_execution_ledger(
          ledger, execution$name, "failed",
          elapsed_seconds = execution$elapsed,
          error_message = conditionMessage(validated)
        )
        analysis <- record_analysis_message(analysis, "ERROR", execution$name, conditionMessage(validated))
        if (engine$fail_fast) stop(validated)
        next
      }

      if (snapshot_batch) {
        analysis <- merge_parallel_module(analysis, context, execution, validated, registry)
      } else {
        analysis <- validated$out$analysis
        context <- validated$out$context
        analysis$results$validation <- analysis$results$validation %||% list()
        analysis$results$validation[[execution$name]] <- validated$validation
        analysis <- record_analysis_timing(analysis, execution$name, execution$elapsed)
        analysis <- record_analysis_message(analysis, "SUCCESS", execution$name, "completed and validated")
        validate_analysis(analysis)
      }
      artifacts <- append_artifact_manifest(artifacts, validated$artifacts)
      completed <- c(completed, execution$name)
      merge_counter <- merge_counter + 1L
      row <- match(execution$name, ledger$module)
      ledger$merge_sequence[[row]] <- merge_counter
      ledger <- update_execution_ledger(
        ledger, execution$name, "success", elapsed_seconds = execution$elapsed
      )
      scheduler_events[[length(scheduler_events) + 1L]] <- list(
        module = execution$name,
        batch = batch_index,
        dispatch_sequence = ledger$dispatch_sequence[[row]],
        completion_sequence = ledger$completion_sequence[[row]],
        merge_sequence = ledger$merge_sequence[[row]],
        worker_pid = ledger$worker_pid[[row]],
        started_at = execution$started_at,
        finished_at = execution$finished_at
      )
    }
  }

  status_counts <- as.list(table(factor(
    ledger$status,
    levels = c("pending", "running", "success", "failed", "blocked")
  )))
  names(status_counts) <- c("pending", "running", "success", "failed", "blocked")
  metadata <- list(
    backend = engine$backend,
    scheduler = if (identical(engine$backend, "multisession")) "PSOCK" else engine$backend,
    workers = engine$workers,
    resource_limits = engine$resource_limits,
    fail_fast = engine$fail_fast,
    deterministic_merge = TRUE,
    rng_stream = if (identical(engine$backend, "multisession")) "L'Ecuyer-CMRG:1" else NA_character_,
    waves = if (length(plan$waves)) max(plan$waves) else 0L,
    batches = unname(batches),
    events = scheduler_events,
    status_counts = status_counts
  )
  analysis <- set_analysis_result(analysis, "execution_engine", metadata)
  analysis <- set_analysis_result(analysis, "execution_ledger", ledger)
  list(
    analysis = analysis, context = context, order = completed,
    plan = plan, artifacts = artifacts, engine = metadata,
    execution = ledger
  )
}
