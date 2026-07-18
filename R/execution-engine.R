#' Create an analysis execution engine
#'
#' The execution engine compiles an analysis registry into deterministic
#' dependency waves and executes modules subject to worker and resource-class
#' limits. Existing modules remain serial unless they explicitly declare
#' `parallel_safe = TRUE` in their registry contract.
#'
#' @param workers Maximum number of concurrently executing modules.
#' @param backend Execution backend. `"sequential"` is portable;
#'   `"multicore"` uses forked workers on non-Windows systems.
#' @param resource_limits Named integer vector giving the maximum concurrent
#'   modules for `light`, `standard`, `heavy`, and `external` resource classes.
#' @param fail_fast Stop immediately after the first failed module.
#' @return A validated `PopgenVCFExecutionEngine` object.
#' @export
new_execution_engine <- function(workers = 1L,
                                 backend = c("sequential", "multicore"),
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

execution_wave_map <- function(registry, order) {
  waves <- stats::setNames(integer(length(order)), order)
  for (name in order) {
    deps <- intersect(registry$modules[[name]]$requires, order)
    waves[[name]] <- if (length(deps)) max(waves[deps]) + 1L else 1L
  }
  waves
}

#' Compile an analysis execution plan
#'
#' @param registry A `PopgenVCFRegistry` object.
#' @param config Validated configuration.
#' @param selected Optional module names.
#' @return A `PopgenVCFExecutionPlan` containing the deterministic order,
#'   dependency waves, and a tabular schedule.
#' @export
plan_analysis_execution <- function(registry, config, selected = NULL) {
  if (!inherits(registry, "PopgenVCFRegistry")) {
    stop("registry must be a PopgenVCFRegistry", call. = FALSE)
  }
  order <- resolve_analysis_order(registry, config, selected)
  waves <- execution_wave_map(registry, order)
  table <- if (length(order)) {
    data.table::rbindlist(lapply(order, function(name) {
      module <- registry$modules[[name]]
      data.table::data.table(
        module = name,
        wave = waves[[name]],
        requires = paste(module$requires, collapse = ","),
        resource_class = module$resource_class,
        parallel_safe = isTRUE(module$parallel_safe)
      )
    }))
  } else {
    data.table::data.table(
      module = character(), wave = integer(), requires = character(),
      resource_class = character(), parallel_safe = logical()
    )
  }
  structure(list(order = order, waves = waves, table = table), class = "PopgenVCFExecutionPlan")
}

#' Print an analysis execution plan
#' @param x A `PopgenVCFExecutionPlan` object.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.PopgenVCFExecutionPlan <- function(x, ...) {
  cat("<PopgenVCFExecutionPlan>\n")
  cat("  modules:", length(x$order), "\n")
  cat("  waves:", if (length(x$waves)) max(x$waves) else 0L, "\n")
  invisible(x)
}

execution_batches <- function(plan, registry, engine) {
  if (!length(plan$order)) return(list())
  batches <- list()
  for (wave in sort(unique(unname(plan$waves)))) {
    pending <- plan$order[plan$waves[plan$order] == wave]
    while (length(pending)) {
      batch <- character()
      used <- stats::setNames(integer(length(engine$resource_limits)), names(engine$resource_limits))
      for (name in pending) {
        module <- registry$modules[[name]]
        cls <- module$resource_class
        can_parallel <- isTRUE(module$parallel_safe)
        if (!can_parallel && length(batch)) next
        if (!can_parallel) {
          batch <- name
          break
        }
        if (length(batch) >= engine$workers || used[[cls]] >= engine$resource_limits[[cls]]) next
        batch <- c(batch, name)
        used[[cls]] <- used[[cls]] + 1L
      }
      if (!length(batch)) batch <- pending[[1]]
      batches[[length(batches) + 1L]] <- batch
      pending <- setdiff(pending, batch)
    }
  }
  batches
}

run_engine_module <- function(name, analysis, context, registry) {
  module <- registry$modules[[name]]
  t0 <- proc.time()[["elapsed"]]
  value <- tryCatch(
    run_stage(name, module$run(analysis, context)),
    error = function(e) structure(list(error = e), class = "PopgenVCFEngineFailure")
  )
  list(name = name, value = value, elapsed = proc.time()[["elapsed"]] - t0)
}

validate_engine_module_output <- function(execution, analysis, context, registry) {
  name <- execution$name
  module <- registry$modules[[name]]
  out <- execution$value
  if (inherits(out, "PopgenVCFEngineFailure")) stop(out$error)
  if (!is.list(out) || is.null(out$analysis) || is.null(out$context)) {
    stop("Analysis module '", name, "' returned an invalid result", call. = FALSE)
  }
  candidate <- out$analysis
  missing_outputs <- setdiff(module$outputs, names(candidate$results))
  if (length(missing_outputs)) {
    stop("Module '", name, "' did not produce declared output(s): ",
         paste(missing_outputs, collapse = ", "), call. = FALSE)
  }
  validation <- module$validate(candidate$results[[module$outputs[[1]]]], candidate, out$context)
  assert_module_validation(validation, name)
  module_artifacts <- module_artifact_manifest(out)
  validate_module_artifacts(
    module_name = name,
    declared = module$artifacts %||% character(),
    manifest = module_artifacts,
    must_exist = isTRUE(module$artifacts_must_exist)
  )
  list(out = out, validation = validation, artifacts = module_artifacts)
}

merge_parallel_module <- function(analysis, context, execution, validated, registry) {
  name <- execution$name
  module <- registry$modules[[name]]
  if (!identical(validated$out$context, context)) {
    stop("Parallel-safe module '", name, "' modified the shared execution context", call. = FALSE)
  }
  for (output in module$outputs) {
    analysis$results[[output]] <- validated$out$analysis$results[[output]]
  }
  analysis$results$validation <- analysis$results$validation %||% list()
  analysis$results$validation[[name]] <- validated$validation
  analysis <- record_analysis_timing(analysis, name, execution$elapsed)
  analysis <- record_analysis_message(analysis, "SUCCESS", name, "completed and validated")
  validate_analysis(analysis)
  analysis
}

#' Execute an analysis plan
#'
#' @param analysis A `PopgenVCFAnalysis` object.
#' @param context Runtime context shared by module runners.
#' @param registry A `PopgenVCFRegistry` object.
#' @param plan A plan returned by [plan_analysis_execution()].
#' @param engine A `PopgenVCFExecutionEngine` object.
#' @return A list containing updated state, execution order, plan, artifacts,
#'   and engine metadata.
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
  completed <- character()

  for (batch in batches) {
    merge_batch <- length(batch) > 1L
    use_parallel <- merge_batch && identical(engine$backend, "multicore")
    executions <- if (use_parallel) {
      parallel::mclapply(
        batch, run_engine_module, analysis = analysis, context = context,
        registry = registry, mc.cores = min(engine$workers, length(batch)),
        mc.preschedule = FALSE
      )
    } else {
      lapply(batch, run_engine_module, analysis = analysis, context = context, registry = registry)
    }

    for (execution in executions) {
      validated <- tryCatch(
        validate_engine_module_output(execution, analysis, context, registry),
        error = function(e) e
      )
      if (inherits(validated, "error")) {
        analysis <- record_analysis_message(analysis, "ERROR", execution$name, conditionMessage(validated))
        if (engine$fail_fast) stop(validated)
        next
      }

      if (merge_batch) {
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
    }
  }

  metadata <- list(
    backend = engine$backend,
    workers = engine$workers,
    resource_limits = engine$resource_limits,
    fail_fast = engine$fail_fast,
    waves = if (length(plan$waves)) max(plan$waves) else 0L,
    batches = unname(batches)
  )
  analysis <- set_analysis_result(analysis, "execution_engine", metadata)
  list(
    analysis = analysis, context = context, order = completed,
    plan = plan, artifacts = artifacts, engine = metadata
  )
}
