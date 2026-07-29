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

new_execution_ledger <- function(plan, registry, batches) {
  batch_map <- stats::setNames(integer(length(plan$order)), plan$order)
  for (i in seq_along(batches)) batch_map[batches[[i]]] <- i
  if (!length(plan$order)) {
    return(data.table::data.table(
      module = character(), wave = integer(), batch = integer(),
      requires = character(), resource_class = character(),
      parallel_safe = logical(), status = character(),
      elapsed_seconds = numeric(), error_message = character(),
      blocked_by = character()
    ))
  }
  data.table::rbindlist(lapply(plan$order, function(name) {
    module <- registry$modules[[name]]
    data.table::data.table(
      module = name,
      wave = unname(plan$waves[[name]]),
      batch = unname(batch_map[[name]]),
      requires = paste(module$requires, collapse = ","),
      resource_class = module$resource_class,
      parallel_safe = isTRUE(module$parallel_safe),
      status = "pending",
      elapsed_seconds = NA_real_,
      error_message = "",
      blocked_by = ""
    )
  }))
}

ledger_status <- function(ledger, modules) {
  stats::setNames(ledger$status[match(modules, ledger$module)], modules)
}

update_execution_ledger <- function(ledger, module, status,
                                    elapsed_seconds = NA_real_,
                                    error_message = "", blocked_by = character()) {
  row <- match(module, ledger$module)
  ledger$status[[row]] <- status
  ledger$elapsed_seconds[[row]] <- elapsed_seconds
  ledger$error_message[[row]] <- as.character(error_message)[1]
  ledger$blocked_by[[row]] <- paste(blocked_by, collapse = ",")
  ledger
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
