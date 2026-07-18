#' Create an execution timeout policy
#'
#' Timeout policies are explicit and fail closed. The default policy applies no
#' time limit. Per-module limits override the default limit.
#'
#' @param default_seconds Default elapsed-time budget in seconds. Use `Inf` for
#'   no timeout.
#' @param module_seconds Optional named numeric vector of per-module budgets.
#' @param label Stable policy identifier recorded in execution metadata.
#' @return A validated `PopgenVCFExecutionTimeoutPolicy` object.
#' @export
new_execution_timeout_policy <- function(default_seconds = Inf,
                                         module_seconds = numeric(),
                                         label = "no-timeout") {
  default_seconds <- as.numeric(default_seconds)[1]
  if (is.na(default_seconds) || default_seconds <= 0) {
    stop("default_seconds must be positive or Inf", call. = FALSE)
  }
  module_names <- names(module_seconds)
  module_seconds <- as.numeric(module_seconds)
  names(module_seconds) <- module_names
  if (length(module_seconds)) {
    if (is.null(names(module_seconds)) || any(!nzchar(names(module_seconds))) ||
        anyDuplicated(names(module_seconds)) || anyNA(module_seconds) ||
        any(module_seconds <= 0)) {
      stop("module_seconds must be a uniquely named vector of positive budgets", call. = FALSE)
    }
  }
  label <- as.character(label)[1]
  if (is.na(label) || !nzchar(label)) {
    stop("label must be a non-empty string", call. = FALSE)
  }
  structure(
    list(
      default_seconds = default_seconds,
      module_seconds = module_seconds,
      label = label
    ),
    class = "PopgenVCFExecutionTimeoutPolicy"
  )
}

#' Print an execution timeout policy
#' @param x A `PopgenVCFExecutionTimeoutPolicy` object.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.PopgenVCFExecutionTimeoutPolicy <- function(x, ...) {
  cat("<PopgenVCFExecutionTimeoutPolicy>\n")
  cat("  label:", x$label, "\n")
  cat("  default budget:", x$default_seconds, "seconds\n")
  cat("  module overrides:", length(x$module_seconds), "\n")
  invisible(x)
}

timeout_budget <- function(policy, module) {
  if (module %in% names(policy$module_seconds)) {
    return(unname(policy$module_seconds[[module]]))
  }
  policy$default_seconds
}

timeout_message <- function(module, seconds) {
  sprintf("Execution timeout for module '%s' after %s seconds", module, format(seconds, scientific = FALSE, trim = TRUE))
}

timeout_registry <- function(registry, policy) {
  out <- registry
  for (name in names(out$modules)) {
    seconds <- timeout_budget(policy, name)
    if (!is.finite(seconds)) next
    original <- out$modules[[name]]$run
    out$modules[[name]]$run <- local({
      module_name <- name
      budget <- seconds
      runner <- original
      function(analysis, context) {
        base::setTimeLimit(elapsed = budget, transient = TRUE)
        on.exit(base::setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE), add = TRUE)
        tryCatch(
          runner(analysis, context),
          error = function(e) {
            message <- conditionMessage(e)
            if (grepl("time limit", message, ignore.case = TRUE)) {
              stop(timeout_message(module_name, budget), call. = FALSE)
            }
            stop(e)
          }
        )
      }
    })
  }
  out
}

classify_timeout_ledger <- function(ledger) {
  if (!nrow(ledger)) return(ledger)
  ledger[
    status == "failed" & startsWith(error_message, "Execution timeout for module"),
    status := "timed_out"
  ]
  ledger
}

add_timeout_metadata <- function(result, policy) {
  result$execution <- classify_timeout_ledger(data.table::copy(result$execution))
  if (!is.null(result$attempt_execution)) {
    result$attempt_execution <- classify_timeout_ledger(data.table::copy(result$attempt_execution))
  }
  timed_out <- result$execution$module[result$execution$status == "timed_out"]
  metadata <- result$engine %||% list()
  metadata$timeout <- list(
    policy = policy$label,
    default_seconds = policy$default_seconds,
    module_seconds = policy$module_seconds,
    timed_out_modules = timed_out
  )
  result$engine <- metadata
  result$analysis <- set_analysis_result(result$analysis, "execution_engine", metadata)
  result$analysis <- set_analysis_result(result$analysis, "execution_ledger", result$execution)
  if (!is.null(result$attempt_execution)) {
    result$analysis <- set_analysis_result(
      result$analysis, "execution_attempt_ledger", result$attempt_execution
    )
  }
  result
}

#' Execute an analysis plan with elapsed-time budgets
#'
#' Timed-out modules are treated as failed by the core engine while dependency
#' propagation and retry decisions are made, then recorded as `timed_out` in the
#' returned final and attempt ledgers. Their outputs are never accepted.
#'
#' @param analysis A `PopgenVCFAnalysis` object.
#' @param context Runtime context.
#' @param registry A `PopgenVCFRegistry` object.
#' @param plan A `PopgenVCFExecutionPlan` object.
#' @param engine Execution engine.
#' @param timeout_policy Timeout policy.
#' @param retry_policy Retry policy.
#' @return A timeout-aware execution result.
#' @export
execute_analysis_plan_with_timeouts <- function(
    analysis,
    context,
    registry,
    plan,
    engine = new_execution_engine(fail_fast = FALSE),
    timeout_policy = new_execution_timeout_policy(),
    retry_policy = new_execution_retry_policy()) {
  if (!inherits(timeout_policy, "PopgenVCFExecutionTimeoutPolicy")) {
    stop("timeout_policy must be a PopgenVCFExecutionTimeoutPolicy", call. = FALSE)
  }
  timed_registry <- timeout_registry(registry, timeout_policy)
  result <- execute_analysis_plan_with_retries(
    analysis, context, timed_registry, plan, engine, retry_policy
  )
  add_timeout_metadata(result, timeout_policy)
}

#' Execute an analysis registry with elapsed-time budgets
#'
#' @param analysis A `PopgenVCFAnalysis` object.
#' @param context Runtime context.
#' @param registry A `PopgenVCFRegistry` object.
#' @param selected Optional module names.
#' @param engine Execution engine.
#' @param timeout_policy Timeout policy.
#' @param retry_policy Retry policy.
#' @return The result of [execute_analysis_plan_with_timeouts()].
#' @export
execute_analysis_registry_with_timeouts <- function(
    analysis,
    context,
    registry,
    selected = NULL,
    engine = new_execution_engine(fail_fast = FALSE),
    timeout_policy = new_execution_timeout_policy(),
    retry_policy = new_execution_retry_policy()) {
  plan <- plan_analysis_execution(registry, analysis$config, selected = selected)
  execute_analysis_plan_with_timeouts(
    analysis, context, registry, plan, engine, timeout_policy, retry_policy
  )
}
