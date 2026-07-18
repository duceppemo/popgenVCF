#' Create an execution retry policy
#'
#' Retry policies are explicit and bounded. By default no failure is retryable;
#' callers must opt in with a classifier that identifies transient failures.
#'
#' @param max_attempts Maximum number of executions for a module, including its
#'   initial attempt.
#' @param retryable Function receiving `module`, `error_message`, `attempt`, and
#'   `ledger`, and returning one logical value.
#' @param backoff_seconds Non-negative delay before each retry. Supply one value
#'   or a vector of length `max_attempts - 1`.
#' @param label Stable human-readable policy identifier recorded in metadata.
#' @return A validated `PopgenVCFExecutionRetryPolicy` object.
#' @export
new_execution_retry_policy <- function(
    max_attempts = 1L,
    retryable = function(module, error_message, attempt, ledger) FALSE,
    backoff_seconds = 0,
    label = "no-retry") {
  max_attempts <- as.integer(max_attempts)[1]
  if (is.na(max_attempts) || max_attempts < 1L) {
    stop("max_attempts must be a positive integer", call. = FALSE)
  }
  if (!is.function(retryable)) {
    stop("retryable must be a function", call. = FALSE)
  }
  backoff_seconds <- as.numeric(backoff_seconds)
  expected <- max(1L, max_attempts - 1L)
  if (!length(backoff_seconds) || anyNA(backoff_seconds) ||
      any(!is.finite(backoff_seconds)) || any(backoff_seconds < 0) ||
      !(length(backoff_seconds) %in% c(1L, expected))) {
    stop(
      "backoff_seconds must be one non-negative value or one value per retry",
      call. = FALSE
    )
  }
  label <- as.character(label)[1]
  if (is.na(label) || !nzchar(label)) {
    stop("label must be a non-empty string", call. = FALSE)
  }
  structure(
    list(
      max_attempts = max_attempts,
      retryable = retryable,
      backoff_seconds = backoff_seconds,
      label = label
    ),
    class = "PopgenVCFExecutionRetryPolicy"
  )
}

#' Print an execution retry policy
#' @param x A `PopgenVCFExecutionRetryPolicy` object.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.PopgenVCFExecutionRetryPolicy <- function(x, ...) {
  cat("<PopgenVCFExecutionRetryPolicy>\n")
  cat("  label:", x$label, "\n")
  cat("  maximum attempts:", x$max_attempts, "\n")
  cat("  retry delays:", paste(x$backoff_seconds, collapse = ", "), "seconds\n")
  invisible(x)
}

retry_policy_delay <- function(policy, completed_attempt) {
  index <- min(completed_attempt, length(policy$backoff_seconds))
  policy$backoff_seconds[[index]]
}

retry_policy_classification <- function(policy, ledger, attempt) {
  failed <- ledger[status == "failed"]
  if (!nrow(failed)) {
    return(stats::setNames(logical(), character()))
  }
  values <- vapply(seq_len(nrow(failed)), function(i) {
    value <- policy$retryable(
      module = failed$module[[i]],
      error_message = failed$error_message[[i]],
      attempt = attempt,
      ledger = data.table::copy(ledger)
    )
    if (length(value) != 1L || is.na(value)) {
      stop("retryable must return one non-missing logical value", call. = FALSE)
    }
    isTRUE(value)
  }, logical(1))
  stats::setNames(values, failed$module)
}

finalize_retry_ledgers <- function(attempt_ledger, plan) {
  final <- attempt_ledger[, .SD[.N], by = module]
  final <- final[match(plan$order, module)]
  counts <- attempt_ledger[status %in% c("success", "failed"), .N, by = module]
  final[, attempt_count := counts$N[match(module, counts$module)]]
  final[is.na(attempt_count), attempt_count := 0L]
  final[, recovered := status == "success" & attempt_count > 1L]
  final
}

#' Execute an analysis plan with bounded retries
#'
#' Each attempt uses the standard execution engine and validation contracts.
#' Successful modules are removed from subsequent plans, while failed and
#' blocked modules are eligible for another attempt only when every observed
#' failure is classified as retryable. All attempt rows are retained.
#'
#' @param analysis A `PopgenVCFAnalysis` object.
#' @param context Runtime context shared by module runners.
#' @param registry A `PopgenVCFRegistry` object.
#' @param plan A `PopgenVCFExecutionPlan` object.
#' @param engine Execution engine. Retry orchestration always evaluates a full
#'   attempt with `fail_fast = FALSE`.
#' @param retry_policy A `PopgenVCFExecutionRetryPolicy` object.
#' @return A regular execution result augmented with `attempt_execution` and
#'   retry metadata.
#' @export
execute_analysis_plan_with_retries <- function(
    analysis,
    context,
    registry,
    plan,
    engine = new_execution_engine(fail_fast = FALSE),
    retry_policy = new_execution_retry_policy()) {
  if (!inherits(plan, "PopgenVCFExecutionPlan")) {
    stop("plan must be a PopgenVCFExecutionPlan", call. = FALSE)
  }
  if (!inherits(engine, "PopgenVCFExecutionEngine")) {
    stop("engine must be a PopgenVCFExecutionEngine", call. = FALSE)
  }
  if (!inherits(retry_policy, "PopgenVCFExecutionRetryPolicy")) {
    stop("retry_policy must be a PopgenVCFExecutionRetryPolicy", call. = FALSE)
  }

  attempt_engine <- engine
  attempt_engine$fail_fast <- FALSE
  current_analysis <- analysis
  current_context <- context
  current_plan <- plan
  completed <- character()
  artifacts <- new_artifact_manifest()
  attempt_rows <- list()
  last_result <- NULL
  stopped_non_retryable <- FALSE

  for (attempt in seq_len(retry_policy$max_attempts)) {
    result <- execute_analysis_plan(
      current_analysis, current_context, registry, current_plan, attempt_engine
    )
    ledger <- data.table::copy(result$execution)
    ledger[, attempt := attempt]
    attempt_rows[[length(attempt_rows) + 1L]] <- ledger

    completed <- union(completed, result$order)
    current_analysis <- result$analysis
    current_context <- result$context
    artifacts <- append_artifact_manifest(artifacts, result$artifacts)
    last_result <- result

    unresolved <- setdiff(plan$order, completed)
    if (!length(unresolved)) break

    classification <- retry_policy_classification(retry_policy, ledger, attempt)
    if (!length(classification) || !all(classification) ||
        attempt >= retry_policy$max_attempts) {
      stopped_non_retryable <- length(classification) > 0L && !all(classification)
      break
    }

    delay <- retry_policy_delay(retry_policy, attempt)
    if (delay > 0) Sys.sleep(delay)
    current_plan <- checkpoint_remaining_plan(plan, registry, unresolved)
  }

  attempt_ledger <- data.table::rbindlist(attempt_rows, use.names = TRUE, fill = TRUE)
  final_ledger <- finalize_retry_ledgers(attempt_ledger, plan)
  recovered <- final_ledger[recovered == TRUE, module]
  exhausted <- final_ledger[status %in% c("failed", "blocked"), module]
  metadata <- last_result$engine %||% list()
  metadata$retry <- list(
    policy = retry_policy$label,
    max_attempts = retry_policy$max_attempts,
    attempts_run = max(attempt_ledger$attempt),
    recovered_modules = recovered,
    exhausted_modules = exhausted,
    stopped_non_retryable = stopped_non_retryable
  )

  current_analysis <- set_analysis_result(
    current_analysis, "execution_attempt_ledger", attempt_ledger
  )
  current_analysis <- set_analysis_result(
    current_analysis, "execution_ledger", final_ledger
  )
  current_analysis <- set_analysis_result(
    current_analysis, "execution_engine", metadata
  )

  list(
    analysis = current_analysis,
    context = current_context,
    order = plan$order[plan$order %in% completed],
    plan = plan,
    artifacts = artifacts,
    engine = metadata,
    execution = final_ledger,
    attempt_execution = attempt_ledger
  )
}

#' Execute an analysis registry with bounded retries
#'
#' @param analysis A `PopgenVCFAnalysis` object.
#' @param context Runtime context.
#' @param registry A `PopgenVCFRegistry` object.
#' @param selected Optional module names.
#' @param engine Execution engine.
#' @param retry_policy Retry policy.
#' @return The result of [execute_analysis_plan_with_retries()].
#' @export
execute_analysis_registry_with_retries <- function(
    analysis,
    context,
    registry,
    selected = NULL,
    engine = new_execution_engine(fail_fast = FALSE),
    retry_policy = new_execution_retry_policy()) {
  plan <- plan_analysis_execution(registry, analysis$config, selected = selected)
  execute_analysis_plan_with_retries(
    analysis, context, registry, plan, engine, retry_policy
  )
}
