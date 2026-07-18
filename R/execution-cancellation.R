#' Create an execution cancellation token
#'
#' Cancellation tokens are mutable coordination objects checked at deterministic
#' module-launch boundaries. Cancellation never accepts partial module output.
#'
#' @param label Stable token identifier recorded in execution metadata.
#' @return A `PopgenVCFExecutionCancellationToken` environment.
#' @export
new_execution_cancellation_token <- function(label = "execution-cancellation") {
  label <- as.character(label)[1]
  if (is.na(label) || !nzchar(label)) {
    stop("label must be a non-empty string", call. = FALSE)
  }
  token <- new.env(parent = emptyenv())
  token$label <- label
  token$requested <- FALSE
  token$reason <- ""
  token$requested_at <- NA_character_
  class(token) <- "PopgenVCFExecutionCancellationToken"
  token
}

#' Print an execution cancellation token
#' @param x A cancellation token.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.PopgenVCFExecutionCancellationToken <- function(x, ...) {
  cat("<PopgenVCFExecutionCancellationToken>\n")
  cat("  label:", x$label, "\n")
  cat("  requested:", isTRUE(x$requested), "\n")
  if (isTRUE(x$requested)) cat("  reason:", x$reason, "\n")
  invisible(x)
}

#' Request deterministic execution cancellation
#'
#' @param token A cancellation token.
#' @param reason Human-readable cancellation reason.
#' @return `token`, invisibly.
#' @export
request_execution_cancellation <- function(token, reason = "Cancellation requested") {
  validate_execution_cancellation_token(token)
  reason <- as.character(reason)[1]
  if (is.na(reason) || !nzchar(reason)) {
    stop("reason must be a non-empty string", call. = FALSE)
  }
  if (!isTRUE(token$requested)) {
    token$requested <- TRUE
    token$reason <- reason
    token$requested_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  }
  invisible(token)
}

#' Validate an execution cancellation token
#' @param token A cancellation token.
#' @return `token`, invisibly.
#' @export
validate_execution_cancellation_token <- function(token) {
  if (!inherits(token, "PopgenVCFExecutionCancellationToken") || !is.environment(token)) {
    stop("token must be a PopgenVCFExecutionCancellationToken", call. = FALSE)
  }
  required <- c("label", "requested", "reason", "requested_at")
  if (!all(vapply(required, exists, logical(1), envir = token, inherits = FALSE))) {
    stop("cancellation token is missing required fields", call. = FALSE)
  }
  invisible(token)
}

cancellation_message <- function(token, module) {
  sprintf("Execution cancelled before module '%s': %s", module, token$reason)
}

cancellation_registry <- function(registry, token) {
  validate_execution_cancellation_token(token)
  out <- registry
  for (name in names(out$modules)) {
    original <- out$modules[[name]]$run
    out$modules[[name]]$run <- local({
      module_name <- name
      runner <- original
      function(analysis, context) {
        if (isTRUE(token$requested)) {
          stop(cancellation_message(token, module_name), call. = FALSE)
        }
        runner(analysis, context)
      }
    })
  }
  out
}

classify_cancellation_ledger <- function(ledger) {
  if (!nrow(ledger)) return(ledger)
  messages <- as.character(ledger[["error_message"]])
  cancelled <- ledger[["status"]] == "failed" &
    !is.na(messages) &
    grepl("Execution cancelled before module", messages, fixed = TRUE)
  ledger[["status"]][cancelled] <- "cancelled"
  ledger
}

cancellation_retry_policy <- function(retry_policy) {
  original <- retry_policy$retryable
  retry_policy$retryable <- function(module, error_message, attempt, ledger) {
    if (grepl("Execution cancelled before module", error_message, fixed = TRUE)) {
      return(FALSE)
    }
    original(module, error_message, attempt, ledger)
  }
  retry_policy
}

add_cancellation_metadata <- function(result, token, registry, checkpoint_path = NULL) {
  result$execution <- classify_cancellation_ledger(data.table::copy(result$execution))
  if (!is.null(result$attempt_execution)) {
    result$attempt_execution <- classify_cancellation_ledger(
      data.table::copy(result$attempt_execution)
    )
  }
  cancelled_modules <- result$execution$module[result$execution$status == "cancelled"]
  metadata <- result$engine %||% list()
  metadata$cancellation <- list(
    token = token$label,
    requested = isTRUE(token$requested),
    reason = token$reason,
    requested_at = token$requested_at,
    cancelled_modules = cancelled_modules
  )
  result$engine <- metadata
  result$analysis <- set_analysis_result(result$analysis, "execution_engine", metadata)
  result$analysis <- set_analysis_result(result$analysis, "execution_ledger", result$execution)
  if (!is.null(result$attempt_execution)) {
    result$analysis <- set_analysis_result(
      result$analysis, "execution_attempt_ledger", result$attempt_execution
    )
  }
  if (isTRUE(token$requested)) {
    result$checkpoint <- new_execution_checkpoint(result, registry)
    if (!is.null(checkpoint_path)) {
      write_execution_checkpoint(result$checkpoint, checkpoint_path, overwrite = TRUE)
      metadata$cancellation$checkpoint_path <- normalizePath(
        checkpoint_path, mustWork = FALSE
      )
      result$engine <- metadata
      result$analysis <- set_analysis_result(result$analysis, "execution_engine", metadata)
    }
  }
  result
}

#' Execute an analysis plan with cooperative cancellation
#'
#' @param analysis A `PopgenVCFAnalysis` object.
#' @param context Runtime context.
#' @param registry A `PopgenVCFRegistry` object.
#' @param plan An execution plan.
#' @param cancellation_token Cancellation token.
#' @param engine Execution engine.
#' @param retry_policy Retry policy. Cancellation is always non-retryable.
#' @param checkpoint_path Optional `.rds` path written when cancellation occurs.
#' @return An execution result with cancellation metadata and, when cancelled, a checkpoint.
#' @export
execute_analysis_plan_with_cancellation <- function(
    analysis,
    context,
    registry,
    plan,
    cancellation_token = new_execution_cancellation_token(),
    engine = new_execution_engine(fail_fast = FALSE),
    retry_policy = new_execution_retry_policy(),
    checkpoint_path = NULL) {
  validate_execution_cancellation_token(cancellation_token)
  guarded_registry <- cancellation_registry(registry, cancellation_token)
  result <- execute_analysis_plan_with_retries(
    analysis,
    context,
    guarded_registry,
    plan,
    engine,
    cancellation_retry_policy(retry_policy)
  )
  add_cancellation_metadata(result, cancellation_token, registry, checkpoint_path)
}

#' Execute an analysis registry with cooperative cancellation
#'
#' @inheritParams execute_analysis_plan_with_cancellation
#' @param selected Optional module names.
#' @return The result of [execute_analysis_plan_with_cancellation()].
#' @export
execute_analysis_registry_with_cancellation <- function(
    analysis,
    context,
    registry,
    selected = NULL,
    cancellation_token = new_execution_cancellation_token(),
    engine = new_execution_engine(fail_fast = FALSE),
    retry_policy = new_execution_retry_policy(),
    checkpoint_path = NULL) {
  plan <- plan_analysis_execution(registry, analysis$config, selected = selected)
  execute_analysis_plan_with_cancellation(
    analysis,
    context,
    registry,
    plan,
    cancellation_token,
    engine,
    retry_policy,
    checkpoint_path
  )
}
