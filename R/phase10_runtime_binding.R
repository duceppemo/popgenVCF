# Phase 10.1.1 — public analysis execution binding

#' Execute a canonical public analysis request
#'
#' Delegates planning and execution to the authoritative Phase 8 execution
#' engine. This function is only a public-contract adapter; it does not
#' implement scheduling, retries, checkpointing, or module execution itself.
#'
#' @param request A validated `PopgenVCFPublicAPIRequest` whose operation is
#'   `analysis.execute`.
#' @param analysis A `PopgenVCFAnalysis` object.
#' @param context Runtime context supplied to registered modules.
#' @param registry A `PopgenVCFRegistry` object.
#' @param engine A `PopgenVCFExecutionEngine` object.
#' @param selected Optional module names passed to the authoritative planner.
#' @return A validated `PopgenVCFPublicAPIResponse`.
#' @export
execute_public_analysis <- function(
    request,
    analysis,
    context,
    registry,
    engine = new_execution_engine(),
    selected = NULL) {
  validate_public_analysis_request(request)
  if (!identical(request$operation_id, "analysis.execute")) {
    stop("execute_public_analysis requires an analysis.execute request.", call. = FALSE)
  }

  executed <- tryCatch(
    {
      plan <- plan_analysis_execution(
        registry = registry,
        config = analysis$config,
        selected = selected
      )
      execute_analysis_plan(
        analysis = analysis,
        context = context,
        registry = registry,
        plan = plan,
        engine = engine
      )
    },
    error = function(e) e
  )

  if (inherits(executed, "error")) {
    return(new_public_analysis_response(
      request = request,
      status = "failed",
      error = list(
        code = "runtime_execution_failed",
        message = conditionMessage(executed)
      )
    ))
  }

  ledger <- executed$execution
  statuses <- as.character(ledger$status)
  names(statuses) <- as.character(ledger$module)
  failed <- sort(names(statuses)[statuses == "failed"], method = "radix")
  blocked <- sort(names(statuses)[statuses == "blocked"], method = "radix")
  completed <- sort(names(statuses)[statuses == "success"], method = "radix")
  result_names <- sort(names(executed$analysis$results), method = "radix")

  values <- list(
    blocked_modules = blocked,
    completed_modules = completed,
    failed_modules = failed,
    result_names = result_names
  )

  if (length(failed) || length(blocked)) {
    details <- ledger$error_message[ledger$status == "failed"]
    details <- details[nzchar(details)]
    message <- if (length(details)) details[[1L]] else
      "One or more modules failed or were blocked."
    return(new_public_analysis_response(
      request = request,
      status = "failed",
      scientific_values = values,
      warnings = if (length(blocked)) "dependent_modules_blocked" else character(),
      error = list(code = "runtime_terminal_failure", message = message)
    ))
  }

  new_public_analysis_response(
    request = request,
    status = "completed",
    scientific_values = values
  )
}
