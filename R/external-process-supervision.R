#' Create an external-process supervision policy
#'
#' @param timeout_seconds Maximum elapsed process time in seconds. Use `Inf` to
#'   disable timeout enforcement.
#' @param resource_policy Execution resource policy used for admission.
#' @param label Stable supervision-policy label.
#' @return A validated `PopgenVCFExternalProcessSupervisionPolicy` object.
#' @export
new_external_process_supervision_policy <- function(
    timeout_seconds = Inf,
    resource_policy = new_execution_resource_policy(),
    label = "default-process-supervision") {
  timeout_seconds <- as.numeric(timeout_seconds)[1]
  if (is.na(timeout_seconds) || timeout_seconds <= 0) {
    stop("timeout_seconds must be positive or Inf", call. = FALSE)
  }
  if (!inherits(resource_policy, "PopgenVCFExecutionResourcePolicy")) {
    stop("resource_policy must be a PopgenVCFExecutionResourcePolicy", call. = FALSE)
  }
  label <- as.character(label)[1]
  if (is.na(label) || !nzchar(label)) {
    stop("label must be a non-empty string", call. = FALSE)
  }
  structure(
    list(
      timeout_seconds = timeout_seconds,
      resource_policy = resource_policy,
      label = label
    ),
    class = "PopgenVCFExternalProcessSupervisionPolicy"
  )
}

#' Print an external-process supervision policy
#' @param x A supervision policy.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.PopgenVCFExternalProcessSupervisionPolicy <- function(x, ...) {
  cat("<PopgenVCFExternalProcessSupervisionPolicy>\n")
  cat("  label:", x$label, "\n")
  cat("  timeout:", x$timeout_seconds, "seconds\n")
  cat("  resources:", x$resource_policy$label, "\n")
  invisible(x)
}

supervision_result <- function(command, status, started, finished,
                               admission, policy, cancellation_token = NULL,
                               exit_status = NA_integer_, stdout = "", stderr = "",
                               resolved_executable = NA_character_,
                               error_message = NA_character_,
                               termination = list(
                                 requested = FALSE,
                                 reason = NA_character_,
                                 tree_cleanup = "not_required"
                               )) {
  result <- new_external_process_result(
    command = command,
    status = status,
    exit_status = exit_status,
    stdout = stdout,
    stderr = stderr,
    started_at = format(started, tz = "UTC", usetz = TRUE),
    finished_at = format(finished, tz = "UTC", usetz = TRUE),
    elapsed_seconds = as.numeric(difftime(finished, started, units = "secs")),
    resolved_executable = resolved_executable,
    error_message = error_message
  )
  result$supervision <- list(
    policy = policy$label,
    backend = "processx",
    timeout_seconds = policy$timeout_seconds,
    resource_policy = policy$resource_policy$label,
    admission = admission,
    cancellation = if (is.null(cancellation_token)) NULL else list(
      token = cancellation_token$label,
      requested = isTRUE(cancellation_token$requested),
      reason = cancellation_token$reason,
      requested_at = cancellation_token$requested_at
    ),
    termination = termination,
    cleanup = "completed"
  )
  result
}

#' Run an external command under deterministic supervision
#'
#' The command is admitted against declared resources before launch. Cancellation
#' is checked at deterministic launch and completion boundaries. The `processx`
#' backend enforces elapsed-time limits and cleans the complete descendant process
#' tree on timeout and abnormal R-side exit paths.
#'
#' @param command A validated `PopgenVCFExternalCommand`.
#' @param requirements Declared process requirements.
#' @param supervision_policy Supervision policy.
#' @param cancellation_token Optional execution cancellation token.
#' @return A `PopgenVCFExternalProcessResult` with supervision metadata.
#' @export
run_supervised_external_command <- function(
    command,
    requirements = new_module_resource_requirements(),
    supervision_policy = new_external_process_supervision_policy(),
    cancellation_token = NULL) {
  validate_external_command(command)
  if (!inherits(supervision_policy, "PopgenVCFExternalProcessSupervisionPolicy")) {
    stop("supervision_policy must be a PopgenVCFExternalProcessSupervisionPolicy", call. = FALSE)
  }
  if (!is.null(cancellation_token)) {
    validate_execution_cancellation_token(cancellation_token)
  }

  admission <- admit_execution_resources(
    requirements,
    supervision_policy$resource_policy
  )
  started <- Sys.time()

  if (!admission$admitted) {
    finished <- Sys.time()
    return(supervision_result(
      command, "resource_unavailable", started, finished, admission,
      supervision_policy, cancellation_token,
      error_message = sprintf(
        "Resource admission rejected: %s",
        paste(admission$exceeded, collapse = ", ")
      )
    ))
  }

  if (!is.null(cancellation_token) && isTRUE(cancellation_token$requested)) {
    finished <- Sys.time()
    return(supervision_result(
      command, "cancelled", started, finished, admission,
      supervision_policy, cancellation_token,
      error_message = cancellation_token$reason
    ))
  }

  resolved <- resolve_external_executable(command$executable)
  if (is.na(resolved)) {
    finished <- Sys.time()
    return(supervision_result(
      command, "launch_failed", started, finished, admission,
      supervision_policy, cancellation_token,
      error_message = sprintf("Executable not found: %s", command$executable)
    ))
  }

  execution <- NULL
  execution_error <- NULL
  execution <- tryCatch(
    processx::run(
      command = resolved,
      args = command$args,
      error_on_status = FALSE,
      wd = command$working_directory,
      timeout = supervision_policy$timeout_seconds,
      stdout = "|",
      stderr = "|",
      env = if (length(command$environment)) command$environment else NULL,
      cleanup_tree = TRUE,
      windows_hide_window = TRUE
    ),
    error = function(e) {
      execution_error <<- conditionMessage(e)
      NULL
    }
  )
  finished <- Sys.time()

  if (!is.null(execution_error)) {
    return(supervision_result(
      command, "launch_failed", started, finished, admission,
      supervision_policy, cancellation_token,
      resolved_executable = resolved,
      error_message = execution_error,
      termination = list(
        requested = FALSE,
        reason = "launch_failed",
        tree_cleanup = "completed"
      )
    ))
  }

  timed_out <- isTRUE(execution$timeout)
  exit_status <- if (timed_out) 124L else as.integer(execution$status)
  status <- if (timed_out) {
    "timed_out"
  } else if (!is.null(cancellation_token) && isTRUE(cancellation_token$requested)) {
    "cancelled"
  } else if (identical(exit_status, 0L)) {
    "success"
  } else {
    "nonzero_exit"
  }

  error_message <- if (identical(status, "timed_out")) {
    sprintf("Process exceeded timeout of %s seconds", supervision_policy$timeout_seconds)
  } else if (identical(status, "cancelled")) {
    cancellation_token$reason
  } else {
    NA_character_
  }

  supervision_result(
    command, status, started, finished, admission, supervision_policy,
    cancellation_token, exit_status, execution$stdout, execution$stderr,
    resolved, error_message,
    termination = list(
      requested = timed_out,
      reason = if (timed_out) "timeout" else NA_character_,
      tree_cleanup = if (timed_out) "completed" else "not_required"
    )
  )
}
