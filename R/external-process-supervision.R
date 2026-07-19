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
                               error_message = NA_character_) {
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
    timeout_seconds = policy$timeout_seconds,
    resource_policy = policy$resource_policy$label,
    admission = admission,
    cancellation = if (is.null(cancellation_token)) NULL else list(
      token = cancellation_token$label,
      requested = isTRUE(cancellation_token$requested),
      reason = cancellation_token$reason,
      requested_at = cancellation_token$requested_at
    ),
    cleanup = "completed"
  )
  result
}

#' Run an external command under deterministic supervision
#'
#' The command is admitted against declared resources before launch. Cancellation
#' is checked at deterministic launch and completion boundaries. Base R timeout
#' enforcement terminates commands that exceed the configured elapsed-time limit.
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

  stdout_path <- tempfile("popgenvcf-supervised-stdout-")
  stderr_path <- tempfile("popgenvcf-supervised-stderr-")
  on.exit(unlink(c(stdout_path, stderr_path), force = TRUE), add = TRUE)

  old_directory <- getwd()
  on.exit(setwd(old_directory), add = TRUE)
  setwd(command$working_directory)

  environment <- if (length(command$environment)) {
    paste0(names(command$environment), "=", unname(command$environment))
  } else {
    character()
  }
  timeout <- if (is.infinite(supervision_policy$timeout_seconds)) {
    0
  } else {
    supervision_policy$timeout_seconds
  }

  execution_error <- NULL
  exit_status <- tryCatch(
    suppressWarnings(system2(
      resolved,
      args = command$args,
      stdout = stdout_path,
      stderr = stderr_path,
      env = environment,
      wait = TRUE,
      timeout = timeout
    )),
    error = function(e) {
      execution_error <<- conditionMessage(e)
      NA_integer_
    }
  )
  finished <- Sys.time()
  stdout <- read_process_output(stdout_path)
  stderr <- read_process_output(stderr_path)

  status <- if (!is.null(execution_error)) {
    "launch_failed"
  } else if (identical(as.integer(exit_status), 124L)) {
    "timed_out"
  } else if (!is.null(cancellation_token) && isTRUE(cancellation_token$requested)) {
    "cancelled"
  } else if (identical(as.integer(exit_status), 0L)) {
    "success"
  } else {
    "nonzero_exit"
  }

  error_message <- if (!is.null(execution_error)) {
    execution_error
  } else if (identical(status, "timed_out")) {
    sprintf("Process exceeded timeout of %s seconds", supervision_policy$timeout_seconds)
  } else if (identical(status, "cancelled")) {
    cancellation_token$reason
  } else {
    NA_character_
  }

  supervision_result(
    command, status, started, finished, admission, supervision_policy,
    cancellation_token, exit_status, stdout, stderr, resolved, error_message
  )
}
