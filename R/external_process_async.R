#' Start an asynchronous supervised external command
#'
#' Starts a validated external command without blocking the R session. The
#' returned handle is polled with [poll_supervised_external_command()] and
#' finalized with [finalize_supervised_external_command()].
#'
#' @param command A validated `PopgenVCFExternalCommand`.
#' @param requirements Declared process resource requirements.
#' @param supervision_policy Supervision policy.
#' @param cancellation_token Optional execution cancellation token.
#' @param termination_grace_seconds Seconds allowed for graceful termination
#'   before forced process-tree cleanup.
#' @return A `PopgenVCFExternalProcessHandle`.
#' @export
start_supervised_external_command <- function(
    command,
    requirements = new_module_resource_requirements(),
    supervision_policy = new_external_process_supervision_policy(),
    cancellation_token = NULL,
    termination_grace_seconds = 1) {
  validate_external_command(command)
  if (!inherits(supervision_policy, "PopgenVCFExternalProcessSupervisionPolicy")) {
    stop("supervision_policy must be a PopgenVCFExternalProcessSupervisionPolicy", call. = FALSE)
  }
  if (!is.null(cancellation_token)) {
    validate_execution_cancellation_token(cancellation_token)
  }
  termination_grace_seconds <- as.numeric(termination_grace_seconds)[1]
  if (is.na(termination_grace_seconds) || termination_grace_seconds < 0) {
    stop("termination_grace_seconds must be non-negative", call. = FALSE)
  }

  admission <- admit_execution_resources(requirements, supervision_policy$resource_policy)
  started <- Sys.time()
  handle <- new.env(parent = emptyenv())
  handle$command <- command
  handle$requirements <- requirements
  handle$policy <- supervision_policy
  handle$cancellation_token <- cancellation_token
  handle$termination_grace_seconds <- termination_grace_seconds
  handle$admission <- admission
  handle$started_at <- started
  handle$finished_at <- NULL
  handle$process <- NULL
  handle$resolved_executable <- NA_character_
  handle$stdout <- ""
  handle$stderr <- ""
  handle$state <- "created"
  handle$terminal_status <- NULL
  handle$exit_status <- NA_integer_
  handle$error_message <- NA_character_
  handle$events <- data.frame(
    sequence = integer(), event = character(), timestamp = character(),
    detail = character(), stringsAsFactors = FALSE
  )
  class(handle) <- "PopgenVCFExternalProcessHandle"
  append_async_event(handle, "handle_created", command$label)

  if (!admission$admitted) {
    set_async_terminal(handle, "resource_unavailable", NA_integer_, sprintf(
      "Resource admission rejected: %s", paste(admission$exceeded, collapse = ", ")
    ))
    return(handle)
  }
  if (!is.null(cancellation_token) && isTRUE(cancellation_token$requested)) {
    set_async_terminal(handle, "cancelled", NA_integer_, cancellation_token$reason)
    return(handle)
  }

  resolved <- resolve_external_executable(command$executable)
  if (is.na(resolved)) {
    set_async_terminal(handle, "launch_failed", NA_integer_, sprintf(
      "Executable not found: %s", command$executable
    ))
    return(handle)
  }
  handle$resolved_executable <- resolved

  env <- if (length(command$environment)) {
    stats::setNames(unname(command$environment), names(command$environment))
  } else {
    character()
  }
  process <- tryCatch(
    processx::process$new(
      command = resolved,
      args = command$args,
      stdout = "|",
      stderr = "|",
      wd = command$working_directory,
      env = env,
      cleanup_tree = TRUE,
      cleanup = TRUE
    ),
    error = function(e) e
  )
  if (inherits(process, "error")) {
    set_async_terminal(handle, "launch_failed", NA_integer_, conditionMessage(process))
    return(handle)
  }

  handle$process <- process
  handle$state <- "running"
  append_async_event(handle, "process_started", as.character(process$get_pid()))
  handle
}

#' Poll an asynchronous supervised external command
#'
#' @param handle A `PopgenVCFExternalProcessHandle`.
#' @param timeout_milliseconds Maximum time to wait for process I/O during this
#'   poll operation.
#' @return `handle`, invisibly, after updating lifecycle state and output.
#' @export
poll_supervised_external_command <- function(handle, timeout_milliseconds = 0) {
  validate_external_process_handle(handle)
  timeout_milliseconds <- as.numeric(timeout_milliseconds)[1]
  if (is.na(timeout_milliseconds) || timeout_milliseconds < 0) {
    stop("timeout_milliseconds must be non-negative", call. = FALSE)
  }
  if (!identical(handle$state, "running")) {
    return(invisible(handle))
  }

  try(handle$process$poll_io(timeout_milliseconds), silent = TRUE)
  drain_async_output(handle)

  cancellation_requested <- !is.null(handle$cancellation_token) &&
    isTRUE(handle$cancellation_token$requested)
  elapsed <- as.numeric(difftime(Sys.time(), handle$started_at, units = "secs"))
  timed_out <- !is.infinite(handle$policy$timeout_seconds) &&
    elapsed >= handle$policy$timeout_seconds

  if (cancellation_requested || timed_out) {
    reason <- if (cancellation_requested) "cancelled" else "timed_out"
    message <- if (cancellation_requested) {
      handle$cancellation_token$reason
    } else {
      sprintf("Process exceeded timeout of %s seconds", handle$policy$timeout_seconds)
    }
    terminate_async_process(handle, reason)
    drain_async_output(handle)
    set_async_terminal(handle, reason, if (timed_out) 124L else NA_integer_, message)
    return(invisible(handle))
  }

  if (!handle$process$is_alive()) {
    drain_async_output(handle)
    exit_status <- as.integer(handle$process$get_exit_status())
    status <- if (identical(exit_status, 0L)) "success" else "nonzero_exit"
    set_async_terminal(handle, status, exit_status, NA_character_)
  }
  invisible(handle)
}

#' Read accumulated asynchronous process output
#'
#' @param handle A `PopgenVCFExternalProcessHandle`.
#' @return A list containing accumulated `stdout` and `stderr` strings.
#' @export
read_supervised_external_output <- function(handle) {
  validate_external_process_handle(handle)
  if (identical(handle$state, "running")) {
    drain_async_output(handle)
  }
  list(stdout = handle$stdout, stderr = handle$stderr)
}

#' Finalize an asynchronous supervised external command
#'
#' @param handle A `PopgenVCFExternalProcessHandle`.
#' @param wait Poll until the process reaches a terminal state.
#' @param poll_interval_milliseconds Poll interval when `wait = TRUE`.
#' @return A finalized `PopgenVCFExternalProcessResult`.
#' @export
finalize_supervised_external_command <- function(
    handle, wait = TRUE, poll_interval_milliseconds = 50) {
  validate_external_process_handle(handle)
  if (isTRUE(wait)) {
    while (identical(handle$state, "running")) {
      poll_supervised_external_command(handle, poll_interval_milliseconds)
    }
  }
  if (identical(handle$state, "running")) {
    stop("process is still running", call. = FALSE)
  }

  result <- supervision_result(
    handle$command,
    handle$terminal_status,
    handle$started_at,
    handle$finished_at,
    handle$admission,
    handle$policy,
    handle$cancellation_token,
    handle$exit_status,
    handle$stdout,
    handle$stderr,
    handle$resolved_executable,
    handle$error_message
  )
  result$supervision$backend <- "processx-async"
  result$supervision$termination_grace_seconds <- handle$termination_grace_seconds
  result$supervision$lifecycle_events <- handle$events
  result
}

#' Validate an asynchronous external-process handle
#' @param handle Object to validate.
#' @return `handle`, invisibly.
#' @export
validate_external_process_handle <- function(handle) {
  if (!inherits(handle, "PopgenVCFExternalProcessHandle") || !is.environment(handle)) {
    stop("handle must be a PopgenVCFExternalProcessHandle", call. = FALSE)
  }
  invisible(handle)
}

#' Print an asynchronous external-process handle
#' @param x A process handle.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.PopgenVCFExternalProcessHandle <- function(x, ...) {
  cat("<PopgenVCFExternalProcessHandle>\n")
  cat("  command:", x$command$label, "\n")
  cat("  state:", x$state, "\n")
  cat("  pid:", if (is.null(x$process)) NA_integer_ else x$process$get_pid(), "\n")
  invisible(x)
}

append_async_event <- function(handle, event, detail = "") {
  handle$events <- rbind(handle$events, data.frame(
    sequence = nrow(handle$events) + 1L,
    event = as.character(event),
    timestamp = format(Sys.time(), tz = "UTC", usetz = TRUE),
    detail = as.character(detail),
    stringsAsFactors = FALSE
  ))
}

set_async_terminal <- function(handle, status, exit_status, error_message) {
  handle$terminal_status <- status
  handle$exit_status <- as.integer(exit_status)
  handle$error_message <- error_message
  handle$finished_at <- Sys.time()
  handle$state <- status
  append_async_event(handle, "process_finalized", status)
  invisible(handle)
}

drain_async_output <- function(handle) {
  if (is.null(handle$process)) return(invisible(handle))
  out <- tryCatch(handle$process$read_output(), error = function(e) "")
  err <- tryCatch(handle$process$read_error(), error = function(e) "")
  if (length(out) && nzchar(out)) handle$stdout <- paste0(handle$stdout, out)
  if (length(err) && nzchar(err)) handle$stderr <- paste0(handle$stderr, err)
  invisible(handle)
}

terminate_async_process <- function(handle, reason) {
  handle$state <- "terminating"
  append_async_event(handle, "termination_requested", reason)
  graceful <- tryCatch({
    handle$process$interrupt()
    TRUE
  }, error = function(e) FALSE)
  append_async_event(handle, "graceful_termination", as.character(graceful))
  if (graceful && handle$termination_grace_seconds > 0) {
    try(handle$process$wait(as.integer(handle$termination_grace_seconds * 1000)), silent = TRUE)
  }
  if (handle$process$is_alive()) {
    killed <- tryCatch({
      if (is.function(handle$process$kill_tree)) {
        handle$process$kill_tree()
      } else {
        handle$process$kill()
      }
      TRUE
    }, error = function(e) FALSE)
    append_async_event(handle, "forced_tree_cleanup", as.character(killed))
  } else {
    append_async_event(handle, "forced_tree_cleanup", "not_required")
  }
  invisible(handle)
}
