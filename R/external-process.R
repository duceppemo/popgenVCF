#' Create an external command specification
#'
#' Command specifications are validated, immutable contracts for deterministic
#' external-process execution. They contain no shell command string: executable
#' and arguments remain separate so provenance and quoting are unambiguous.
#'
#' @param executable Executable name or path.
#' @param args Character vector of command-line arguments.
#' @param working_directory Existing working directory.
#' @param environment Optional named character vector of environment overrides.
#' @param label Stable command label recorded in provenance.
#' @return A validated `PopgenVCFExternalCommand` object.
#' @export
new_external_command <- function(executable,
                                 args = character(),
                                 working_directory = getwd(),
                                 environment = character(),
                                 label = basename(executable)) {
  build_external_command(executable, args, working_directory, environment, label,
                         require_directory = TRUE)
}

# The constructor's body, with the one check that depends on the machine made
# optional. validate_external_command() used to rebuild through the public
# constructor, so a command stopped validating the moment its working directory
# was removed -- which the default workspace policy does on every successful
# run. A workspace-backed result could therefore never be validated or
# persisted, and no persisted result could be read back on another machine.
# A recorded command is checked against its own fingerprint instead.
build_external_command <- function(executable, args, working_directory,
                                   environment, label, require_directory) {
  executable <- as.character(executable)[1]
  args <- as.character(args)
  working_directory <- as.character(working_directory)[1]
  environment_names <- names(environment)
  environment <- as.character(environment)
  names(environment) <- environment_names
  label <- as.character(label)[1]

  if (is.na(executable) || !nzchar(executable)) {
    stop("executable must be a non-empty string", call. = FALSE)
  }
  if (anyNA(args)) {
    stop("args must not contain missing values", call. = FALSE)
  }
  if (is.na(working_directory) || !nzchar(working_directory) ||
      (require_directory && !dir.exists(working_directory))) {
    stop("working_directory must be an existing directory", call. = FALSE)
  }
  if (length(environment) &&
      (is.null(names(environment)) || any(!nzchar(names(environment))) ||
       anyDuplicated(names(environment)) || anyNA(environment))) {
    stop("environment must be a uniquely named character vector", call. = FALSE)
  }
  if (is.na(label) || !nzchar(label)) {
    stop("label must be a non-empty string", call. = FALSE)
  }

  if (length(environment)) {
    environment <- environment[order(names(environment))]
  }
  specification <- list(
    executable = executable,
    args = args,
    working_directory = if (require_directory) normalizePath(working_directory, mustWork = TRUE) else working_directory,
    environment = environment,
    label = label
  )
  specification$fingerprint <- digest::digest(
    specification,
    algo = "sha256",
    serialize = TRUE
  )
  structure(specification, class = "PopgenVCFExternalCommand")
}

#' Validate an external command specification
#' @param command A `PopgenVCFExternalCommand` object.
#' @return `command`, invisibly.
#' @export
validate_external_command <- function(command) {
  if (!inherits(command, "PopgenVCFExternalCommand") || !is.list(command)) {
    stop("command must be a PopgenVCFExternalCommand", call. = FALSE)
  }
  required <- c(
    "executable", "args", "working_directory", "environment", "label",
    "fingerprint"
  )
  if (!all(required %in% names(command))) {
    stop("external command is missing required fields", call. = FALSE)
  }
  rebuilt <- build_external_command(
    command$executable,
    command$args,
    command$working_directory,
    command$environment,
    command$label,
    require_directory = FALSE
  )
  if (!identical(command$fingerprint, rebuilt$fingerprint)) {
    stop("external command fingerprint does not match its contents", call. = FALSE)
  }
  invisible(command)
}

#' Print an external command specification
#' @param x A `PopgenVCFExternalCommand` object.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.PopgenVCFExternalCommand <- function(x, ...) {
  cat("<PopgenVCFExternalCommand>\n")
  cat("  label:", x$label, "\n")
  cat("  executable:", x$executable, "\n")
  cat("  arguments:", length(x$args), "\n")
  cat("  fingerprint:", x$fingerprint, "\n")
  invisible(x)
}

resolve_external_executable <- function(executable) {
  has_separator <- grepl("[/\\\\]", executable)
  if (has_separator) {
    if (!file.exists(executable)) return(NA_character_)
    return(normalizePath(executable, mustWork = TRUE))
  }
  resolved <- Sys.which(executable)
  if (!nzchar(resolved)) NA_character_ else unname(resolved)
}

# Launch-time message for a missing executable or working directory, NA when
# the command can start. The directory is checked here, at launch, rather than
# by validate_external_command(): a recorded command must stay valid after
# its directory is gone, but starting one there must fail cleanly.
external_command_launch_problem <- function(command, resolved) {
  if (is.na(resolved)) return(sprintf("Executable not found: %s", command$executable))
  if (!dir.exists(command$working_directory)) {
    return(sprintf("Working directory does not exist: %s", command$working_directory))
  }
  NA_character_
}

read_process_output <- function(path) {
  # Decoded from bytes (read_captured_process_stream()), not readLines():
  # readLines() cut a line short at a NUL byte and marked invalid UTF-8 as
  # UTF-8, so a tool printing one Latin-1 character made every later grepl()
  # on its output fail with "unable to translate ... to a wide string". The
  # line-joined shape readLines() gave -- "\n" separators, no terminator --
  # is kept.
  text <- gsub("\r\n", "\n", read_captured_process_stream(path), fixed = TRUE)
  sub("\n$", "", text)
}

new_external_process_result <- function(command, status, exit_status,
                                        stdout, stderr, started_at,
                                        finished_at, elapsed_seconds,
                                        resolved_executable = NA_character_,
                                        error_message = NA_character_) {
  structure(
    list(
      command = command,
      command_fingerprint = command$fingerprint,
      status = status,
      exit_status = as.integer(exit_status),
      stdout = stdout,
      stderr = stderr,
      started_at = started_at,
      finished_at = finished_at,
      elapsed_seconds = as.numeric(elapsed_seconds),
      resolved_executable = resolved_executable,
      error_message = error_message
    ),
    class = "PopgenVCFExternalProcessResult"
  )
}

#' Run a validated external command
#'
#' Standard output and standard error are captured independently. Launch
#' failures, successful exits, and non-zero exits receive distinct normalized
#' states. A failed process result never represents accepted scientific output.
#'
#' @param command A validated `PopgenVCFExternalCommand` object.
#' @return A `PopgenVCFExternalProcessResult` object.
#' @export
run_external_command <- function(command) {
  validate_external_command(command)
  resolved <- resolve_external_executable(command$executable)
  started <- Sys.time()

  launch_problem <- external_command_launch_problem(command, resolved)
  if (!is.na(launch_problem)) {
    finished <- Sys.time()
    return(new_external_process_result(
      command = command,
      status = "launch_failed",
      exit_status = NA_integer_,
      stdout = "",
      stderr = "",
      started_at = format(started, tz = "UTC", usetz = TRUE),
      finished_at = format(finished, tz = "UTC", usetz = TRUE),
      elapsed_seconds = as.numeric(difftime(finished, started, units = "secs")),
      resolved_executable = resolved,
      error_message = launch_problem
    ))
  }

  stdout_path <- tempfile("popgenvcf-stdout-")
  stderr_path <- tempfile("popgenvcf-stderr-")
  on.exit(unlink(c(stdout_path, stderr_path), force = TRUE), add = TRUE)

  old_directory <- getwd()
  on.exit(setwd(old_directory), add = TRUE)
  setwd(command$working_directory)

  # base::system2() does NOT shell-quote its own `args` or `env` parameters
  # -- only the `command` executable itself (`paste(c(env, shQuote(command),
  # args), collapse = " ")`, per its own source). The docstring above
  # ("no shell command string ... quoting is unambiguous") was true of
  # new_external_command()'s own representation but did not carry through
  # to how system2() actually invokes it: an arg or environment value
  # containing shell metacharacters (or even a plain space) was pasted
  # unquoted into the shell command line system2() builds on Unix,
  # confirmed directly (an arg like "; touch pwned; echo" executes the
  # injected command instead of being passed through literally). Quoting
  # each arg individually with shQuote() keeps arguments correctly
  # separated (a shQuote()'d string is exactly one shell word) while
  # neutralizing injection; an environment value needs the assignment's
  # `=` left outside the quoting (`NAME=` + shQuote(value)), since
  # shQuote()-ing the whole "NAME=value" string stops the shell from
  # recognizing it as an environment-variable-assignment prefix at all
  # (confirmed directly: it is then read as the command itself).
  environment <- if (length(command$environment)) {
    paste0(names(command$environment), "=", vapply(command$environment, shQuote, character(1L)))
  } else {
    character()
  }
  quoted_args <- vapply(command$args, shQuote, character(1L))

  execution_error <- NULL
  exit_status <- tryCatch(
    suppressWarnings(system2(
      resolved,
      args = quoted_args,
      stdout = stdout_path,
      stderr = stderr_path,
      env = environment,
      wait = TRUE
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
  } else if (identical(as.integer(exit_status), 0L)) {
    "success"
  } else {
    "nonzero_exit"
  }

  new_external_process_result(
    command = command,
    status = status,
    exit_status = exit_status,
    stdout = stdout,
    stderr = stderr,
    started_at = format(started, tz = "UTC", usetz = TRUE),
    finished_at = format(finished, tz = "UTC", usetz = TRUE),
    elapsed_seconds = as.numeric(difftime(finished, started, units = "secs")),
    resolved_executable = resolved,
    error_message = if (is.null(execution_error)) NA_character_ else execution_error
  )
}

#' Print an external process result
#' @param x A `PopgenVCFExternalProcessResult` object.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.PopgenVCFExternalProcessResult <- function(x, ...) {
  cat("<PopgenVCFExternalProcessResult>\n")
  cat("  command:", x$command$label, "\n")
  cat("  status:", x$status, "\n")
  cat("  exit status:", x$exit_status, "\n")
  cat("  elapsed:", format(x$elapsed_seconds, digits = 6), "seconds\n")
  invisible(x)
}
