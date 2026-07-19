external_process_result_required_fields <- function() {
  c(
    "command", "command_fingerprint", "status", "exit_status", "stdout",
    "stderr", "started_at", "finished_at", "elapsed_seconds",
    "resolved_executable", "error_message"
  )
}

#' Validate an external process result
#'
#' @param result A `PopgenVCFExternalProcessResult` object.
#' @return `result`, invisibly.
#' @export
validate_external_process_result <- function(result) {
  if (!inherits(result, "PopgenVCFExternalProcessResult") || !is.list(result)) {
    stop("result must be a PopgenVCFExternalProcessResult", call. = FALSE)
  }
  missing <- setdiff(external_process_result_required_fields(), names(result))
  if (length(missing)) {
    stop(
      "external process result is missing required field(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  validate_external_command(result$command)
  if (!identical(result$command_fingerprint, result$command$fingerprint)) {
    stop("external process result command fingerprint mismatch", call. = FALSE)
  }

  allowed_status <- c(
    "success", "nonzero_exit", "launch_failed", "resource_unavailable",
    "cancelled", "timed_out"
  )
  status <- as.character(result$status)[1]
  if (is.na(status) || !status %in% allowed_status) {
    stop("external process result contains an unsupported status", call. = FALSE)
  }

  exit_status <- result$exit_status
  if (length(exit_status) != 1L ||
      (!is.na(exit_status) && !identical(as.integer(exit_status), exit_status))) {
    stop("external process result exit_status must be one integer or NA", call. = FALSE)
  }
  if (identical(status, "success") && !identical(exit_status, 0L)) {
    stop("successful external process results require exit status zero", call. = FALSE)
  }
  if (identical(status, "nonzero_exit") &&
      (is.na(exit_status) || identical(exit_status, 0L))) {
    stop("nonzero external process results require a non-zero exit status", call. = FALSE)
  }

  for (field in c("stdout", "stderr")) {
    value <- result[[field]]
    if (!is.character(value) || length(value) != 1L || is.na(value)) {
      stop("external process result output fields must be non-missing strings", call. = FALSE)
    }
  }
  for (field in c("resolved_executable", "error_message")) {
    value <- result[[field]]
    if (!is.character(value) || length(value) != 1L) {
      stop("external process result diagnostic fields must be scalar strings", call. = FALSE)
    }
  }

  elapsed <- as.numeric(result$elapsed_seconds)
  if (length(elapsed) != 1L || is.na(elapsed) || !is.finite(elapsed) || elapsed < 0) {
    stop("external process result elapsed_seconds must be finite and non-negative", call. = FALSE)
  }
  timestamps <- as.character(c(result$started_at, result$finished_at))
  if (length(timestamps) != 2L || anyNA(timestamps) || any(!nzchar(timestamps))) {
    stop("external process result timestamps must be non-empty strings", call. = FALSE)
  }

  invisible(result)
}

external_process_result_sidecar_digest <- function(path) {
  digest::digest(file = path, algo = "sha256")
}

read_external_process_result_sidecar <- function(path) {
  lines <- readLines(path, warn = FALSE)
  if (length(lines) != 1L) {
    stop("external process result SHA-256 sidecar is malformed", call. = FALSE)
  }
  fields <- strsplit(lines, "[[:space:]]+")[[1]]
  if (length(fields) < 1L || !grepl("^[0-9a-f]{64}$", fields[[1]])) {
    stop("external process result SHA-256 sidecar is malformed", call. = FALSE)
  }
  fields[[1]]
}

#' Write an external process result
#'
#' @param result A validated `PopgenVCFExternalProcessResult` object.
#' @param path Destination `.rds` path.
#' @param overwrite Whether existing result files may be replaced.
#' @return The normalized result path, invisibly.
#' @export
write_external_process_result <- function(result, path, overwrite = FALSE) {
  validate_external_process_result(result)
  path <- normalizePath(path, mustWork = FALSE)
  checksum_path <- paste0(path, ".sha256")
  if (!overwrite && (file.exists(path) || file.exists(checksum_path))) {
    stop("external process result already exists", call. = FALSE)
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  envelope <- new_runtime_integrity_envelope("process_result", result)
  tmp <- tempfile("process-result-", tmpdir = dirname(path), fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(envelope, tmp, version = 3, compress = "xz")
  checksum <- external_process_result_sidecar_digest(tmp)
  if (!file.rename(tmp, path)) {
    stop("unable to install external process result", call. = FALSE)
  }
  writeLines(paste(checksum, basename(path)), checksum_path, useBytes = TRUE)
  invisible(path)
}

#' Read and verify an external process result
#'
#' @param path External-process result `.rds` path.
#' @return A validated `PopgenVCFExternalProcessResult` object.
#' @export
read_external_process_result <- function(path) {
  checksum_path <- paste0(path, ".sha256")
  if (!file.exists(path) || !file.exists(checksum_path)) {
    stop("external process result and SHA-256 sidecar are required", call. = FALSE)
  }
  expected <- read_external_process_result_sidecar(checksum_path)
  observed <- external_process_result_sidecar_digest(path)
  if (!identical(expected, observed)) {
    stop("external process result file checksum mismatch", call. = FALSE)
  }
  envelope <- tryCatch(
    readRDS(path),
    error = function(error) {
      stop("external process result is unreadable or truncated", call. = FALSE)
    }
  )
  if (!inherits(envelope, "PopgenVCFRuntimeEnvelope")) {
    stop(
      "legacy unwrapped external process result requires explicit migration",
      call. = FALSE
    )
  }
  if (!identical(envelope$kind, "process_result")) {
    stop("runtime integrity envelope is not an external process result", call. = FALSE)
  }
  result <- runtime_integrity_payload(envelope)
  validate_external_process_result(result)
  result
}
