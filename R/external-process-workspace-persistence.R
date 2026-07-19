#' Create a canonical external-process workspace record
#'
#' @param result A workspace-backed `PopgenVCFExternalProcessResult`.
#' @return A validated `PopgenVCFExternalProcessWorkspace` object.
#' @export
new_external_process_workspace <- function(result) {
  if (!inherits(result, "PopgenVCFExternalProcessResult") || !is.list(result)) {
    stop("result must be a PopgenVCFExternalProcessResult", call. = FALSE)
  }
  validate_external_process_result(result)
  if (is.null(result$workspace) || !is.list(result$workspace)) {
    stop("result does not contain workspace provenance", call. = FALSE)
  }
  if (is.null(result$original_command_fingerprint)) {
    stop("workspace result is missing original command fingerprint", call. = FALSE)
  }
  workspace <- result$workspace
  record <- structure(
    list(
      command_fingerprint = as.character(result$original_command_fingerprint)[1],
      workspace_command_fingerprint = as.character(result$command_fingerprint)[1],
      process_status = as.character(result$status)[1],
      policy = workspace$policy,
      identifier = workspace$identifier,
      path = workspace$path,
      retained = workspace$retained,
      input_manifest = data.table::as.data.table(data.table::copy(workspace$input_manifest)),
      contents_fingerprint = workspace$contents_fingerprint,
      events = data.table::as.data.table(data.table::copy(workspace$events))
    ),
    class = "PopgenVCFExternalProcessWorkspace"
  )
  validate_external_process_workspace(record)
  record
}

workspace_sha256 <- function(value, field) {
  value <- as.character(value)[1]
  if (is.na(value) || !grepl("^[0-9a-f]{64}$", value)) {
    stop(field, " must be a lowercase SHA-256 digest", call. = FALSE)
  }
  value
}

#' Validate an external-process workspace record
#'
#' @param workspace A `PopgenVCFExternalProcessWorkspace` object.
#' @return `workspace`, invisibly.
#' @export
validate_external_process_workspace <- function(workspace) {
  if (!inherits(workspace, "PopgenVCFExternalProcessWorkspace") || !is.list(workspace)) {
    stop("workspace must be a PopgenVCFExternalProcessWorkspace", call. = FALSE)
  }
  required <- c(
    "command_fingerprint", "workspace_command_fingerprint", "process_status",
    "policy", "identifier", "path", "retained", "input_manifest",
    "contents_fingerprint", "events"
  )
  missing <- setdiff(required, names(workspace))
  if (length(missing)) {
    stop("external-process workspace is missing field(s): ",
         paste(missing, collapse = ", "), call. = FALSE)
  }

  workspace_sha256(workspace$command_fingerprint, "command_fingerprint")
  workspace_sha256(workspace$workspace_command_fingerprint,
                   "workspace_command_fingerprint")
  workspace_sha256(workspace$identifier, "identifier")
  workspace_sha256(workspace$contents_fingerprint, "contents_fingerprint")

  status <- as.character(workspace$process_status)[1]
  allowed_status <- c(
    "success", "nonzero_exit", "launch_failed", "timed_out",
    "cancelled", "resource_unavailable"
  )
  if (is.na(status) || !status %in% allowed_status) {
    stop("workspace contains an unsupported process status", call. = FALSE)
  }
  policy <- as.character(workspace$policy)[1]
  if (is.na(policy) || !nzchar(policy)) {
    stop("workspace policy must be a non-empty string", call. = FALSE)
  }
  retained <- as.logical(workspace$retained)[1]
  if (is.na(retained)) {
    stop("workspace retained must be TRUE or FALSE", call. = FALSE)
  }
  path <- as.character(workspace$path)[1]
  if (retained) {
    if (is.na(path) || !nzchar(path)) {
      stop("retained workspace must record a path", call. = FALSE)
    }
  } else if (!is.na(path)) {
    stop("cleaned workspace path must be missing", call. = FALSE)
  }

  manifest <- workspace$input_manifest
  if (!data.table::is.data.table(manifest)) {
    stop("workspace input_manifest must be a data table", call. = FALSE)
  }
  manifest_required <- c("source", "staged_name", "sha256")
  if (!all(manifest_required %in% names(manifest))) {
    stop("workspace input manifest is missing required columns", call. = FALSE)
  }
  if (nrow(manifest)) {
    source <- as.character(manifest$source)
    staged <- as.character(manifest$staged_name)
    sha256 <- as.character(manifest$sha256)
    if (anyNA(source) || any(!nzchar(source)) || anyNA(staged) ||
        any(!nzchar(staged)) || anyDuplicated(staged)) {
      stop("workspace input manifest contains invalid paths", call. = FALSE)
    }
    if (any(!grepl("^[0-9a-f]{64}$", sha256))) {
      stop("workspace input manifest contains invalid SHA-256 digests", call. = FALSE)
    }
    expected_order <- order(staged, source)
    if (!identical(expected_order, seq_len(nrow(manifest)))) {
      stop("workspace input manifest is not canonically ordered", call. = FALSE)
    }
  }

  events <- workspace$events
  if (!data.table::is.data.table(events) ||
      !all(c("sequence", "event", "detail") %in% names(events))) {
    stop("workspace events must be a canonical data table", call. = FALSE)
  }
  sequence <- suppressWarnings(as.integer(events$sequence))
  if (!nrow(events) || anyNA(sequence) ||
      !identical(sequence, seq_len(nrow(events)))) {
    stop("workspace event sequence must be contiguous", call. = FALSE)
  }
  event <- as.character(events$event)
  expected_prefix <- c(
    "workspace_created", "inputs_staged", "process_dispatched",
    "process_completed"
  )
  if (nrow(events) != 5L || !identical(event[1:4], expected_prefix)) {
    stop("workspace lifecycle events are malformed", call. = FALSE)
  }
  expected_terminal <- if (retained) "workspace_retained" else "workspace_cleaned"
  if (!identical(event[[5]], expected_terminal)) {
    stop("workspace retention state conflicts with lifecycle events", call. = FALSE)
  }
  if (!identical(as.character(events$detail[[4]]), status)) {
    stop("workspace process status conflicts with lifecycle events", call. = FALSE)
  }
  invisible(workspace)
}

external_process_workspace_sidecar_digest <- function(path) {
  digest::digest(file = path, algo = "sha256")
}

read_external_process_workspace_sidecar <- function(path) {
  lines <- readLines(path, warn = FALSE)
  if (length(lines) != 1L) {
    stop("external-process workspace SHA-256 sidecar is malformed", call. = FALSE)
  }
  fields <- strsplit(lines, "[[:space:]]+")[[1]]
  if (!length(fields) || !grepl("^[0-9a-f]{64}$", fields[[1]])) {
    stop("external-process workspace SHA-256 sidecar is malformed", call. = FALSE)
  }
  fields[[1]]
}

#' Write an external-process workspace record
#'
#' @param workspace A validated external-process workspace record or a
#'   workspace-backed process result.
#' @param path Destination `.rds` path.
#' @param overwrite Whether existing record and checksum files may be replaced.
#' @return The normalized path, invisibly.
#' @export
write_external_process_workspace <- function(workspace, path, overwrite = FALSE) {
  if (inherits(workspace, "PopgenVCFExternalProcessResult")) {
    workspace <- new_external_process_workspace(workspace)
  }
  validate_external_process_workspace(workspace)
  path <- normalizePath(path, mustWork = FALSE)
  checksum_path <- paste0(path, ".sha256")
  if (!overwrite && (file.exists(path) || file.exists(checksum_path))) {
    stop("external-process workspace record already exists", call. = FALSE)
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  envelope <- new_runtime_integrity_envelope("process_workspace", workspace)
  tmp <- tempfile("process-workspace-", tmpdir = dirname(path), fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(envelope, tmp, version = 3, compress = "xz")
  checksum <- external_process_workspace_sidecar_digest(tmp)
  if (!file.rename(tmp, path)) {
    stop("unable to install external-process workspace record", call. = FALSE)
  }
  writeLines(paste(checksum, basename(path)), checksum_path, useBytes = TRUE)
  invisible(path)
}

#' Read and verify an external-process workspace record
#'
#' @param path External-process workspace `.rds` path.
#' @return A validated `PopgenVCFExternalProcessWorkspace` object.
#' @export
read_external_process_workspace <- function(path) {
  checksum_path <- paste0(path, ".sha256")
  if (!file.exists(path) || !file.exists(checksum_path)) {
    stop("external-process workspace record and SHA-256 sidecar are required",
         call. = FALSE)
  }
  expected <- read_external_process_workspace_sidecar(checksum_path)
  observed <- external_process_workspace_sidecar_digest(path)
  if (!identical(expected, observed)) {
    stop("external-process workspace file checksum mismatch", call. = FALSE)
  }
  envelope <- tryCatch(
    readRDS(path),
    error = function(error) {
      stop("external-process workspace record is unreadable or truncated",
           call. = FALSE)
    }
  )
  if (!inherits(envelope, "PopgenVCFRuntimeEnvelope")) {
    stop("legacy unwrapped external-process workspace requires explicit migration",
         call. = FALSE)
  }
  if (!identical(envelope$kind, "process_workspace")) {
    stop("runtime integrity envelope is not a process workspace", call. = FALSE)
  }
  workspace <- runtime_integrity_payload(envelope)
  validate_external_process_workspace(workspace)
  workspace
}
