scheduler_metadata_required_columns <- function() {
  c("module", "wave", "batch", "requires", "dispatch_sequence",
    "completion_sequence", "merge_sequence", "worker_pid")
}

#' Create canonical scheduler metadata
#'
#' @param metadata A data frame containing scheduler records.
#' @return A validated `PopgenVCFSchedulerMetadata` data table.
#' @export
new_scheduler_metadata <- function(metadata) {
  if (!is.data.frame(metadata)) stop("metadata must be a data frame", call. = FALSE)
  metadata <- data.table::as.data.table(data.table::copy(metadata))
  data.table::setattr(metadata, "class",
    unique(c("PopgenVCFSchedulerMetadata", class(metadata))))
  validate_scheduler_metadata(metadata)
  metadata
}

validate_scheduler_sequence <- function(values, field, n) {
  values <- as.integer(values)
  present <- !is.na(values)
  if (any(values[present] < 1L) || anyDuplicated(values[present])) {
    stop("scheduler metadata ", field, " must contain unique positive integers", call. = FALSE)
  }
  if (any(present) && !identical(sort(values[present]), seq_len(sum(present)))) {
    stop("scheduler metadata ", field, " must be contiguous from one", call. = FALSE)
  }
  invisible(values)
}

#' Validate scheduler metadata
#'
#' @param metadata Scheduler metadata.
#' @return `metadata`, invisibly.
#' @export
validate_scheduler_metadata <- function(metadata) {
  if (!inherits(metadata, "PopgenVCFSchedulerMetadata") ||
      !data.table::is.data.table(metadata)) {
    stop("metadata must be a PopgenVCFSchedulerMetadata data table", call. = FALSE)
  }
  missing <- setdiff(scheduler_metadata_required_columns(), names(metadata))
  if (length(missing)) stop("scheduler metadata is missing required column(s): ",
    paste(missing, collapse = ", "), call. = FALSE)
  modules <- as.character(metadata$module)
  if (anyNA(modules) || any(!nzchar(modules)) || anyDuplicated(modules)) {
    stop("scheduler metadata module identities must be unique and non-empty", call. = FALSE)
  }
  wave <- as.integer(metadata$wave)
  batch <- as.integer(metadata$batch)
  if (anyNA(wave) || any(wave < 1L)) stop("scheduler metadata waves must be positive integers", call. = FALSE)
  if (anyNA(batch) || any(batch < 1L)) stop("scheduler metadata batches must be positive integers", call. = FALSE)
  requires <- strsplit(as.character(metadata$requires), ",", fixed = TRUE)
  requires <- lapply(requires, function(x) x[nzchar(x)])
  unknown <- setdiff(unique(unlist(requires, use.names = FALSE)), modules)
  if (length(unknown)) stop("scheduler metadata contains unknown dependencies: ",
    paste(unknown, collapse = ", "), call. = FALSE)
  for (i in seq_along(requires)) {
    if (length(requires[[i]]) && any(wave[match(requires[[i]], modules)] >= wave[[i]])) {
      stop("scheduler metadata dependencies must precede dependent waves", call. = FALSE)
    }
  }
  validate_scheduler_sequence(metadata$dispatch_sequence, "dispatch_sequence", nrow(metadata))
  validate_scheduler_sequence(metadata$completion_sequence, "completion_sequence", nrow(metadata))
  validate_scheduler_sequence(metadata$merge_sequence, "merge_sequence", nrow(metadata))
  worker <- as.integer(metadata$worker_pid)
  if (any(!is.na(worker) & worker < 1L)) stop("scheduler metadata worker_pid values must be positive", call. = FALSE)
  invisible(metadata)
}

scheduler_metadata_sidecar_digest <- function(path) digest::digest(file = path, algo = "sha256")

read_scheduler_metadata_sidecar <- function(path) {
  lines <- readLines(path, warn = FALSE)
  if (length(lines) != 1L) stop("scheduler metadata SHA-256 sidecar is malformed", call. = FALSE)
  fields <- strsplit(lines, "[[:space:]]+")[[1]]
  if (!length(fields) || !grepl("^[0-9a-f]{64}$", fields[[1]])) {
    stop("scheduler metadata SHA-256 sidecar is malformed", call. = FALSE)
  }
  fields[[1]]
}

#' Write scheduler metadata
#' @param metadata Scheduler metadata or compatible data frame.
#' @param path Destination `.rds` path.
#' @param overwrite Whether existing files may be replaced.
#' @return Normalized path, invisibly.
#' @export
write_scheduler_metadata <- function(metadata, path, overwrite = FALSE) {
  if (!inherits(metadata, "PopgenVCFSchedulerMetadata")) metadata <- new_scheduler_metadata(metadata)
  validate_scheduler_metadata(metadata)
  path <- normalizePath(path, mustWork = FALSE)
  sidecar <- paste0(path, ".sha256")
  if (!overwrite && (file.exists(path) || file.exists(sidecar))) stop("scheduler metadata already exists", call. = FALSE)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  envelope <- new_runtime_integrity_envelope("scheduler_metadata", metadata)
  tmp <- tempfile("scheduler-metadata-", tmpdir = dirname(path), fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(envelope, tmp, version = 3, compress = "xz")
  checksum <- scheduler_metadata_sidecar_digest(tmp)
  if (!file.rename(tmp, path)) stop("unable to install scheduler metadata", call. = FALSE)
  writeLines(paste(checksum, basename(path)), sidecar, useBytes = TRUE)
  invisible(path)
}

#' Read and verify scheduler metadata
#' @param path Scheduler metadata `.rds` path.
#' @return Validated scheduler metadata.
#' @export
read_scheduler_metadata <- function(path) {
  sidecar <- paste0(path, ".sha256")
  if (!file.exists(path) || !file.exists(sidecar)) stop("scheduler metadata and SHA-256 sidecar are required", call. = FALSE)
  if (!identical(read_scheduler_metadata_sidecar(sidecar), scheduler_metadata_sidecar_digest(path))) {
    stop("scheduler metadata file checksum mismatch", call. = FALSE)
  }
  envelope <- tryCatch(readRDS(path), error = function(e) stop("scheduler metadata is unreadable or truncated", call. = FALSE))
  if (!inherits(envelope, "PopgenVCFRuntimeEnvelope")) {
    stop("legacy unwrapped scheduler metadata requires explicit migration", call. = FALSE)
  }
  if (!identical(envelope$kind, "scheduler_metadata")) stop("runtime integrity envelope is not scheduler metadata", call. = FALSE)
  metadata <- runtime_integrity_payload(envelope)
  validate_scheduler_metadata(metadata)
  metadata
}
