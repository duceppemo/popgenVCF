# Loaded after canonical_production_execution.R so checksum verification policy
# remains independently testable and can evolve without changing acquisition.
verify_canonical_production_evidence <- function(output_dir) {
  output_dir <- canonical_production_dir(output_dir, "output_dir")
  checksum_path <- file.path(output_dir, "canonical-production-SHA256SUMS.txt")
  if (!file.exists(checksum_path)) {
    stop("canonical production checksum inventory is missing", call. = FALSE)
  }

  lines <- readLines(checksum_path, warn = FALSE)
  if (!length(lines) || any(!grepl("^[a-f0-9]{64}  [^/].+", lines))) {
    stop("canonical production checksum inventory is malformed", call. = FALSE)
  }
  expected_hashes <- substr(lines, 1L, 64L)
  relative <- substring(lines, 67L)
  if (anyDuplicated(relative) || any(startsWith(relative, "/")) ||
      any(grepl("(^|/)\\.\\.(/|$)", relative))) {
    stop("canonical production checksum paths are unsafe or duplicated", call. = FALSE)
  }

  paths <- file.path(output_dir, relative)
  if (any(!file.exists(paths)) || any(file.info(paths)$isdir) ||
      any(nzchar(Sys.readlink(paths)))) {
    stop("canonical production evidence file is missing or is not regular", call. = FALSE)
  }

  actual_files <- sort(list.files(
    output_dir,
    recursive = TRUE,
    full.names = TRUE,
    all.files = FALSE
  ))
  actual_files <- actual_files[
    normalizePath(actual_files, winslash = "/", mustWork = TRUE) !=
      normalizePath(checksum_path, winslash = "/", mustWork = TRUE)
  ]
  actual_relative <- vapply(
    actual_files,
    canonical_production_relative,
    character(1),
    root = output_dir
  )
  if (!identical(
    unname(sort(relative)),
    unname(sort(actual_relative))
  )) {
    stop("canonical production checksum inventory is incomplete", call. = FALSE)
  }

  observed_hashes <- vapply(paths, canonical_production_sha256, character(1))
  if (!identical(unname(observed_hashes), unname(expected_hashes))) {
    stop("canonical production evidence checksum verification failed", call. = FALSE)
  }
  invisible(TRUE)
}
