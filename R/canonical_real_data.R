#' Create a licensed canonical real-data descriptor
#'
#' @param id Stable dataset identifier.
#' @param version Dataset release version.
#' @param title Human-readable title.
#' @param license SPDX identifier or explicit license label.
#' @param citation Dataset citation.
#' @param files Data frame with `filename`, `sha256`, and optional `size_bytes` and `source` columns.
#' @param organism Organism label.
#' @param analyses Supported analysis identifiers.
#' @param metadata Additional named metadata.
#' @return A validated `PopgenVCFCanonicalDataset`.
#' @export
new_canonical_dataset <- function(id, version, title, license, citation, files,
                                  organism = "unspecified", analyses = character(),
                                  metadata = list()) {
  scalar <- function(x, label) {
    if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x)))
      stop(label, " must be one non-empty string", call. = FALSE)
    trimws(x)
  }
  files <- as.data.frame(files, stringsAsFactors = FALSE)
  required <- c("filename", "sha256")
  if (!all(required %in% names(files)) || !nrow(files))
    stop("files must contain at least one filename and sha256", call. = FALSE)
  files$filename <- basename(as.character(files$filename))
  files$sha256 <- tolower(as.character(files$sha256))
  if (any(!nzchar(files$filename)) || anyDuplicated(files$filename))
    stop("dataset filenames must be unique non-empty basenames", call. = FALSE)
  if (any(!grepl("^[a-f0-9]{64}$", files$sha256)))
    stop("every canonical dataset file requires a SHA256 checksum", call. = FALSE)
  if (!"size_bytes" %in% names(files)) files$size_bytes <- NA_real_
  files$size_bytes <- as.numeric(files$size_bytes)
  if (any(!is.na(files$size_bytes) & (!is.finite(files$size_bytes) | files$size_bytes < 0)))
    stop("size_bytes must be nonnegative or NA", call. = FALSE)
  if (!"source" %in% names(files)) files$source <- NA_character_
  files$source <- as.character(files$source)
  files <- files[order(files$filename), c("filename", "sha256", "size_bytes", "source"), drop = FALSE]
  if (!is.list(metadata) || (length(metadata) && is.null(names(metadata))))
    stop("metadata must be a named list", call. = FALSE)
  x <- structure(list(
    schema_version = "1.0", id = tolower(scalar(id, "id")),
    version = scalar(as.character(version), "version"), title = scalar(title, "title"),
    license = scalar(license, "license"), citation = scalar(citation, "citation"),
    organism = scalar(organism, "organism"), analyses = sort(unique(tolower(as.character(analyses)))),
    files = files, metadata = metadata
  ), class = "PopgenVCFCanonicalDataset")
  validate_canonical_dataset(x)
}

#' Validate a canonical real-data descriptor
#' @param x A canonical dataset descriptor.
#' @return `x`, invisibly.
#' @export
validate_canonical_dataset <- function(x) {
  if (!inherits(x, "PopgenVCFCanonicalDataset"))
    stop("x must be a PopgenVCFCanonicalDataset", call. = FALSE)
  if (!identical(x$schema_version, "1.0")) stop("unsupported canonical dataset schema", call. = FALSE)
  if (!nzchar(x$license) || !nzchar(x$citation))
    stop("canonical datasets require license and citation metadata", call. = FALSE)
  if (!nrow(x$files) || any(!grepl("^[a-f0-9]{64}$", x$files$sha256)))
    stop("canonical dataset files must be checksum-pinned", call. = FALSE)
  invisible(x)
}

#' Verify a materialized canonical dataset
#'
#' @param descriptor Canonical dataset descriptor.
#' @param directory Directory containing dataset files.
#' @param strict_size Enforce declared file sizes when available.
#' @return Deterministic verification table.
#' @export
verify_canonical_dataset <- function(descriptor, directory, strict_size = TRUE) {
  validate_canonical_dataset(descriptor)
  rows <- lapply(seq_len(nrow(descriptor$files)), function(i) {
    spec <- descriptor$files[i, , drop = FALSE]
    path <- file.path(directory, spec$filename)
    exists <- file.exists(path)
    size <- if (exists) unname(file.info(path)$size) else NA_real_
    checksum <- if (exists) tolower(digest::digest(path, algo = "sha256", file = TRUE)) else NA_character_
    size_ok <- is.na(spec$size_bytes) || !isTRUE(strict_size) || identical(as.numeric(size), as.numeric(spec$size_bytes))
    data.frame(filename = spec$filename, exists = exists, expected_size = spec$size_bytes,
               observed_size = size, size_ok = size_ok, expected_sha256 = spec$sha256,
               observed_sha256 = checksum, checksum_ok = exists && identical(checksum, spec$sha256),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out$passed <- out$exists & out$size_ok & out$checksum_ok
  rownames(out) <- NULL
  out
}

#' Materialize a canonical dataset from explicit local sources
#'
#' @param descriptor Canonical dataset descriptor.
#' @param destination Destination directory.
#' @param source_dir Optional local mirror directory.
#' @param allow_download Permit explicit downloads from descriptor file sources.
#' @param quiet Suppress download progress.
#' @return Verified destination directory.
#' @export
materialize_canonical_dataset <- function(descriptor, destination, source_dir = NULL,
                                          allow_download = FALSE, quiet = TRUE) {
  validate_canonical_dataset(descriptor)
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  for (i in seq_len(nrow(descriptor$files))) {
    spec <- descriptor$files[i, , drop = FALSE]
    target <- file.path(destination, spec$filename)
    if (file.exists(target) && isTRUE(verify_canonical_dataset(descriptor, destination)$passed[i])) next
    candidate <- if (!is.null(source_dir)) file.path(source_dir, spec$filename) else ""
    temporary <- tempfile(pattern = paste0(spec$filename, "."), tmpdir = destination)
    on.exit(unlink(temporary), add = TRUE)
    if (nzchar(candidate) && file.exists(candidate)) {
      if (!file.copy(candidate, temporary, overwrite = TRUE)) stop("failed to copy canonical dataset file", call. = FALSE)
    } else if (isTRUE(allow_download) && !is.na(spec$source) && nzchar(spec$source)) {
      status <- tryCatch(utils::download.file(spec$source, temporary, mode = "wb", quiet = quiet), error = identity)
      if (inherits(status, "error") || !identical(as.integer(status), 0L))
        stop("failed to download canonical dataset file: ", spec$filename, call. = FALSE)
    } else {
      stop("canonical dataset file is unavailable locally and downloads are disabled: ", spec$filename, call. = FALSE)
    }
    observed <- tolower(digest::digest(temporary, algo = "sha256", file = TRUE))
    if (!identical(observed, spec$sha256)) stop("SHA256 verification failed: ", spec$filename, call. = FALSE)
    if (!is.na(spec$size_bytes) && !identical(as.numeric(file.info(temporary)$size), as.numeric(spec$size_bytes)))
      stop("file-size verification failed: ", spec$filename, call. = FALSE)
    if (!file.rename(temporary, target) && !file.copy(temporary, target, overwrite = TRUE))
      stop("failed to install canonical dataset file: ", spec$filename, call. = FALSE)
  }
  evidence <- verify_canonical_dataset(descriptor, destination)
  if (!all(evidence$passed)) stop("canonical dataset verification failed", call. = FALSE)
  normalizePath(destination)
}

#' Compare canonical and external numerical results
#'
#' @param observed,reference Data frames containing aligned identifiers and values.
#' @param id_cols Identifier columns used for deterministic alignment.
#' @param value_cols Numeric columns to compare.
#' @param tolerance Absolute tolerance, scalar or named vector.
#' @param tool,tool_version External tool provenance.
#' @return A deterministic comparison table.
#' @export
compare_external_results <- function(observed, reference, id_cols, value_cols,
                                     tolerance = 1e-8, tool, tool_version) {
  observed <- as.data.frame(observed, stringsAsFactors = FALSE)
  reference <- as.data.frame(reference, stringsAsFactors = FALSE)
  required <- unique(c(id_cols, value_cols))
  if (!all(required %in% names(observed)) || !all(required %in% names(reference)))
    stop("observed and reference tables are missing required columns", call. = FALSE)
  if (!length(id_cols) || !length(value_cols)) stop("id_cols and value_cols must be non-empty", call. = FALSE)
  key <- function(x) do.call(paste, c(x[id_cols], sep = "\r"))
  if (anyDuplicated(key(observed)) || anyDuplicated(key(reference))) stop("comparison identifiers must be unique", call. = FALSE)
  merged <- merge(reference[required], observed[required], by = id_cols, all = TRUE,
                  suffixes = c("_reference", "_observed"), sort = TRUE)
  tol <- rep(as.numeric(tolerance), length.out = length(value_cols)); names(tol) <- value_cols
  rows <- lapply(value_cols, function(column) {
    ref <- as.numeric(merged[[paste0(column, "_reference")]])
    obs <- as.numeric(merged[[paste0(column, "_observed")]])
    delta <- abs(obs - ref)
    data.frame(merged[id_cols], metric = column, reference = ref, observed = obs,
               absolute_error = delta, tolerance = tol[[column]],
               status = ifelse(is.na(ref) | is.na(obs), "missing", ifelse(delta <= tol[[column]], "pass", "fail")),
               tool = as.character(tool)[1L], tool_version = as.character(tool_version)[1L],
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

#' Write canonical real-data validation evidence
#' @param descriptor Canonical dataset descriptor.
#' @param directory Materialized dataset directory.
#' @param output_dir Evidence directory.
#' @param comparisons Optional external comparison table.
#' @return Named paths to deterministic evidence files.
#' @export
write_canonical_validation_evidence <- function(descriptor, directory, output_dir,
                                                comparisons = NULL) {
  validate_canonical_dataset(descriptor)
  verification <- verify_canonical_dataset(descriptor, directory)
  if (!all(verification$passed)) stop("cannot write evidence for an invalid canonical dataset", call. = FALSE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  paths <- list(descriptor = file.path(output_dir, "canonical_dataset.tsv"),
                verification = file.path(output_dir, "canonical_dataset_verification.tsv"),
                comparisons = file.path(output_dir, "external_comparisons.tsv"),
                methods = file.path(output_dir, "canonical_validation_methods.md"))
  descriptor_table <- data.frame(id = descriptor$id, version = descriptor$version,
    title = descriptor$title, license = descriptor$license, citation = descriptor$citation,
    organism = descriptor$organism, analyses = paste(descriptor$analyses, collapse = ","),
    stringsAsFactors = FALSE)
  data.table::fwrite(descriptor_table, paths$descriptor, sep = "\t", quote = FALSE)
  data.table::fwrite(verification, paths$verification, sep = "\t", quote = FALSE, na = "NA")
  if (!is.null(comparisons)) data.table::fwrite(comparisons, paths$comparisons, sep = "\t", quote = FALSE, na = "NA")
  writeLines(paste0("Dataset ", descriptor$id, " version ", descriptor$version,
    " was materialized explicitly and verified against SHA-256 checksums. External numerical comparisons, when supplied, used identifier alignment and declared absolute tolerances."),
    paths$methods, useBytes = TRUE)
  if (is.null(comparisons)) paths$comparisons <- NA_character_
  paths
}
