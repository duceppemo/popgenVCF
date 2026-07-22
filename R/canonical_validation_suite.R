#' Create a canonical validation suite
#'
#' @param id Stable suite identifier.
#' @param title Human-readable title.
#' @param fail_fast Stop after the first failed validation.
#' @param metadata Optional named metadata.
#' @return A `PopgenVCFCanonicalValidationSuite`.
#' @export
new_canonical_validation_suite <- function(id, title, fail_fast = TRUE, metadata = list()) {
  scalar <- function(x, label) {
    if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x)))
      stop(label, " must be one non-empty string", call. = FALSE)
    trimws(x)
  }
  if (!is.logical(fail_fast) || length(fail_fast) != 1L || is.na(fail_fast))
    stop("fail_fast must be TRUE or FALSE", call. = FALSE)
  if (!is.list(metadata) || (length(metadata) && is.null(names(metadata))))
    stop("metadata must be a named list", call. = FALSE)
  validate_canonical_validation_suite(structure(list(
    schema_version = "1.0", id = tolower(scalar(id, "id")),
    title = scalar(title, "title"), fail_fast = fail_fast,
    entries = list(), metadata = metadata
  ), class = "PopgenVCFCanonicalValidationSuite"))
}

#' Register a dataset validation in a suite
#'
#' @param suite Canonical validation suite.
#' @param id Registered canonical dataset identifier.
#' @param directory Materialized dataset directory.
#' @param validation Optional function accepting `(descriptor, directory)` and
#'   returning a data frame with a logical `passed` column.
#' @param replace Permit replacement of an existing entry.
#' @return Updated suite.
#' @export
register_canonical_validation <- function(suite, id, directory,
                                          validation = NULL, replace = FALSE) {
  validate_canonical_validation_suite(suite)
  id <- tolower(as.character(id)[1L])
  if (is.na(id) || !nzchar(id)) stop("id must be non-empty", call. = FALSE)
  if (!is.character(directory) || length(directory) != 1L || is.na(directory))
    stop("directory must be one path", call. = FALSE)
  if (!is.null(validation) && !is.function(validation))
    stop("validation must be NULL or a function", call. = FALSE)
  if (id %in% names(suite$entries) && !isTRUE(replace))
    stop("canonical validation is already registered: ", id, call. = FALSE)
  suite$entries[[id]] <- list(id = id, directory = directory, validation = validation)
  suite$entries <- suite$entries[sort(names(suite$entries))]
  validate_canonical_validation_suite(suite)
}

#' Validate a canonical validation suite
#' @param suite Canonical validation suite.
#' @return `suite`, invisibly.
#' @export
validate_canonical_validation_suite <- function(suite) {
  if (!inherits(suite, "PopgenVCFCanonicalValidationSuite"))
    stop("suite must be a PopgenVCFCanonicalValidationSuite", call. = FALSE)
  if (!identical(suite$schema_version, "1.0")) stop("unsupported suite schema", call. = FALSE)
  if (!is.list(suite$entries) || (length(suite$entries) && is.null(names(suite$entries))))
    stop("suite entries must be a named list", call. = FALSE)
  if (anyDuplicated(names(suite$entries))) stop("suite identifiers must be unique", call. = FALSE)
  invisible(suite)
}

#' Run a canonical validation suite
#'
#' @param suite Canonical validation suite.
#' @param registry Approved canonical dataset registry.
#' @return A `PopgenVCFCanonicalValidationSuiteResult`.
#' @export
run_canonical_validation_suite <- function(suite, registry) {
  validate_canonical_validation_suite(suite)
  validate_canonical_dataset_registry(registry)
  started <- proc.time()[["elapsed"]]
  results <- list()
  for (id in names(suite$entries)) {
    entry <- suite$entries[[id]]
    item_started <- proc.time()[["elapsed"]]
    outcome <- tryCatch({
      descriptor <- get_canonical_dataset(registry, id, require_approved = TRUE)
      verification <- verify_canonical_dataset(descriptor, entry$directory)
      custom <- if (is.null(entry$validation)) NULL else entry$validation(descriptor, entry$directory)
      if (!is.null(custom)) {
        custom <- as.data.frame(custom, stringsAsFactors = FALSE)
        if (!"passed" %in% names(custom) || !is.logical(custom$passed))
          stop("custom validation must return a logical passed column", call. = FALSE)
      }
      passed <- all(verification$passed) && (is.null(custom) || all(custom$passed))
      list(status = if (passed) "pass" else "fail", verification = verification,
           validation = custom, error = NA_character_)
    }, error = function(e) list(status = "error", verification = NULL,
      validation = NULL, error = conditionMessage(e)))
    outcome$elapsed_seconds <- unname(proc.time()[["elapsed"]] - item_started)
    results[[id]] <- outcome
    if (isTRUE(suite$fail_fast) && !identical(outcome$status, "pass")) break
  }
  structure(list(
    schema_version = "1.0", suite_id = suite$id, title = suite$title,
    fail_fast = suite$fail_fast, results = results,
    elapsed_seconds = unname(proc.time()[["elapsed"]] - started)
  ), class = "PopgenVCFCanonicalValidationSuiteResult")
}

#' Summarize a canonical validation suite result
#' @param result Suite result.
#' @return Deterministically ordered data frame.
#' @export
canonical_validation_suite_table <- function(result) {
  if (!inherits(result, "PopgenVCFCanonicalValidationSuiteResult"))
    stop("result must be a PopgenVCFCanonicalValidationSuiteResult", call. = FALSE)
  ids <- sort(names(result$results))
  rows <- lapply(ids, function(id) {
    x <- result$results[[id]]
    data.frame(dataset_id = id, status = x$status,
      passed = identical(x$status, "pass"), elapsed_seconds = x$elapsed_seconds,
      files = if (is.null(x$verification)) NA_integer_ else nrow(x$verification),
      checks = if (is.null(x$validation)) 0L else nrow(x$validation),
      error = x$error, stringsAsFactors = FALSE)
  })
  if (!length(rows)) return(data.frame(dataset_id = character(), status = character(),
    passed = logical(), elapsed_seconds = numeric(), files = integer(), checks = integer(),
    error = character(), stringsAsFactors = FALSE))
  out <- do.call(rbind, rows); rownames(out) <- NULL; out
}

#' Summarize analysis coverage across suite datasets
#' @param suite Canonical validation suite.
#' @param registry Canonical dataset registry.
#' @return Dataset-by-analysis coverage table.
#' @export
canonical_validation_coverage <- function(suite, registry) {
  validate_canonical_validation_suite(suite)
  validate_canonical_dataset_registry(registry)
  rows <- lapply(sort(names(suite$entries)), function(id) {
    descriptor <- get_canonical_dataset(registry, id, require_approved = TRUE)
    data.frame(dataset_id = id, analysis = descriptor$analyses, stringsAsFactors = FALSE)
  })
  if (!length(rows)) return(data.frame(dataset_id = character(), analysis = character()))
  out <- do.call(rbind, rows); out <- out[order(out$analysis, out$dataset_id), , drop = FALSE]
  rownames(out) <- NULL; out
}

#' Write deterministic canonical validation suite evidence
#' @param result Suite result.
#' @param output_dir Evidence directory.
#' @return Named normalized paths.
#' @export
write_canonical_validation_suite <- function(result, output_dir) {
  table <- canonical_validation_suite_table(result)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  summary_path <- file.path(output_dir, "canonical_validation_suite.tsv")
  methods_path <- file.path(output_dir, "canonical_validation_suite_methods.md")
  data.table::fwrite(table, summary_path, sep = "\t", quote = FALSE, na = "NA")
  writeLines(paste0("Canonical validation suite ", result$suite_id,
    " executed ", nrow(table), " approved dataset validation(s). ",
    sum(table$passed), " passed; fail_fast=", tolower(as.character(result$fail_fast)), "."),
    methods_path, useBytes = TRUE)
  c(summary = normalizePath(summary_path), methods = normalizePath(methods_path))
}
