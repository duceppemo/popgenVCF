golden_scalar_string <- function(x, label) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop(label, " must be one non-empty string", call. = FALSE)
  }
  x
}

golden_named_list <- function(x, label) {
  if (!is.list(x) || (length(x) && (is.null(names(x)) || any(!nzchar(names(x)))))) {
    stop(label, " must be a named list", call. = FALSE)
  }
  x
}

#' Create a golden-output specification
#'
#' @param id Stable output identifier.
#' @param mode Comparison mode: exact, numeric, matrix, eigenspace, q_matrix, or manifest.
#' @param absolute_tolerance,relative_tolerance Nonnegative numerical tolerances.
#' @param role Either gating or diagnostic.
#' @param metadata Additional named metadata.
#' @return A validated `PopgenVCFGoldenSpec`.
#' @export
new_golden_spec <- function(id, mode = "exact", absolute_tolerance = 0,
                            relative_tolerance = 0, role = "gating",
                            metadata = list()) {
  id <- golden_scalar_string(id, "id")
  if (!grepl("^[A-Za-z0-9_.-]+$", id)) {
    stop("id may contain only letters, numbers, dot, underscore, and hyphen", call. = FALSE)
  }
  mode <- match.arg(mode, c("exact", "numeric", "matrix", "eigenspace", "q_matrix", "manifest"))
  role <- match.arg(role, c("gating", "diagnostic"))
  tolerance <- c(absolute = as.numeric(absolute_tolerance), relative = as.numeric(relative_tolerance))
  if (length(tolerance) != 2L || anyNA(tolerance) || any(!is.finite(tolerance)) || any(tolerance < 0)) {
    stop("tolerances must be nonnegative finite scalar values", call. = FALSE)
  }
  metadata <- golden_named_list(metadata, "metadata")
  structure(list(schema_version = "1.0", id = id, mode = mode,
                 tolerance = tolerance, role = role, metadata = metadata),
            class = "PopgenVCFGoldenSpec")
}

#' Create a golden-output entry
#' @param spec A golden specification.
#' @param value Canonical output object.
#' @param approved_by,approval_reason Optional intentional-change approval metadata.
#' @param created_at UTC timestamp.
#' @return A `PopgenVCFGoldenEntry`.
#' @export
new_golden_entry <- function(spec, value, approved_by = NA_character_,
                             approval_reason = NA_character_,
                             created_at = format(Sys.time(), tz = "UTC", usetz = TRUE)) {
  if (!inherits(spec, "PopgenVCFGoldenSpec")) stop("spec is invalid", call. = FALSE)
  structure(list(
    schema_version = "1.0", spec = spec, value = value,
    value_digest = digest::digest(value, algo = "sha256", serialize = TRUE),
    approved_by = as.character(approved_by)[1L],
    approval_reason = as.character(approval_reason)[1L],
    created_at = as.character(created_at)[1L]
  ), class = "PopgenVCFGoldenEntry")
}

#' Create and update a golden-output store
#' @param entries Optional list of golden entries.
#' @param metadata Optional store metadata.
#' @return A `PopgenVCFGoldenStore`.
#' @export
new_golden_store <- function(entries = list(), metadata = list()) {
  metadata <- golden_named_list(metadata, "metadata")
  x <- structure(list(schema_version = "1.0", entries = list(), metadata = metadata),
                 class = "PopgenVCFGoldenStore")
  for (entry in entries) x <- register_golden_entry(x, entry)
  x
}

#' @rdname new_golden_store
#' @param store A golden store.
#' @param entry A golden entry.
#' @param replace Permit an approved replacement.
#' @export
register_golden_entry <- function(store, entry, replace = FALSE) {
  if (!inherits(store, "PopgenVCFGoldenStore")) stop("store is invalid", call. = FALSE)
  validate_golden_entry(entry)
  id <- entry$spec$id
  exists <- id %in% names(store$entries)
  if (exists && !isTRUE(replace)) stop("golden entry already exists: ", id, call. = FALSE)
  if (exists && (is.na(entry$approved_by) || !nzchar(entry$approved_by) ||
                 is.na(entry$approval_reason) || !nzchar(entry$approval_reason))) {
    stop("replacing a golden entry requires approved_by and approval_reason", call. = FALSE)
  }
  store$entries[[id]] <- entry
  store
}

#' Validate a golden entry
#' @param x A golden entry.
#' @return `x`, invisibly.
#' @export
validate_golden_entry <- function(x) {
  if (!inherits(x, "PopgenVCFGoldenEntry")) stop("x must be a PopgenVCFGoldenEntry", call. = FALSE)
  if (!identical(x$schema_version, "1.0")) stop("unsupported golden entry schema", call. = FALSE)
  if (!inherits(x$spec, "PopgenVCFGoldenSpec") || !identical(x$spec$schema_version, "1.0")) {
    stop("golden entry spec is invalid", call. = FALSE)
  }
  expected <- digest::digest(x$value, algo = "sha256", serialize = TRUE)
  if (!identical(expected, x$value_digest)) stop("golden entry digest mismatch", call. = FALSE)
  invisible(x)
}

golden_numeric_metrics <- function(observed, expected, abs_tol, rel_tol) {
  observed <- as.numeric(observed)
  expected <- as.numeric(expected)
  if (length(observed) != length(expected)) {
    return(list(passed = FALSE, max_absolute_difference = Inf,
                max_relative_difference = Inf, message = "length mismatch"))
  }
  if (!length(observed)) {
    return(list(passed = TRUE, max_absolute_difference = 0,
                max_relative_difference = 0, message = "empty numeric outputs"))
  }
  same_missing <- identical(is.na(observed), is.na(expected))
  finite <- !is.na(observed) & !is.na(expected)
  if (!same_missing || any(!is.finite(observed[finite])) || any(!is.finite(expected[finite]))) {
    return(list(passed = FALSE, max_absolute_difference = Inf,
                max_relative_difference = Inf, message = "missingness or finiteness mismatch"))
  }
  if (!any(finite)) {
    return(list(passed = TRUE, max_absolute_difference = 0,
                max_relative_difference = 0, message = "matching missing outputs"))
  }
  delta <- abs(observed[finite] - expected[finite])
  scale <- pmax(abs(expected[finite]), .Machine$double.eps)
  relative <- delta / scale
  passed <- all(delta <= abs_tol | relative <= rel_tol)
  list(passed = passed, max_absolute_difference = max(delta),
       max_relative_difference = max(relative), message = "numeric comparison")
}

golden_compare_value <- function(spec, observed, expected) {
  mode <- spec$mode
  abs_tol <- spec$tolerance[["absolute"]]
  rel_tol <- spec$tolerance[["relative"]]
  if (mode %in% c("exact", "manifest")) {
    same <- identical(observed, expected)
    return(list(passed = same, max_absolute_difference = if (same) 0 else Inf,
                max_relative_difference = if (same) 0 else Inf,
                message = if (same) "identical" else "objects differ"))
  }
  if (mode %in% c("numeric", "matrix")) {
    if (mode == "matrix" && (!identical(dim(observed), dim(expected)) ||
                              !identical(dimnames(observed), dimnames(expected)))) {
      return(list(passed = FALSE, max_absolute_difference = Inf,
                  max_relative_difference = Inf, message = "matrix structure mismatch"))
    }
    return(golden_numeric_metrics(observed, expected, abs_tol, rel_tol))
  }
  if (mode == "eigenspace") {
    comparison <- compare_pca_subspaces(observed, expected)
    metric <- comparison$minimum
    return(list(passed = is.finite(metric) && (1 - metric) <= abs_tol,
                max_absolute_difference = 1 - metric, max_relative_difference = NA_real_,
                message = paste(comparison$canonical_correlations, collapse = ",")))
  }
  if (mode == "q_matrix") {
    comparison <- compare_q_matrices(observed, expected)
    metric <- comparison$maximum_absolute_difference
    return(list(passed = is.finite(metric) && metric <= abs_tol,
                max_absolute_difference = metric, max_relative_difference = NA_real_,
                message = paste(comparison$permutation, collapse = ",")))
  }
  stop("unsupported golden comparison mode", call. = FALSE)
}

#' Compare observed outputs with a golden store
#' @param observed Named list of observed canonical outputs.
#' @param store A golden store.
#' @param ids Optional subset of identifiers.
#' @return A `PopgenVCFGoldenResult`.
#' @export
compare_golden_outputs <- function(observed, store, ids = names(store$entries)) {
  observed <- golden_named_list(observed, "observed")
  if (!inherits(store, "PopgenVCFGoldenStore")) stop("store is invalid", call. = FALSE)
  ids <- as.character(ids)
  rows <- lapply(ids, function(id) {
    entry <- store$entries[[id]]
    if (is.null(entry)) return(data.table::data.table(
      id = id, mode = NA_character_, role = NA_character_, status = "skipped", passed = NA,
      max_absolute_difference = NA_real_, max_relative_difference = NA_real_,
      message = "golden entry not found"))
    validate_golden_entry(entry)
    if (is.null(observed[[id]])) return(data.table::data.table(
      id = id, mode = entry$spec$mode, role = entry$spec$role, status = "skipped", passed = NA,
      max_absolute_difference = NA_real_, max_relative_difference = NA_real_,
      message = "observed output not supplied"))
    result <- tryCatch(golden_compare_value(entry$spec, observed[[id]], entry$value), error = identity)
    if (inherits(result, "error")) return(data.table::data.table(
      id = id, mode = entry$spec$mode, role = entry$spec$role, status = "error", passed = FALSE,
      max_absolute_difference = Inf, max_relative_difference = Inf,
      message = conditionMessage(result)))
    data.table::data.table(
      id = id, mode = entry$spec$mode, role = entry$spec$role,
      status = if (result$passed) "passed" else "failed", passed = result$passed,
      max_absolute_difference = result$max_absolute_difference,
      max_relative_difference = result$max_relative_difference, message = result$message)
  })
  comparisons <- data.table::rbindlist(rows, fill = TRUE)
  gating_failed <- comparisons$role == "gating" & comparisons$status %in% c("failed", "error")
  status <- if (any(gating_failed, na.rm = TRUE)) "failed" else "passed"
  structure(list(schema_version = "1.0", status = status, comparisons = comparisons,
                 store_digest = digest::digest(store, algo = "sha256", serialize = TRUE)),
            class = "PopgenVCFGoldenResult")
}

#' Convert golden objects to stable tables
#' @param x A golden store or result.
#' @return A data table.
#' @export
golden_output_table <- function(x) {
  if (inherits(x, "PopgenVCFGoldenResult")) return(data.table::copy(x$comparisons))
  if (inherits(x, "PopgenVCFGoldenStore")) {
    return(data.table::rbindlist(lapply(x$entries, function(entry) data.table::data.table(
      id = entry$spec$id, mode = entry$spec$mode, role = entry$spec$role,
      absolute_tolerance = entry$spec$tolerance[["absolute"]],
      relative_tolerance = entry$spec$tolerance[["relative"]],
      value_digest = entry$value_digest, approved_by = entry$approved_by,
      approval_reason = entry$approval_reason, created_at = entry$created_at
    )), fill = TRUE))
  }
  stop("x must be a golden store or result", call. = FALSE)
}

#' Write, read, and verify a golden-output store
#' @param store A golden store.
#' @param path Destination directory.
#' @param overwrite Permit replacement of an existing directory.
#' @return Normalized path for writing, or a validated store for reading.
#' @export
write_golden_store <- function(store, path, overwrite = FALSE) {
  if (!inherits(store, "PopgenVCFGoldenStore")) stop("store is invalid", call. = FALSE)
  if (dir.exists(path) && !isTRUE(overwrite)) stop("golden store directory already exists", call. = FALSE)
  if (dir.exists(path)) unlink(path, recursive = TRUE, force = TRUE)
  dir.create(file.path(path, "entries"), recursive = TRUE)
  root <- normalizePath(path)
  files <- character()
  for (entry in store$entries) {
    validate_golden_entry(entry)
    file <- file.path(root, "entries", paste0(entry$spec$id, ".rds"))
    saveRDS(entry, file, version = 3)
    files <- c(files, file)
  }
  saveRDS(store, file.path(root, "store.rds"), version = 3)
  data.table::fwrite(golden_output_table(store), file.path(root, "entries.tsv"), sep = "\t")
  jsonlite::write_json(store$metadata, file.path(root, "metadata.json"), auto_unbox = TRUE,
                       pretty = TRUE, null = "null", na = "null")
  files <- c(files, file.path(root, "store.rds"), file.path(root, "entries.tsv"),
             file.path(root, "metadata.json"))
  normalized_files <- normalizePath(files)
  manifest <- data.table::data.table(
    path = substring(normalized_files, nchar(root) + 2L),
    size_bytes = file.info(normalized_files)$size,
    sha256 = vapply(normalized_files, digest::digest, character(1L), algo = "sha256", file = TRUE)
  )
  data.table::fwrite(manifest, file.path(root, "manifest.tsv"), sep = "\t")
  invisible(root)
}

#' @rdname write_golden_store
#' @param verify Verify checksums while reading.
#' @export
read_golden_store <- function(path, verify = TRUE) {
  if (isTRUE(verify)) verify_golden_store(path)
  store <- readRDS(file.path(path, "store.rds"))
  if (!inherits(store, "PopgenVCFGoldenStore")) stop("invalid golden store object", call. = FALSE)
  for (entry in store$entries) validate_golden_entry(entry)
  store
}

#' @rdname write_golden_store
#' @export
verify_golden_store <- function(path) {
  manifest_path <- file.path(path, "manifest.tsv")
  if (!file.exists(manifest_path)) stop("golden store manifest is missing", call. = FALSE)
  manifest <- data.table::fread(manifest_path)
  for (i in seq_len(nrow(manifest))) {
    file <- file.path(path, manifest$path[[i]])
    if (!file.exists(file)) stop("golden store file is missing: ", manifest$path[[i]], call. = FALSE)
    actual <- digest::digest(file, algo = "sha256", file = TRUE)
    if (!identical(actual, manifest$sha256[[i]])) {
      stop("golden store checksum mismatch: ", manifest$path[[i]], call. = FALSE)
    }
  }
  invisible(TRUE)
}