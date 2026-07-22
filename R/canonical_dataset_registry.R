#' Create a canonical dataset registry
#'
#' @param entries Optional list of canonical dataset descriptors.
#' @return A validated `PopgenVCFCanonicalDatasetRegistry`.
#' @export
new_canonical_dataset_registry <- function(entries = list()) {
  registry <- structure(list(entries = list()), class = "PopgenVCFCanonicalDatasetRegistry")
  for (entry in entries) registry <- register_canonical_dataset(registry, entry)
  validate_canonical_dataset_registry(registry)
}

#' Register a canonical dataset
#'
#' @param registry Canonical dataset registry.
#' @param descriptor A `PopgenVCFCanonicalDataset`.
#' @param approval One of `candidate`, `approved`, `deprecated`, or `rejected`.
#' @param reviewed_by Reviewer identity for approved or rejected entries.
#' @param reviewed_at ISO-8601 review date.
#' @param notes Optional review notes.
#' @param replace Permit replacement of an existing identifier.
#' @return Updated registry.
#' @export
register_canonical_dataset <- function(registry, descriptor,
                                       approval = "candidate",
                                       reviewed_by = NA_character_,
                                       reviewed_at = NA_character_,
                                       notes = NA_character_,
                                       replace = FALSE) {
  validate_canonical_dataset_registry(registry)
  validate_canonical_dataset(descriptor)
  approval <- match.arg(approval, c("candidate", "approved", "deprecated", "rejected"))
  id <- descriptor$id
  if (id %in% names(registry$entries) && !isTRUE(replace))
    stop("canonical dataset is already registered: ", id, call. = FALSE)
  if (approval %in% c("approved", "rejected")) {
    if (length(reviewed_by) != 1L || is.na(reviewed_by) || !nzchar(trimws(reviewed_by)))
      stop("reviewed_by is required for approved or rejected datasets", call. = FALSE)
    if (length(reviewed_at) != 1L || is.na(reviewed_at) || !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", reviewed_at))
      stop("reviewed_at must be an ISO-8601 date for approved or rejected datasets", call. = FALSE)
  }
  registry$entries[[id]] <- list(
    descriptor = descriptor,
    approval = approval,
    reviewed_by = as.character(reviewed_by)[1L],
    reviewed_at = as.character(reviewed_at)[1L],
    notes = as.character(notes)[1L]
  )
  registry$entries <- registry$entries[sort(names(registry$entries))]
  validate_canonical_dataset_registry(registry)
}

#' Validate a canonical dataset registry
#' @param registry Canonical dataset registry.
#' @return `registry`, invisibly.
#' @export
validate_canonical_dataset_registry <- function(registry) {
  if (!inherits(registry, "PopgenVCFCanonicalDatasetRegistry"))
    stop("registry must be a PopgenVCFCanonicalDatasetRegistry", call. = FALSE)
  if (!is.list(registry$entries) || (length(registry$entries) && is.null(names(registry$entries))))
    stop("registry entries must be a named list", call. = FALSE)
  if (anyDuplicated(names(registry$entries))) stop("registry identifiers must be unique", call. = FALSE)
  for (id in names(registry$entries)) {
    entry <- registry$entries[[id]]
    validate_canonical_dataset(entry$descriptor)
    if (!identical(id, entry$descriptor$id)) stop("registry key does not match descriptor id", call. = FALSE)
    if (!entry$approval %in% c("candidate", "approved", "deprecated", "rejected"))
      stop("invalid canonical dataset approval state", call. = FALSE)
  }
  invisible(registry)
}

#' List registered canonical datasets
#' @param registry Canonical dataset registry.
#' @param approval Optional approval-state filter.
#' @return Deterministically ordered data frame.
#' @export
list_canonical_datasets <- function(registry, approval = NULL) {
  validate_canonical_dataset_registry(registry)
  rows <- lapply(names(registry$entries), function(id) {
    entry <- registry$entries[[id]]
    d <- entry$descriptor
    data.frame(id = id, version = d$version, title = d$title,
      organism = d$organism, license = d$license, approval = entry$approval,
      reviewed_by = entry$reviewed_by, reviewed_at = entry$reviewed_at,
      files = nrow(d$files), analyses = paste(d$analyses, collapse = ","),
      stringsAsFactors = FALSE)
  })
  out <- if (length(rows)) do.call(rbind, rows) else data.frame(
    id = character(), version = character(), title = character(), organism = character(),
    license = character(), approval = character(), reviewed_by = character(),
    reviewed_at = character(), files = integer(), analyses = character(),
    stringsAsFactors = FALSE)
  if (!is.null(approval)) out <- out[out$approval %in% approval, , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Get a registered canonical dataset
#' @param registry Canonical dataset registry.
#' @param id Dataset identifier.
#' @param require_approved Fail unless the entry is approved.
#' @return Registered descriptor.
#' @export
get_canonical_dataset <- function(registry, id, require_approved = FALSE) {
  validate_canonical_dataset_registry(registry)
  id <- tolower(as.character(id)[1L])
  entry <- registry$entries[[id]]
  if (is.null(entry)) stop("unknown canonical dataset: ", id, call. = FALSE)
  if (isTRUE(require_approved) && !identical(entry$approval, "approved"))
    stop("canonical dataset is not approved: ", id, call. = FALSE)
  entry$descriptor
}

#' Materialize an approved registered canonical dataset
#' @param registry Canonical dataset registry.
#' @param id Dataset identifier.
#' @param ... Arguments passed to [materialize_canonical_dataset()].
#' @return Verified destination directory.
#' @export
materialize_registered_canonical_dataset <- function(registry, id, ...) {
  descriptor <- get_canonical_dataset(registry, id, require_approved = TRUE)
  materialize_canonical_dataset(descriptor, ...)
}

#' Write canonical dataset registry evidence
#' @param registry Canonical dataset registry.
#' @param path Output TSV path.
#' @return Normalized output path.
#' @export
write_canonical_dataset_registry <- function(registry, path) {
  table <- list_canonical_datasets(registry)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(table, path, sep = "\t", quote = FALSE, na = "NA")
  normalizePath(path)
}
