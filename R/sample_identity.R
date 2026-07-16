identity_character_column <- function(x) {
  out <- trimws(as.character(x))
  out[is.na(out) | !nzchar(out)] <- NA_character_
  out
}

#' Build a canonical sample identity table
#'
#' @param metadata Metadata containing the immutable `sample` column and optional
#'   `alias`, `individual`, `family`, `replicate`, and `display_order` columns.
#' @return A validated `PopgenVCFSampleIdentity` data table.
#' @export
new_sample_identity <- function(metadata) {
  x <- data.table::copy(data.table::as.data.table(metadata))
  if (!"sample" %in% names(x)) stop("metadata must contain a sample column", call. = FALSE)
  x[, sample := identity_character_column(sample)]
  if (anyNA(x$sample)) stop("sample identifiers must be non-empty", call. = FALSE)
  if (anyDuplicated(x$sample)) stop("sample identifiers must be unique", call. = FALSE)

  for (column in c("alias", "individual", "family", "replicate")) {
    if (!column %in% names(x)) x[, (column) := NA_character_]
    x[, (column) := identity_character_column(get(column))]
  }
  if (!"display_order" %in% names(x)) x[, display_order := NA_integer_]
  x[, display_order := suppressWarnings(as.integer(display_order))]
  if (any(!is.na(x$display_order) & x$display_order < 1L)) {
    stop("display_order must contain positive integers", call. = FALSE)
  }
  if (anyDuplicated(x$display_order[!is.na(x$display_order)])) {
    stop("non-missing display_order values must be unique", call. = FALSE)
  }

  duplicate_alias <- unique(x[!is.na(alias) & duplicated(alias), alias])
  if (length(duplicate_alias)) {
    stop("Metadata aliases must be unique: ", paste(duplicate_alias, collapse = ", "), call. = FALSE)
  }
  x[, public_sample := data.table::fifelse(is.na(alias), sample, alias)]
  duplicate_public <- unique(x[duplicated(public_sample), public_sample])
  if (length(duplicate_public)) {
    stop(
      "Aliases and original sample IDs must resolve to globally unique public names: ",
      paste(duplicate_public, collapse = ", "),
      call. = FALSE
    )
  }
  data.table::setattr(x, "class", unique(c("PopgenVCFSampleIdentity", class(x))))
  validate_sample_identity(x)
  x[]
}

#' Validate a canonical sample identity table
#' @param x A sample identity table.
#' @return `x`, invisibly.
#' @export
validate_sample_identity <- function(x) {
  if (!inherits(x, "PopgenVCFSampleIdentity")) stop("x must be a PopgenVCFSampleIdentity", call. = FALSE)
  required <- c("sample", "alias", "individual", "family", "replicate", "display_order", "public_sample")
  if (!all(required %in% names(x))) stop("sample identity table is incomplete", call. = FALSE)
  if (anyDuplicated(x$sample)) stop("sample identifiers must be unique", call. = FALSE)
  if (anyDuplicated(x$public_sample)) stop("public sample identifiers must be unique", call. = FALSE)
  if (anyDuplicated(x$display_order[!is.na(x$display_order)])) {
    stop("non-missing display_order values must be unique", call. = FALSE)
  }
  invisible(x)
}

#' Return a stable sample identity table
#' @param x Metadata or a `PopgenVCFSampleIdentity`.
#' @param ordered Apply explicit display order, followed by public name.
#' @return A data table.
#' @export
sample_identity_table <- function(x, ordered = FALSE) {
  if (!inherits(x, "PopgenVCFSampleIdentity")) x <- new_sample_identity(x)
  validate_sample_identity(x)
  out <- data.table::copy(x)
  if (isTRUE(ordered)) {
    out[, .missing_order := is.na(display_order)]
    data.table::setorder(out, .missing_order, display_order, public_sample, na.last = TRUE)
    out[, .missing_order := NULL]
  }
  out[]
}

#' Resolve public sample names from immutable VCF identifiers
#' @param identity Metadata or sample identity table.
#' @param sample_ids Immutable VCF/GDS sample identifiers.
#' @return Public sample identifiers in requested order.
#' @export
resolve_sample_identity <- function(identity, sample_ids) {
  identity <- sample_identity_table(identity)
  sample_ids <- as.character(sample_ids)
  index <- match(sample_ids, identity$sample)
  if (anyNA(index)) stop("sample IDs are absent from the identity table", call. = FALSE)
  identity$public_sample[index]
}

#' Summarize identity groupings
#' @param identity Metadata or sample identity table.
#' @return Long-form counts for individual, family, and replicate groupings.
#' @export
sample_identity_groups <- function(identity) {
  identity <- sample_identity_table(identity)
  data.table::rbindlist(lapply(c("individual", "family", "replicate"), function(column) {
    values <- identity[[column]]
    values <- values[!is.na(values)]
    if (!length(values)) return(data.table::data.table(
      grouping = character(), id = character(), n_samples = integer()
    ))
    counts <- data.table::as.data.table(table(values), keep.rownames = FALSE)
    data.table::setnames(counts, c("id", "n_samples"))
    counts[, grouping := column]
    data.table::setcolorder(counts, c("grouping", "id", "n_samples"))
    counts[]
  }), fill = TRUE)
}
