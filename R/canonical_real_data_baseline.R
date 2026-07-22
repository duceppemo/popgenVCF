#' Create a production canonical real-data baseline snapshot
#'
#' @param dataset A validated `PopgenVCFCanonicalDataset`.
#' @param registry A validated `PopgenVCFCanonicalBaselineRegistry`.
#' @param sample_metadata Complete sample metadata as a data frame.
#' @param dataset_version Version of the acquired canonical dataset.
#' @param generated_by Non-empty description of the generating workflow.
#' @param generated_at ISO-8601 UTC timestamp.
#' @param source_commit Full 40-character Git commit SHA.
#' @param approval One of `proposed` or `approved`.
#' @param approved_by Reviewer identity; required when approved.
#' @param approved_at ISO-8601 date; required when approved.
#' @param notes Optional review notes.
#' @return A validated `PopgenVCFCanonicalRealDataBaselineSnapshot`.
#' @export
new_canonical_real_data_baseline_snapshot <- function(
    dataset,
    registry,
    sample_metadata,
    dataset_version,
    generated_by,
    generated_at,
    source_commit,
    approval = c("proposed", "approved"),
    approved_by = NULL,
    approved_at = NULL,
    notes = NULL) {
  validate_canonical_dataset(dataset)
  validate_canonical_baseline_registry(registry)
  approval <- match.arg(approval)
  scalar <- function(x, label) {
    if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x)))
      stop(label, " must be one non-empty string", call. = FALSE)
    trimws(x)
  }
  metadata <- as.data.frame(sample_metadata, stringsAsFactors = FALSE)
  required_metadata <- c("sample_id", "population", "superpopulation", "sex")
  if (!nrow(metadata) || !all(required_metadata %in% names(metadata)))
    stop("sample_metadata must contain sample_id, population, superpopulation, and sex", call. = FALSE)
  metadata <- metadata[order(metadata$sample_id), required_metadata, drop = FALSE]
  rownames(metadata) <- NULL
  if (anyNA(metadata) || any(!nzchar(trimws(as.matrix(metadata)))))
    stop("sample_metadata must be complete", call. = FALSE)
  if (anyDuplicated(metadata$sample_id))
    stop("sample_metadata sample_id values must be unique", call. = FALSE)
  metric_dataset_ids <- unique(vapply(registry$metrics, `[[`, character(1), "dataset_id"))
  if (length(metric_dataset_ids) != 1L || !identical(metric_dataset_ids, dataset$id))
    stop("all baseline metrics must target the canonical dataset", call. = FALSE)
  if (!grepl("^[0-9a-f]{40}$", source_commit))
    stop("source_commit must be a full lowercase Git SHA", call. = FALSE)
  if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$", generated_at))
    stop("generated_at must be an ISO-8601 UTC timestamp", call. = FALSE)
  if (approval == "approved") {
    approved_by <- scalar(approved_by, "approved_by")
    approved_at <- scalar(approved_at, "approved_at")
    if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", approved_at))
      stop("approved_at must be an ISO-8601 date", call. = FALSE)
  } else if (!is.null(approved_by) || !is.null(approved_at)) {
    stop("proposed snapshots cannot contain approval metadata", call. = FALSE)
  }
  snapshot <- structure(list(
    schema_version = "1.0",
    dataset_id = dataset$id,
    dataset_version = scalar(dataset_version, "dataset_version"),
    dataset_sha256 = sort(setNames(dataset$files$sha256, dataset$files$filename)),
    sample_count = nrow(metadata),
    sample_metadata = metadata,
    baseline_registry = registry,
    generated_by = scalar(generated_by, "generated_by"),
    generated_at = generated_at,
    source_commit = source_commit,
    approval = approval,
    approved_by = approved_by,
    approved_at = approved_at,
    notes = if (is.null(notes)) NULL else scalar(notes, "notes")
  ), class = "PopgenVCFCanonicalRealDataBaselineSnapshot")
  validate_canonical_real_data_baseline_snapshot(snapshot)
}

#' Validate a canonical real-data baseline snapshot
#'
#' @param snapshot Snapshot object.
#' @param require_approved Fail unless the snapshot is approved.
#' @return `snapshot`, invisibly.
#' @export
validate_canonical_real_data_baseline_snapshot <- function(snapshot, require_approved = FALSE) {
  if (!inherits(snapshot, "PopgenVCFCanonicalRealDataBaselineSnapshot"))
    stop("snapshot must be a PopgenVCFCanonicalRealDataBaselineSnapshot", call. = FALSE)
  required <- c("schema_version", "dataset_id", "dataset_version", "dataset_sha256",
                "sample_count", "sample_metadata", "baseline_registry", "generated_by",
                "generated_at", "source_commit", "approval", "approved_by", "approved_at", "notes")
  if (!all(required %in% names(snapshot)) || !identical(snapshot$schema_version, "1.0"))
    stop("invalid canonical real-data baseline snapshot schema", call. = FALSE)
  validate_canonical_baseline_registry(snapshot$baseline_registry)
  if (!snapshot$approval %in% c("proposed", "approved"))
    stop("invalid real-data baseline approval state", call. = FALSE)
  if (isTRUE(require_approved) && !identical(snapshot$approval, "approved"))
    stop("canonical real-data baseline snapshot is not approved", call. = FALSE)
  if (!identical(snapshot$sample_count, nrow(snapshot$sample_metadata)))
    stop("sample_count does not match sample_metadata", call. = FALSE)
  invisible(snapshot)
}

#' Write a canonical real-data baseline snapshot
#'
#' @param snapshot Snapshot object.
#' @param path Destination JSON path.
#' @param require_approved Fail unless the snapshot is approved.
#' @return Normalized output path.
#' @export
write_canonical_real_data_baseline_snapshot <- function(snapshot, path, require_approved = FALSE) {
  validate_canonical_real_data_baseline_snapshot(snapshot, require_approved = require_approved)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  payload <- unclass(snapshot)
  payload$baseline_registry <- list(
    schema_version = snapshot$baseline_registry$schema_version,
    metrics = unname(lapply(snapshot$baseline_registry$metrics, unclass))
  )
  jsonlite::write_json(payload, path, auto_unbox = TRUE, pretty = TRUE, na = "null", digits = 17)
  normalizePath(path)
}
