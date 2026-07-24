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
  dataset_sha256 <- setNames(dataset$files$sha256, dataset$files$filename)
  dataset_sha256 <- dataset_sha256[order(names(dataset_sha256))]
  snapshot <- structure(list(
    schema_version = "1.0",
    dataset_id = dataset$id,
    dataset_version = scalar(dataset_version, "dataset_version"),
    dataset_sha256 = dataset_sha256,
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
  scalar <- function(x, label) {
    if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x)))
      stop(label, " must be one non-empty string", call. = FALSE)
    trimws(x)
  }
  dataset_id <- scalar(snapshot$dataset_id, "dataset_id")
  scalar(snapshot$dataset_version, "dataset_version")
  checksums <- snapshot$dataset_sha256
  if (!is.character(checksums) || !length(checksums) || is.null(names(checksums)) ||
      any(!nzchar(names(checksums))) || anyDuplicated(names(checksums)) ||
      any(basename(names(checksums)) != names(checksums)) ||
      any(names(checksums) %in% c(".", "..")) ||
      any(!grepl("^[0-9a-f]{64}$", checksums))) {
    stop("dataset_sha256 must be a named SHA-256 inventory", call. = FALSE)
  }
  if (!identical(names(checksums), sort(names(checksums)))) {
    stop("dataset_sha256 must be ordered by filename", call. = FALSE)
  }
  metadata <- snapshot$sample_metadata
  metadata_columns <- c("sample_id", "population", "superpopulation", "sex")
  if (!is.data.frame(metadata) || !identical(names(metadata), metadata_columns) ||
      !nrow(metadata) || anyNA(metadata) ||
      any(!nzchar(trimws(as.matrix(metadata)))) ||
      anyDuplicated(metadata$sample_id)) {
    stop("sample_metadata must be complete with unique sample identifiers", call. = FALSE)
  }
  if (!identical(metadata$sample_id, sort(metadata$sample_id))) {
    stop("sample_metadata must be ordered by sample_id", call. = FALSE)
  }
  if (!is.integer(snapshot$sample_count) || length(snapshot$sample_count) != 1L ||
      is.na(snapshot$sample_count) || snapshot$sample_count < 1L ||
      !identical(snapshot$sample_count, nrow(metadata))) {
    stop("sample_count does not match sample_metadata", call. = FALSE)
  }
  validate_canonical_baseline_registry(snapshot$baseline_registry)
  metric_dataset_ids <- unique(vapply(
    snapshot$baseline_registry$metrics, `[[`, character(1), "dataset_id"
  ))
  if (!length(metric_dataset_ids) || length(metric_dataset_ids) != 1L ||
      !identical(metric_dataset_ids, dataset_id)) {
    stop("all baseline metrics must target the snapshot dataset", call. = FALSE)
  }
  scalar(snapshot$generated_by, "generated_by")
  if (!is.character(snapshot$generated_at) || length(snapshot$generated_at) != 1L ||
      is.na(snapshot$generated_at) ||
      !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$",
             snapshot$generated_at)) {
    stop("generated_at must be an ISO-8601 UTC timestamp", call. = FALSE)
  }
  if (!is.character(snapshot$source_commit) || length(snapshot$source_commit) != 1L ||
      is.na(snapshot$source_commit) ||
      !grepl("^[0-9a-f]{40}$", snapshot$source_commit)) {
    stop("source_commit must be a full lowercase Git SHA", call. = FALSE)
  }
  if (!is.character(snapshot$approval) || length(snapshot$approval) != 1L ||
      is.na(snapshot$approval) ||
      !snapshot$approval %in% c("proposed", "approved"))
    stop("invalid real-data baseline approval state", call. = FALSE)
  if (identical(snapshot$approval, "approved")) {
    scalar(snapshot$approved_by, "approved_by")
    if (!is.character(snapshot$approved_at) || length(snapshot$approved_at) != 1L ||
        is.na(snapshot$approved_at) ||
        !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", snapshot$approved_at)) {
      stop("approved_at must be an ISO-8601 date", call. = FALSE)
    }
  } else if (!is.null(snapshot$approved_by) || !is.null(snapshot$approved_at)) {
    stop("proposed snapshots cannot contain approval metadata", call. = FALSE)
  }
  if (!is.null(snapshot$notes)) scalar(snapshot$notes, "notes")
  if (isTRUE(require_approved) && !identical(snapshot$approval, "approved"))
    stop("canonical real-data baseline snapshot is not approved", call. = FALSE)
  invisible(snapshot)
}

#' Read a canonical real-data baseline snapshot
#'
#' @param path Source JSON path.
#' @param require_approved Fail unless the snapshot has approved status.
#' @return A validated `PopgenVCFCanonicalRealDataBaselineSnapshot`.
#' @export
read_canonical_real_data_baseline_snapshot <- function(path, require_approved = FALSE) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !file.exists(path) ||
      dir.exists(path)) {
    stop("path must identify one existing snapshot JSON file", call. = FALSE)
  }
  payload <- jsonlite::read_json(path, simplifyVector = FALSE)
  required <- c("schema_version", "dataset_id", "dataset_version", "dataset_sha256",
                "sample_count", "sample_metadata", "baseline_registry", "generated_by",
                "generated_at", "source_commit", "approval", "approved_by", "approved_at", "notes")
  if (!is.list(payload) || !identical(sort(names(payload)), sort(required))) {
    stop("invalid canonical real-data baseline snapshot JSON", call. = FALSE)
  }
  checksum_values <- payload$dataset_sha256
  if (!is.list(checksum_values) || !length(checksum_values) ||
      is.null(names(checksum_values))) {
    stop("snapshot JSON dataset_sha256 must preserve filename keys", call. = FALSE)
  }
  checksums <- vapply(checksum_values, function(x) {
    if (!is.character(x) || length(x) != 1L || is.na(x)) {
      stop("snapshot JSON contains an invalid SHA-256 value", call. = FALSE)
    }
    x
  }, character(1))
  metadata_rows <- payload$sample_metadata
  if (!is.list(metadata_rows) || !length(metadata_rows)) {
    stop("snapshot JSON sample_metadata must contain rows", call. = FALSE)
  }
  metadata <- do.call(rbind, lapply(metadata_rows, function(row) {
    as.data.frame(row, stringsAsFactors = FALSE, optional = TRUE)
  }))
  rownames(metadata) <- NULL
  registry_fields <- c("schema_version", "metrics")
  registry_payload <- payload$baseline_registry
  if (!is.list(registry_payload) ||
      !identical(sort(names(registry_payload)), sort(registry_fields)) ||
      !identical(registry_payload$schema_version, "1.0")) {
    stop("snapshot JSON contains an invalid baseline_registry schema", call. = FALSE)
  }
  metric_rows <- registry_payload$metrics
  metric_fields <- c(
    "schema_version", "id", "dataset_id", "analysis", "expected",
    "comparator", "tolerance", "version", "rationale", "provenance"
  )
  if (!is.list(metric_rows) || !length(metric_rows) ||
      any(!vapply(metric_rows, function(metric) {
        is.list(metric) && identical(sort(names(metric)), sort(metric_fields)) &&
          identical(metric$schema_version, "1.0")
      }, logical(1)))) {
    stop("snapshot JSON baseline_registry must contain metrics", call. = FALSE)
  }
  metric_ids <- vapply(metric_rows, `[[`, character(1), "id")
  if (anyDuplicated(metric_ids) || !identical(metric_ids, sort(metric_ids))) {
    stop("snapshot JSON baseline metrics must be uniquely ordered", call. = FALSE)
  }
  metrics <- lapply(metric_rows, function(metric) {
    expected <- unlist(metric$expected, recursive = TRUE, use.names = FALSE)
    new_canonical_baseline_metric(
      id = metric$id, dataset_id = metric$dataset_id, analysis = metric$analysis,
      expected = expected, comparator = metric$comparator,
      tolerance = metric$tolerance, version = metric$version,
      rationale = metric$rationale, provenance = metric$provenance
    )
  })
  snapshot <- structure(list(
    schema_version = payload$schema_version,
    dataset_id = payload$dataset_id,
    dataset_version = payload$dataset_version,
    dataset_sha256 = checksums,
    sample_count = as.integer(payload$sample_count),
    sample_metadata = metadata,
    baseline_registry = new_canonical_baseline_registry(metrics),
    generated_by = payload$generated_by,
    generated_at = payload$generated_at,
    source_commit = payload$source_commit,
    approval = payload$approval,
    approved_by = payload$approved_by,
    approved_at = payload$approved_at,
    notes = payload$notes
  ), class = "PopgenVCFCanonicalRealDataBaselineSnapshot")
  validate_canonical_real_data_baseline_snapshot(
    snapshot, require_approved = require_approved
  )
  snapshot
}

#' Approve a proposed canonical real-data baseline snapshot
#'
#' @param snapshot A validated proposed snapshot.
#' @param approved_by Non-empty scientific reviewer identity.
#' @param approved_at ISO-8601 review date.
#' @param notes Optional approval notes; defaults to the proposal notes.
#' @return A validated approved snapshot.
#' @export
approve_canonical_real_data_baseline_snapshot <- function(
    snapshot, approved_by, approved_at, notes = snapshot$notes) {
  validate_canonical_real_data_baseline_snapshot(snapshot)
  if (!identical(snapshot$approval, "proposed")) {
    stop("only proposed snapshots can be approved", call. = FALSE)
  }
  snapshot$approval <- "approved"
  snapshot$approved_by <- approved_by
  snapshot$approved_at <- approved_at
  snapshot$notes <- notes
  validate_canonical_real_data_baseline_snapshot(snapshot, require_approved = TRUE)
  snapshot
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
  payload$dataset_sha256 <- as.list(snapshot$dataset_sha256)
  payload$baseline_registry <- list(
    schema_version = snapshot$baseline_registry$schema_version,
    metrics = unname(lapply(snapshot$baseline_registry$metrics, unclass))
  )
  jsonlite::write_json(
    payload, path, auto_unbox = TRUE, pretty = TRUE, na = "null",
    null = "null", digits = 17
  )
  normalizePath(path)
}
