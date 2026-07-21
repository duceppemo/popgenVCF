.canonical_validation_fingerprint <- function(x) {
  candidate <- unclass(x)
  candidate$fingerprint <- NULL
  digest::digest(candidate, algo = "sha256", serialize = TRUE)
}

#' Create a canonical validation dataset specification
#'
#' @param dataset_id Stable dataset identifier.
#' @param version Dataset version.
#' @param source_uri Authoritative source URI.
#' @param license_id SPDX or stable license identifier.
#' @param checksum_sha256 SHA-256 checksum of the canonical dataset artifact.
#' @param samples Deterministic sample inventory.
#' @param populations Deterministic population inventory.
#' @param loci Deterministic locus inventory.
#' @param expected_results Expected-result table with tolerances.
#' @param external_comparisons Optional external-tool comparison table.
#' @return A fingerprinted canonical validation dataset specification.
#' @export
new_canonical_validation_dataset <- function(
    dataset_id, version, source_uri, license_id, checksum_sha256,
    samples, populations, loci, expected_results,
    external_comparisons = NULL) {
  strings <- c(dataset_id, version, source_uri, license_id, checksum_sha256)
  if (anyNA(strings) || any(!nzchar(strings))) {
    stop("Canonical dataset metadata must be non-empty.", call. = FALSE)
  }
  if (!grepl("^[0-9a-f]{64}$", checksum_sha256)) {
    stop("checksum_sha256 must be a lowercase SHA-256 digest.", call. = FALSE)
  }
  tables <- list(samples = samples, populations = populations, loci = loci,
                 expected_results = expected_results)
  if (any(!vapply(tables, is.data.frame, logical(1)))) {
    stop("Canonical dataset inventories must be data frames.", call. = FALSE)
  }
  normalize <- function(x) {
    if (!nrow(x)) return(x)
    x[do.call(order, x), , drop = FALSE]
  }
  tables <- lapply(tables, normalize)
  if (!is.null(external_comparisons)) {
    if (!is.data.frame(external_comparisons)) {
      stop("external_comparisons must be a data frame or NULL.", call. = FALSE)
    }
    external_comparisons <- normalize(external_comparisons)
  }
  spec <- c(list(
    record_type = "popgenvcf_canonical_validation_dataset",
    schema_version = "1.0.0", dataset_id = dataset_id, version = version,
    source_uri = source_uri, license_id = license_id,
    checksum_sha256 = checksum_sha256
  ), tables, list(external_comparisons = external_comparisons))
  spec$fingerprint <- .canonical_validation_fingerprint(spec)
  class(spec) <- c("PopgenVCFCanonicalValidationDataset", "list")
  validate_canonical_validation_dataset(spec)
  spec
}

#' Validate a canonical validation dataset specification
#' @param spec Canonical validation dataset specification.
#' @return `TRUE`, invisibly.
#' @export
validate_canonical_validation_dataset <- function(spec) {
  if (!inherits(spec, "PopgenVCFCanonicalValidationDataset") ||
      !identical(spec$schema_version, "1.0.0") ||
      !identical(spec$fingerprint, .canonical_validation_fingerprint(spec))) {
    stop("Invalid canonical validation dataset specification.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Render a canonical validation dataset report
#' @param spec Canonical validation dataset specification.
#' @return Markdown report lines.
#' @export
canonical_validation_dataset_report <- function(spec) {
  validate_canonical_validation_dataset(spec)
  c(
    "# Canonical validation dataset", "",
    sprintf("- Dataset: `%s`", spec$dataset_id),
    sprintf("- Version: `%s`", spec$version),
    sprintf("- Samples: `%d`", nrow(spec$samples)),
    sprintf("- Populations: `%d`", nrow(spec$populations)),
    sprintf("- Loci: `%d`", nrow(spec$loci)),
    sprintf("- Expected results: `%d`", nrow(spec$expected_results)),
    sprintf("- Fingerprint: `%s`", spec$fingerprint)
  )
}