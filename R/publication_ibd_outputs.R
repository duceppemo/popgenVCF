# Phase 0.9.11 - deterministic publication isolation-by-distance outputs

.publication_ibd_fingerprint <- function(x) {
  candidate <- unclass(x)
  candidate$fingerprint <- NULL
  digest::digest(candidate, algo = "sha256", serialize = TRUE)
}

#' Create a publication isolation-by-distance specification
#'
#' @param sample1_column First sample or population identity column.
#' @param sample2_column Second sample or population identity column.
#' @param genetic_distance_column Genetic-distance column.
#' @param geographic_distance_column Geographic-distance column.
#' @param source_data_format Machine-readable source-data format.
#' @param version Specification version.
#' @return A fingerprinted publication IBD specification.
#' @export
new_publication_ibd_spec <- function(
    sample1_column = "sample1", sample2_column = "sample2",
    genetic_distance_column = "genetic_distance",
    geographic_distance_column = "geographic_distance",
    source_data_format = "tsv", version = "1.0.0") {
  values <- c(sample1_column, sample2_column, genetic_distance_column, geographic_distance_column)
  if (anyNA(values) || any(!nzchar(values)) || anyDuplicated(values)) {
    stop("IBD specification columns must be unique non-empty names.", call. = FALSE)
  }
  source_data_format <- match.arg(source_data_format, c("tsv", "csv"))
  spec <- list(
    record_type = "popgenvcf_publication_ibd_spec", schema_version = "1.0.0",
    sample1_column = sample1_column, sample2_column = sample2_column,
    genetic_distance_column = genetic_distance_column,
    geographic_distance_column = geographic_distance_column,
    source_data_format = source_data_format, version = version
  )
  spec$fingerprint <- .publication_ibd_fingerprint(spec)
  class(spec) <- c("PopgenVCFPublicationIBDSpec", "list")
  validate_publication_ibd_spec(spec)
  spec
}

#' Validate a publication IBD specification
#' @param spec An IBD publication specification.
#' @return `TRUE`, invisibly.
#' @export
validate_publication_ibd_spec <- function(spec) {
  if (!inherits(spec, "PopgenVCFPublicationIBDSpec") ||
      !identical(spec$schema_version, "1.0.0") ||
      !identical(spec$fingerprint, .publication_ibd_fingerprint(spec))) {
    stop("Invalid publication IBD specification.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Create a deterministic publication IBD output
#'
#' @param spec A validated IBD publication specification.
#' @param pairs Authoritative pairwise distance table.
#' @param regression Authoritative regression summary table.
#' @param permutation Optional authoritative Mantel or permutation evidence.
#' @param result_fingerprint Fingerprint of the authoritative IBD result.
#' @param figure_binding Optional publication figure-style binding.
#' @return A fingerprinted publication IBD output.
#' @export
new_publication_ibd_output <- function(
    spec, pairs, regression, permutation = NULL,
    result_fingerprint, figure_binding = NULL) {
  validate_publication_ibd_spec(spec)
  if (!is.data.frame(pairs) || !is.data.frame(regression)) {
    stop("pairs and regression must be data frames.", call. = FALSE)
  }
  required <- c(spec$sample1_column, spec$sample2_column,
                spec$genetic_distance_column, spec$geographic_distance_column)
  if (!all(required %in% names(pairs))) stop("pairs is missing required IBD columns.", call. = FALSE)
  ids1 <- as.character(pairs[[spec$sample1_column]])
  ids2 <- as.character(pairs[[spec$sample2_column]])
  if (anyNA(ids1) || anyNA(ids2) || any(!nzchar(ids1)) || any(!nzchar(ids2)) || any(ids1 == ids2)) {
    stop("IBD pair identities must be non-empty and distinct.", call. = FALSE)
  }
  canonical <- Map(function(a, b) sort(c(a, b)), ids1, ids2)
  pairs[[spec$sample1_column]] <- vapply(canonical, `[[`, character(1), 1L)
  pairs[[spec$sample2_column]] <- vapply(canonical, `[[`, character(1), 2L)
  for (column in c(spec$genetic_distance_column, spec$geographic_distance_column)) {
    pairs[[column]] <- as.numeric(pairs[[column]])
    if (any(!is.finite(pairs[[column]])) || any(pairs[[column]] < 0)) {
      stop("IBD distances must be finite and non-negative.", call. = FALSE)
    }
  }
  key <- paste(pairs[[spec$sample1_column]], pairs[[spec$sample2_column]], sep = "\r")
  if (anyDuplicated(key)) stop("IBD pairs must be unique.", call. = FALSE)
  pairs <- pairs[order(pairs[[spec$sample1_column]], pairs[[spec$sample2_column]]), , drop = FALSE]
  regression <- regression[do.call(order, regression), , drop = FALSE]
  if (!is.null(permutation)) {
    if (!is.data.frame(permutation)) stop("permutation must be a data frame or NULL.", call. = FALSE)
    permutation <- permutation[do.call(order, permutation), , drop = FALSE]
  }
  output <- list(
    record_type = "popgenvcf_publication_ibd_output", schema_version = "1.0.0",
    specification_fingerprint = spec$fingerprint,
    result_fingerprint = result_fingerprint,
    figure_binding_fingerprint = if (is.null(figure_binding)) NULL else figure_binding$fingerprint,
    pairs = pairs, regression = regression, permutation = permutation,
    source_data = list(pairs = pairs, regression = regression, permutation = permutation)
  )
  output$fingerprint <- .publication_ibd_fingerprint(output)
  class(output) <- c("PopgenVCFPublicationIBDOutput", "list")
  validate_publication_ibd_output(output, spec)
  output
}

#' Validate a publication IBD output
#' @param output An IBD publication output.
#' @param spec Originating specification.
#' @return `TRUE`, invisibly.
#' @export
validate_publication_ibd_output <- function(output, spec) {
  validate_publication_ibd_spec(spec)
  if (!inherits(output, "PopgenVCFPublicationIBDOutput") ||
      !identical(output$specification_fingerprint, spec$fingerprint)) {
    stop("output is not bound to the supplied IBD specification.", call. = FALSE)
  }
  if (!identical(output$source_data$pairs, output$pairs) ||
      !identical(output$source_data$regression, output$regression) ||
      !identical(output$source_data$permutation, output$permutation)) {
    stop("IBD source data drifted from the publication output.", call. = FALSE)
  }
  if (!identical(output$fingerprint, .publication_ibd_fingerprint(output))) {
    stop("IBD output fingerprint mismatch.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Create a deterministic publication IBD caption
#' @param output A validated IBD output.
#' @param spec Originating specification.
#' @return One manuscript-ready caption string.
#' @export
publication_ibd_caption <- function(output, spec) {
  validate_publication_ibd_output(output, spec)
  sprintf("Isolation-by-distance relationship across %d unique pairwise comparisons.", nrow(output$pairs))
}

#' Render a deterministic publication IBD report
#' @param output An IBD publication output.
#' @param spec Originating specification.
#' @return Markdown report lines.
#' @export
publication_ibd_report <- function(output, spec) {
  validate_publication_ibd_output(output, spec)
  c(
    "# Publication isolation-by-distance output", "",
    sprintf("- Pairwise comparisons: `%d`", nrow(output$pairs)),
    sprintf("- Regression rows: `%d`", nrow(output$regression)),
    sprintf("- Permutation evidence: `%s`", if (is.null(output$permutation)) "absent" else "present"),
    sprintf("- Result fingerprint: `%s`", output$result_fingerprint),
    sprintf("- Output fingerprint: `%s`", output$fingerprint)
  )
}
