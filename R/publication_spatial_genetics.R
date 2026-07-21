.publication_spatial_fingerprint <- function(x) {
  candidate <- unclass(x)
  candidate$fingerprint <- NULL
  digest::digest(candidate, algo = "sha256", serialize = TRUE)
}

#' Create a publication spatial-genetics specification
#' @param id_column Sample identity column.
#' @param longitude_column Longitude or projected X column.
#' @param latitude_column Latitude or projected Y column.
#' @param source_data_format Machine-readable source-data format.
#' @param version Specification version.
#' @return A fingerprinted spatial-genetics publication specification.
#' @export
new_publication_spatial_spec <- function(
    id_column = "sample_id", longitude_column = "longitude",
    latitude_column = "latitude", source_data_format = "tsv",
    version = "1.0.0") {
  columns <- c(id_column, longitude_column, latitude_column)
  if (anyNA(columns) || any(!nzchar(columns)) || anyDuplicated(columns)) {
    stop("Spatial specification columns must be unique non-empty names.", call. = FALSE)
  }
  source_data_format <- match.arg(source_data_format, c("tsv", "csv"))
  spec <- list(
    record_type = "popgenvcf_publication_spatial_spec",
    schema_version = "1.0.0", id_column = id_column,
    longitude_column = longitude_column, latitude_column = latitude_column,
    source_data_format = source_data_format, version = version
  )
  spec$fingerprint <- .publication_spatial_fingerprint(spec)
  class(spec) <- c("PopgenVCFPublicationSpatialSpec", "list")
  validate_publication_spatial_spec(spec)
  spec
}

#' Validate a publication spatial-genetics specification
#' @param spec A spatial-genetics publication specification.
#' @return `TRUE`, invisibly.
#' @export
validate_publication_spatial_spec <- function(spec) {
  if (!inherits(spec, "PopgenVCFPublicationSpatialSpec") ||
      !identical(spec$schema_version, "1.0.0") ||
      !identical(spec$fingerprint, .publication_spatial_fingerprint(spec))) {
    stop("Invalid publication spatial-genetics specification.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Create a deterministic publication spatial-genetics output
#' @param spec A validated spatial-genetics publication specification.
#' @param coordinates Authoritative sample-coordinate table.
#' @param statistics Authoritative spatial-statistic summary table.
#' @param neighborhoods Optional authoritative neighborhood or distance-class table.
#' @param permutation Optional authoritative permutation evidence.
#' @param result_fingerprint Fingerprint of the authoritative spatial result.
#' @param figure_binding Optional publication figure-style binding.
#' @return A fingerprinted publication spatial-genetics output.
#' @export
new_publication_spatial_output <- function(
    spec, coordinates, statistics, neighborhoods = NULL, permutation = NULL,
    result_fingerprint, figure_binding = NULL) {
  validate_publication_spatial_spec(spec)
  if (!is.data.frame(coordinates) || !is.data.frame(statistics)) {
    stop("coordinates and statistics must be data frames.", call. = FALSE)
  }
  required <- c(spec$id_column, spec$longitude_column, spec$latitude_column)
  if (!all(required %in% names(coordinates))) {
    stop("coordinates is missing required spatial columns.", call. = FALSE)
  }
  coordinates[[spec$id_column]] <- as.character(coordinates[[spec$id_column]])
  if (anyNA(coordinates[[spec$id_column]]) || any(!nzchar(coordinates[[spec$id_column]])) ||
      anyDuplicated(coordinates[[spec$id_column]])) {
    stop("Spatial sample identities must be unique and non-empty.", call. = FALSE)
  }
  for (column in c(spec$longitude_column, spec$latitude_column)) {
    coordinates[[column]] <- as.numeric(coordinates[[column]])
    if (any(!is.finite(coordinates[[column]]))) {
      stop("Spatial coordinates must be finite.", call. = FALSE)
    }
  }
  coordinates <- coordinates[order(coordinates[[spec$id_column]]), , drop = FALSE]
  statistics <- statistics[do.call(order, statistics), , drop = FALSE]
  if (!is.null(neighborhoods)) {
    if (!is.data.frame(neighborhoods)) stop("neighborhoods must be a data frame or NULL.", call. = FALSE)
    neighborhoods <- neighborhoods[do.call(order, neighborhoods), , drop = FALSE]
  }
  if (!is.null(permutation)) {
    if (!is.data.frame(permutation)) stop("permutation must be a data frame or NULL.", call. = FALSE)
    permutation <- permutation[do.call(order, permutation), , drop = FALSE]
  }
  output <- list(
    record_type = "popgenvcf_publication_spatial_output",
    schema_version = "1.0.0", specification_fingerprint = spec$fingerprint,
    result_fingerprint = result_fingerprint,
    figure_binding_fingerprint = if (is.null(figure_binding)) NULL else figure_binding$fingerprint,
    coordinates = coordinates, statistics = statistics,
    neighborhoods = neighborhoods, permutation = permutation,
    source_data = list(coordinates = coordinates, statistics = statistics,
                       neighborhoods = neighborhoods, permutation = permutation)
  )
  output$fingerprint <- .publication_spatial_fingerprint(output)
  class(output) <- c("PopgenVCFPublicationSpatialOutput", "list")
  validate_publication_spatial_output(output, spec)
  output
}

#' Validate a publication spatial-genetics output
#' @param output A spatial-genetics publication output.
#' @param spec Originating specification.
#' @return `TRUE`, invisibly.
#' @export
validate_publication_spatial_output <- function(output, spec) {
  validate_publication_spatial_spec(spec)
  if (!inherits(output, "PopgenVCFPublicationSpatialOutput") ||
      !identical(output$specification_fingerprint, spec$fingerprint)) {
    stop("output is not bound to the supplied spatial specification.", call. = FALSE)
  }
  if (!identical(output$source_data$coordinates, output$coordinates) ||
      !identical(output$source_data$statistics, output$statistics) ||
      !identical(output$source_data$neighborhoods, output$neighborhoods) ||
      !identical(output$source_data$permutation, output$permutation)) {
    stop("Spatial source data drifted from the publication output.", call. = FALSE)
  }
  if (!identical(output$fingerprint, .publication_spatial_fingerprint(output))) {
    stop("Spatial output fingerprint mismatch.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Create a deterministic publication spatial-genetics caption
#' @param output A validated spatial-genetics output.
#' @param spec Originating specification.
#' @return One manuscript-ready caption string.
#' @export
publication_spatial_caption <- function(output, spec) {
  validate_publication_spatial_output(output, spec)
  sprintf("Spatial-genetics summary across %d georeferenced samples.", nrow(output$coordinates))
}

#' Render a deterministic publication spatial-genetics report
#' @param output A spatial-genetics publication output.
#' @param spec Originating specification.
#' @return Markdown report lines.
#' @export
publication_spatial_report <- function(output, spec) {
  validate_publication_spatial_output(output, spec)
  c(
    "# Publication spatial-genetics output", "",
    sprintf("- Georeferenced samples: `%d`", nrow(output$coordinates)),
    sprintf("- Spatial statistic rows: `%d`", nrow(output$statistics)),
    sprintf("- Neighborhood evidence: `%s`", if (is.null(output$neighborhoods)) "absent" else "present"),
    sprintf("- Permutation evidence: `%s`", if (is.null(output$permutation)) "absent" else "present"),
    sprintf("- Result fingerprint: `%s`", output$result_fingerprint),
    sprintf("- Output fingerprint: `%s`", output$fingerprint)
  )
}
