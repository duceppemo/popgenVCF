# Phase 0.9.10 - deterministic publication AMOVA outputs

.publication_amova_fingerprint <- function(x) {
  candidate <- unclass(x)
  candidate$fingerprint <- NULL
  digest::digest(candidate, algo = "sha256", serialize = TRUE)
}

.publication_amova_table <- function(x, name) {
  if (is.null(x)) return(NULL)
  if (!is.data.frame(x)) stop(sprintf("%s must be a data frame.", name), call. = FALSE)
  as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
}

#' Create a publication AMOVA specification
#'
#' @param source_column Variance-source identity column.
#' @param component_columns Ordered AMOVA component columns.
#' @param phi_columns Optional ordered Phi-statistic columns.
#' @param source_data_format Machine-readable source-data format.
#' @param version Specification version.
#' @return A fingerprinted publication AMOVA specification.
#' @export
new_publication_amova_spec <- function(
    source_column = "source",
    component_columns = c("df", "sum_squares", "variance_component", "percent_variation"),
    phi_columns = NULL,
    source_data_format = "tsv",
    version = "1.0.0") {
  if (!is.character(source_column) || length(source_column) != 1L || !nzchar(source_column)) {
    stop("source_column must be one non-empty name.", call. = FALSE)
  }
  component_columns <- as.character(component_columns)
  if (!length(component_columns) || anyNA(component_columns) || any(!nzchar(component_columns)) || anyDuplicated(component_columns)) {
    stop("component_columns must contain unique non-empty names.", call. = FALSE)
  }
  if (!is.null(phi_columns)) {
    phi_columns <- as.character(phi_columns)
    if (anyNA(phi_columns) || any(!nzchar(phi_columns)) || anyDuplicated(phi_columns)) {
      stop("phi_columns must contain unique non-empty names.", call. = FALSE)
    }
  }
  source_data_format <- match.arg(source_data_format, c("tsv", "csv"))
  spec <- list(
    record_type = "popgenvcf_publication_amova_spec",
    schema_version = "1.0.0",
    source_column = source_column,
    component_columns = component_columns,
    phi_columns = phi_columns,
    source_data_format = source_data_format,
    version = version
  )
  spec$fingerprint <- .publication_amova_fingerprint(spec)
  class(spec) <- c("PopgenVCFPublicationAMOVASpec", "list")
  validate_publication_amova_spec(spec)
  spec
}

#' Validate a publication AMOVA specification
#' @param spec An AMOVA publication specification.
#' @return `TRUE`, invisibly.
#' @export
validate_publication_amova_spec <- function(spec) {
  if (!inherits(spec, "PopgenVCFPublicationAMOVASpec") || !identical(spec$schema_version, "1.0.0")) {
    stop("spec must be a supported AMOVA publication specification.", call. = FALSE)
  }
  if (!identical(spec$fingerprint, .publication_amova_fingerprint(spec))) {
    stop("AMOVA specification fingerprint mismatch.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Create a deterministic publication AMOVA output
#'
#' @param spec A validated AMOVA publication specification.
#' @param variance_components Authoritative AMOVA variance-component table.
#' @param phi_statistics Optional authoritative Phi-statistic table.
#' @param permutation_tests Optional authoritative permutation-test table.
#' @param result_fingerprint Fingerprint of the authoritative AMOVA result.
#' @param figure_binding Optional publication figure-style binding.
#' @return A fingerprinted publication AMOVA output.
#' @export
new_publication_amova_output <- function(
    spec, variance_components, phi_statistics = NULL, permutation_tests = NULL,
    result_fingerprint, figure_binding = NULL) {
  validate_publication_amova_spec(spec)
  variance_components <- .publication_amova_table(variance_components, "variance_components")
  required <- c(spec$source_column, spec$component_columns)
  if (!all(required %in% names(variance_components))) {
    stop("variance_components is missing required AMOVA columns.", call. = FALSE)
  }
  sources <- as.character(variance_components[[spec$source_column]])
  if (anyNA(sources) || any(!nzchar(sources)) || anyDuplicated(sources)) {
    stop("AMOVA variance sources must be unique and non-empty.", call. = FALSE)
  }
  for (column in spec$component_columns) {
    variance_components[[column]] <- as.numeric(variance_components[[column]])
    if (any(!is.finite(variance_components[[column]]))) {
      stop("AMOVA component values must be finite.", call. = FALSE)
    }
  }
  variance_components <- variance_components[order(sources), , drop = FALSE]
  phi_statistics <- .publication_amova_table(phi_statistics, "phi_statistics")
  if (!is.null(phi_statistics) && length(spec$phi_columns)) {
    if (!all(spec$phi_columns %in% names(phi_statistics))) {
      stop("phi_statistics is missing required Phi columns.", call. = FALSE)
    }
    for (column in spec$phi_columns) {
      phi_statistics[[column]] <- as.numeric(phi_statistics[[column]])
      if (any(!is.finite(phi_statistics[[column]]))) stop("Phi statistics must be finite.", call. = FALSE)
    }
    phi_statistics <- phi_statistics[do.call(order, phi_statistics), , drop = FALSE]
  }
  permutation_tests <- .publication_amova_table(permutation_tests, "permutation_tests")
  if (!is.character(result_fingerprint) || length(result_fingerprint) != 1L || !nzchar(result_fingerprint)) {
    stop("result_fingerprint must be one non-empty value.", call. = FALSE)
  }
  output <- list(
    record_type = "popgenvcf_publication_amova_output",
    schema_version = "1.0.0",
    specification_fingerprint = spec$fingerprint,
    result_fingerprint = result_fingerprint,
    figure_binding_fingerprint = if (is.null(figure_binding)) NULL else figure_binding$fingerprint,
    variance_components = variance_components,
    phi_statistics = phi_statistics,
    permutation_tests = permutation_tests,
    source_data = list(
      variance_components = variance_components,
      phi_statistics = phi_statistics,
      permutation_tests = permutation_tests
    )
  )
  output$fingerprint <- .publication_amova_fingerprint(output)
  class(output) <- c("PopgenVCFPublicationAMOVAOutput", "list")
  validate_publication_amova_output(output, spec)
  output
}

#' Validate a publication AMOVA output
#' @param output An AMOVA publication output.
#' @param spec Originating specification.
#' @return `TRUE`, invisibly.
#' @export
validate_publication_amova_output <- function(output, spec) {
  validate_publication_amova_spec(spec)
  if (!inherits(output, "PopgenVCFPublicationAMOVAOutput") ||
      !identical(output$specification_fingerprint, spec$fingerprint)) {
    stop("output is not bound to the supplied AMOVA specification.", call. = FALSE)
  }
  if (!identical(output$source_data$variance_components, output$variance_components) ||
      !identical(output$source_data$phi_statistics, output$phi_statistics) ||
      !identical(output$source_data$permutation_tests, output$permutation_tests)) {
    stop("AMOVA source data drifted from the publication output.", call. = FALSE)
  }
  if (!identical(output$fingerprint, .publication_amova_fingerprint(output))) {
    stop("AMOVA output fingerprint mismatch.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Create a deterministic publication AMOVA caption
#' @param output A validated AMOVA output.
#' @param spec Originating specification.
#' @return One manuscript-ready caption string.
#' @export
publication_amova_caption <- function(output, spec) {
  validate_publication_amova_output(output, spec)
  sprintf(
    "Analysis of molecular variance across %d hierarchical variance sources%s.",
    nrow(output$variance_components),
    if (is.null(output$permutation_tests)) "" else ", with permutation-test evidence"
  )
}

#' Render a deterministic publication AMOVA report
#' @param output An AMOVA publication output.
#' @param spec Originating specification.
#' @return Markdown report lines.
#' @export
publication_amova_report <- function(output, spec) {
  validate_publication_amova_output(output, spec)
  c(
    "# Publication AMOVA output", "",
    sprintf("- Variance sources: `%d`", nrow(output$variance_components)),
    sprintf("- Component columns: `%s`", paste(spec$component_columns, collapse = ", ")),
    sprintf("- Phi-statistic table: `%s`", if (is.null(output$phi_statistics)) "absent" else "present"),
    sprintf("- Permutation evidence: `%s`", if (is.null(output$permutation_tests)) "absent" else "present"),
    sprintf("- Result fingerprint: `%s`", output$result_fingerprint),
    sprintf("- Output fingerprint: `%s`", output$fingerprint)
  )
}
