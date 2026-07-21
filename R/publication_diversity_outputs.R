# Phase 0.9.8 - deterministic publication diversity outputs

.publication_diversity_fingerprint <- function(x) {
  candidate <- unclass(x)
  candidate$fingerprint <- NULL
  digest::digest(candidate, algo = "sha256", serialize = TRUE)
}

.publication_diversity_table <- function(x, name) {
  if (is.null(x)) return(NULL)
  if (!is.data.frame(x)) stop(sprintf("%s must be a data frame.", name), call. = FALSE)
  as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
}

#' Create a publication diversity specification
#'
#' @param population_column Population identity column.
#' @param metric_columns Ordered diversity metric columns.
#' @param interval_columns Optional named lower/upper interval columns.
#' @param source_data_format Machine-readable source-data format.
#' @param version Specification version.
#' @return A fingerprinted publication diversity specification.
#' @export
new_publication_diversity_spec <- function(
    population_column = "population",
    metric_columns = c("observed_heterozygosity", "expected_heterozygosity"),
    interval_columns = NULL,
    source_data_format = "tsv",
    version = "1.0.0") {
  if (!is.character(population_column) || length(population_column) != 1L || !nzchar(population_column)) {
    stop("population_column must be one non-empty name.", call. = FALSE)
  }
  metric_columns <- as.character(metric_columns)
  if (!length(metric_columns) || anyNA(metric_columns) || any(!nzchar(metric_columns)) || anyDuplicated(metric_columns)) {
    stop("metric_columns must contain unique non-empty names.", call. = FALSE)
  }
  if (!is.null(interval_columns)) {
    interval_columns <- as.character(interval_columns)
    if (anyNA(interval_columns) || any(!nzchar(interval_columns)) || anyDuplicated(interval_columns)) {
      stop("interval_columns must contain unique non-empty names.", call. = FALSE)
    }
  }
  source_data_format <- match.arg(source_data_format, c("tsv", "csv"))
  spec <- list(
    record_type = "popgenvcf_publication_diversity_spec",
    schema_version = "1.0.0",
    population_column = population_column,
    metric_columns = metric_columns,
    interval_columns = interval_columns,
    source_data_format = source_data_format,
    version = version
  )
  spec$fingerprint <- .publication_diversity_fingerprint(spec)
  class(spec) <- c("PopgenVCFPublicationDiversitySpec", "list")
  validate_publication_diversity_spec(spec)
  spec
}

#' Validate a publication diversity specification
#' @param spec A diversity publication specification.
#' @return `TRUE`, invisibly.
#' @export
validate_publication_diversity_spec <- function(spec) {
  if (!inherits(spec, "PopgenVCFPublicationDiversitySpec") || !identical(spec$schema_version, "1.0.0")) {
    stop("spec must be a supported diversity publication specification.", call. = FALSE)
  }
  if (!identical(spec$fingerprint, .publication_diversity_fingerprint(spec))) {
    stop("Diversity specification fingerprint mismatch.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Create a deterministic publication diversity output
#'
#' @param spec A validated diversity publication specification.
#' @param summaries Authoritative per-population diversity summaries.
#' @param private_alleles Optional authoritative private-allele table.
#' @param frequency_spectra Optional authoritative frequency-spectrum table.
#' @param result_fingerprint Fingerprint of the authoritative diversity result.
#' @param figure_binding Optional publication figure-style binding.
#' @return A fingerprinted publication diversity output.
#' @export
new_publication_diversity_output <- function(
    spec, summaries, private_alleles = NULL, frequency_spectra = NULL,
    result_fingerprint, figure_binding = NULL) {
  validate_publication_diversity_spec(spec)
  summaries <- .publication_diversity_table(summaries, "summaries")
  required <- c(spec$population_column, spec$metric_columns, spec$interval_columns)
  if (!all(required %in% names(summaries))) stop("summaries is missing required diversity columns.", call. = FALSE)
  populations <- as.character(summaries[[spec$population_column]])
  if (anyNA(populations) || any(!nzchar(populations)) || anyDuplicated(populations)) {
    stop("Population identities must be unique and non-empty.", call. = FALSE)
  }
  for (column in c(spec$metric_columns, spec$interval_columns)) {
    summaries[[column]] <- as.numeric(summaries[[column]])
    if (any(!is.finite(summaries[[column]]))) stop("Diversity estimates and intervals must be finite.", call. = FALSE)
  }
  summaries <- summaries[order(populations), , drop = FALSE]
  private_alleles <- .publication_diversity_table(private_alleles, "private_alleles")
  frequency_spectra <- .publication_diversity_table(frequency_spectra, "frequency_spectra")
  output <- list(
    record_type = "popgenvcf_publication_diversity_output",
    schema_version = "1.0.0",
    specification_fingerprint = spec$fingerprint,
    result_fingerprint = result_fingerprint,
    figure_binding_fingerprint = if (is.null(figure_binding)) NULL else figure_binding$fingerprint,
    summaries = summaries,
    private_alleles = private_alleles,
    frequency_spectra = frequency_spectra,
    source_data = list(
      summaries = summaries,
      private_alleles = private_alleles,
      frequency_spectra = frequency_spectra
    )
  )
  output$fingerprint <- .publication_diversity_fingerprint(output)
  class(output) <- c("PopgenVCFPublicationDiversityOutput", "list")
  validate_publication_diversity_output(output, spec)
  output
}

#' Validate a publication diversity output
#' @param output A diversity publication output.
#' @param spec Originating specification.
#' @return `TRUE`, invisibly.
#' @export
validate_publication_diversity_output <- function(output, spec) {
  validate_publication_diversity_spec(spec)
  if (!inherits(output, "PopgenVCFPublicationDiversityOutput") ||
      !identical(output$specification_fingerprint, spec$fingerprint)) {
    stop("output is not bound to the supplied diversity specification.", call. = FALSE)
  }
  if (!identical(output$source_data$summaries, output$summaries) ||
      !identical(output$source_data$private_alleles, output$private_alleles) ||
      !identical(output$source_data$frequency_spectra, output$frequency_spectra)) {
    stop("Diversity source data drifted from the publication output.", call. = FALSE)
  }
  if (!identical(output$fingerprint, .publication_diversity_fingerprint(output))) {
    stop("Diversity output fingerprint mismatch.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Create a deterministic publication diversity caption
#' @param output A validated diversity output.
#' @param spec Originating specification.
#' @return One manuscript-ready caption string.
#' @export
publication_diversity_caption <- function(output, spec) {
  validate_publication_diversity_output(output, spec)
  sprintf(
    "Population diversity summaries for %d populations across %d reported metrics.",
    nrow(output$summaries), length(spec$metric_columns)
  )
}

#' Render a deterministic publication diversity report
#' @param output A diversity publication output.
#' @param spec Originating specification.
#' @return Markdown report lines.
#' @export
publication_diversity_report <- function(output, spec) {
  validate_publication_diversity_output(output, spec)
  c(
    "# Publication diversity output", "",
    sprintf("- Populations: `%d`", nrow(output$summaries)),
    sprintf("- Metrics: `%s`", paste(spec$metric_columns, collapse = ", ")),
    sprintf("- Private-allele table: `%s`", if (is.null(output$private_alleles)) "absent" else "present"),
    sprintf("- Frequency-spectrum table: `%s`", if (is.null(output$frequency_spectra)) "absent" else "present"),
    sprintf("- Result fingerprint: `%s`", output$result_fingerprint),
    sprintf("- Output fingerprint: `%s`", output$fingerprint)
  )
}
