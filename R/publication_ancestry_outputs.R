# Phase 0.9.9 - deterministic publication ancestry outputs

.publication_ancestry_fingerprint <- function(x) {
  candidate <- unclass(x)
  candidate$fingerprint <- NULL
  digest::digest(candidate, algo = "sha256", serialize = TRUE)
}

.publication_ancestry_table <- function(x, name) {
  if (is.null(x)) return(NULL)
  if (!is.data.frame(x)) stop(sprintf("%s must be a data frame.", name), call. = FALSE)
  as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
}

#' Create a publication ancestry specification
#'
#' @param sample_column Sample identity column.
#' @param population_column Optional population identity column.
#' @param ancestry_prefix Prefix used by ancestry coefficient columns.
#' @param source_data_format Machine-readable source-data format.
#' @param version Specification version.
#' @return A fingerprinted ancestry publication specification.
#' @export
new_publication_ancestry_spec <- function(
    sample_column = "sample_id", population_column = "population",
    ancestry_prefix = "Q", source_data_format = "tsv", version = "1.0.0") {
  for (value in list(sample_column, ancestry_prefix, version)) {
    if (!is.character(value) || length(value) != 1L || !nzchar(value)) {
      stop("Ancestry specification names and version must be non-empty strings.", call. = FALSE)
    }
  }
  if (!is.null(population_column) &&
      (!is.character(population_column) || length(population_column) != 1L || !nzchar(population_column))) {
    stop("population_column must be NULL or one non-empty name.", call. = FALSE)
  }
  source_data_format <- match.arg(source_data_format, c("tsv", "csv"))
  spec <- list(
    record_type = "popgenvcf_publication_ancestry_spec",
    schema_version = "1.0.0",
    sample_column = sample_column,
    population_column = population_column,
    ancestry_prefix = ancestry_prefix,
    source_data_format = source_data_format,
    version = version
  )
  spec$fingerprint <- .publication_ancestry_fingerprint(spec)
  class(spec) <- c("PopgenVCFPublicationAncestrySpec", "list")
  validate_publication_ancestry_spec(spec)
  spec
}

#' Validate a publication ancestry specification
#' @param spec An ancestry publication specification.
#' @return `TRUE`, invisibly.
#' @export
validate_publication_ancestry_spec <- function(spec) {
  if (!inherits(spec, "PopgenVCFPublicationAncestrySpec") ||
      !identical(spec$schema_version, "1.0.0") ||
      !identical(spec$fingerprint, .publication_ancestry_fingerprint(spec))) {
    stop("Invalid or mutated ancestry publication specification.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Create a deterministic publication ancestry output
#'
#' @param spec Validated ancestry publication specification.
#' @param q_matrix Authoritative sample-level ancestry coefficients.
#' @param consensus Optional authoritative consensus assignment table.
#' @param replicate_diagnostics Optional authoritative replicate diagnostics.
#' @param k_selection Optional authoritative K-selection evidence.
#' @param result_fingerprint Fingerprint of the authoritative ancestry result.
#' @param figure_binding Optional publication figure-style binding.
#' @return A fingerprinted ancestry publication output.
#' @export
new_publication_ancestry_output <- function(
    spec, q_matrix, consensus = NULL, replicate_diagnostics = NULL,
    k_selection = NULL, result_fingerprint, figure_binding = NULL) {
  validate_publication_ancestry_spec(spec)
  q_matrix <- .publication_ancestry_table(q_matrix, "q_matrix")
  if (!spec$sample_column %in% names(q_matrix)) stop("q_matrix is missing the sample identity column.", call. = FALSE)
  q_columns <- grep(paste0("^", spec$ancestry_prefix), names(q_matrix), value = TRUE)
  if (!length(q_columns)) stop("q_matrix contains no ancestry coefficient columns.", call. = FALSE)
  samples <- as.character(q_matrix[[spec$sample_column]])
  if (anyNA(samples) || any(!nzchar(samples)) || anyDuplicated(samples)) {
    stop("Sample identities must be unique and non-empty.", call. = FALSE)
  }
  for (column in q_columns) {
    q_matrix[[column]] <- as.numeric(q_matrix[[column]])
    if (any(!is.finite(q_matrix[[column]])) || any(q_matrix[[column]] < 0) || any(q_matrix[[column]] > 1)) {
      stop("Ancestry coefficients must be finite values in [0, 1].", call. = FALSE)
    }
  }
  totals <- rowSums(q_matrix[q_columns])
  if (any(abs(totals - 1) > 1e-6)) stop("Each Q-matrix row must sum to one.", call. = FALSE)
  order_columns <- c(spec$population_column, spec$sample_column)
  order_columns <- order_columns[!vapply(order_columns, is.null, logical(1)) & order_columns %in% names(q_matrix)]
  ord <- do.call(order, lapply(order_columns, function(column) as.character(q_matrix[[column]])))
  q_matrix <- q_matrix[ord, , drop = FALSE]
  consensus <- .publication_ancestry_table(consensus, "consensus")
  replicate_diagnostics <- .publication_ancestry_table(replicate_diagnostics, "replicate_diagnostics")
  k_selection <- .publication_ancestry_table(k_selection, "k_selection")
  output <- list(
    record_type = "popgenvcf_publication_ancestry_output",
    schema_version = "1.0.0",
    specification_fingerprint = spec$fingerprint,
    result_fingerprint = result_fingerprint,
    figure_binding_fingerprint = if (is.null(figure_binding)) NULL else figure_binding$fingerprint,
    k = length(q_columns),
    q_columns = q_columns,
    q_matrix = q_matrix,
    consensus = consensus,
    replicate_diagnostics = replicate_diagnostics,
    k_selection = k_selection,
    source_data = list(
      q_matrix = q_matrix,
      consensus = consensus,
      replicate_diagnostics = replicate_diagnostics,
      k_selection = k_selection
    )
  )
  output$fingerprint <- .publication_ancestry_fingerprint(output)
  class(output) <- c("PopgenVCFPublicationAncestryOutput", "list")
  validate_publication_ancestry_output(output, spec)
  output
}

#' Validate a publication ancestry output
#' @param output An ancestry publication output.
#' @param spec Originating specification.
#' @return `TRUE`, invisibly.
#' @export
validate_publication_ancestry_output <- function(output, spec) {
  validate_publication_ancestry_spec(spec)
  if (!inherits(output, "PopgenVCFPublicationAncestryOutput") ||
      !identical(output$specification_fingerprint, spec$fingerprint)) {
    stop("output is not bound to the supplied ancestry specification.", call. = FALSE)
  }
  for (name in c("q_matrix", "consensus", "replicate_diagnostics", "k_selection")) {
    if (!identical(output$source_data[[name]], output[[name]])) {
      stop("Ancestry source data drifted from the publication output.", call. = FALSE)
    }
  }
  if (!identical(output$fingerprint, .publication_ancestry_fingerprint(output))) {
    stop("Ancestry output fingerprint mismatch.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Create a deterministic publication ancestry caption
#' @param output A validated ancestry output.
#' @param spec Originating specification.
#' @return One manuscript-ready caption string.
#' @export
publication_ancestry_caption <- function(output, spec) {
  validate_publication_ancestry_output(output, spec)
  sprintf("Ancestry coefficient profiles for %d samples at K = %d.", nrow(output$q_matrix), output$k)
}

#' Render a deterministic publication ancestry report
#' @param output An ancestry publication output.
#' @param spec Originating specification.
#' @return Markdown report lines.
#' @export
publication_ancestry_report <- function(output, spec) {
  validate_publication_ancestry_output(output, spec)
  c(
    "# Publication ancestry output", "",
    sprintf("- Samples: `%d`", nrow(output$q_matrix)),
    sprintf("- K: `%d`", output$k),
    sprintf("- Consensus table: `%s`", if (is.null(output$consensus)) "absent" else "present"),
    sprintf("- Replicate diagnostics: `%s`", if (is.null(output$replicate_diagnostics)) "absent" else "present"),
    sprintf("- K-selection evidence: `%s`", if (is.null(output$k_selection)) "absent" else "present"),
    sprintf("- Result fingerprint: `%s`", output$result_fingerprint),
    sprintf("- Output fingerprint: `%s`", output$fingerprint)
  )
}
