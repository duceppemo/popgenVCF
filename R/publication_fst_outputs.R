# Phase 0.9.7 - deterministic publication FST outputs

.publication_fst_fingerprint <- function(x) {
  candidate <- unclass(x)
  candidate$fingerprint <- NULL
  digest::digest(candidate, algo = "sha256", serialize = TRUE)
}

#' Create a publication FST specification
#'
#' @param population1_column First population column.
#' @param population2_column Second population column.
#' @param estimate_column FST estimate column.
#' @param lower_column Optional lower uncertainty bound column.
#' @param upper_column Optional upper uncertainty bound column.
#' @param source_data_format Machine-readable source-data format.
#' @return A fingerprinted publication FST specification.
#' @export
new_publication_fst_spec <- function(population1_column = "population1",
                                     population2_column = "population2",
                                     estimate_column = "fst",
                                     lower_column = "lower",
                                     upper_column = "upper",
                                     source_data_format = c("tsv", "csv")) {
  values <- list(population1_column, population2_column, estimate_column,
                 lower_column, upper_column)
  if (any(!vapply(values, function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x)), logical(1)))) {
    stop("FST column names must be non-empty character scalars.", call. = FALSE)
  }
  spec <- list(
    record_type = "popgenvcf_publication_fst_spec",
    schema_version = "1.0.0",
    population1_column = trimws(population1_column),
    population2_column = trimws(population2_column),
    estimate_column = trimws(estimate_column),
    lower_column = trimws(lower_column),
    upper_column = trimws(upper_column),
    source_data_format = match.arg(source_data_format)
  )
  spec$fingerprint <- .publication_fst_fingerprint(spec)
  class(spec) <- c("PopgenVCFPublicationFSTSpec", "list")
  validate_publication_fst_spec(spec)
  spec
}

#' Validate a publication FST specification
#' @param spec A publication FST specification.
#' @return `TRUE`, invisibly.
#' @export
validate_publication_fst_spec <- function(spec) {
  if (!inherits(spec, "PopgenVCFPublicationFSTSpec") || !identical(spec$schema_version, "1.0.0")) {
    stop("spec must be a supported publication FST specification.", call. = FALSE)
  }
  if (!identical(spec$fingerprint, .publication_fst_fingerprint(spec))) {
    stop("FST specification fingerprint mismatch.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Create deterministic publication FST output
#'
#' @param spec A validated publication FST specification.
#' @param pairwise Authoritative pairwise FST table.
#' @param global_fst Optional authoritative global FST estimate.
#' @param result_fingerprint Fingerprint of the authoritative FST result.
#' @param figure_binding Optional publication figure-style binding.
#' @return A fingerprinted publication FST output.
#' @export
new_publication_fst_output <- function(spec, pairwise, global_fst = NULL,
                                       result_fingerprint,
                                       figure_binding = NULL) {
  validate_publication_fst_spec(spec)
  if (!is.data.frame(pairwise)) stop("pairwise must be a data frame.", call. = FALSE)
  required <- c(spec$population1_column, spec$population2_column, spec$estimate_column)
  if (!all(required %in% names(pairwise))) stop("pairwise is missing required FST columns.", call. = FALSE)
  pairwise <- as.data.frame(pairwise, stringsAsFactors = FALSE, check.names = FALSE)
  p1 <- as.character(pairwise[[spec$population1_column]])
  p2 <- as.character(pairwise[[spec$population2_column]])
  if (anyNA(p1) || anyNA(p2) || any(!nzchar(p1)) || any(!nzchar(p2)) || any(p1 == p2)) {
    stop("Population pairs must be non-empty, distinct values.", call. = FALSE)
  }
  canonical1 <- pmin(p1, p2)
  canonical2 <- pmax(p1, p2)
  pairwise[[spec$population1_column]] <- canonical1
  pairwise[[spec$population2_column]] <- canonical2
  pairwise[[spec$estimate_column]] <- as.numeric(pairwise[[spec$estimate_column]])
  if (any(!is.finite(pairwise[[spec$estimate_column]]))) stop("FST estimates must be finite.", call. = FALSE)
  pairwise <- pairwise[order(canonical1, canonical2), , drop = FALSE]
  key <- paste(pairwise[[spec$population1_column]], pairwise[[spec$population2_column]], sep = "\r")
  if (anyDuplicated(key)) stop("Population pairs must be unique.", call. = FALSE)
  for (bound in c(spec$lower_column, spec$upper_column)) {
    if (bound %in% names(pairwise)) {
      pairwise[[bound]] <- as.numeric(pairwise[[bound]])
      if (any(!is.finite(pairwise[[bound]]))) stop("FST uncertainty bounds must be finite.", call. = FALSE)
    }
  }
  if (all(c(spec$lower_column, spec$upper_column) %in% names(pairwise))) {
    if (any(pairwise[[spec$lower_column]] > pairwise[[spec$estimate_column]]) ||
        any(pairwise[[spec$upper_column]] < pairwise[[spec$estimate_column]])) {
      stop("FST uncertainty intervals must contain their estimates.", call. = FALSE)
    }
  }
  if (!is.null(global_fst)) {
    global_fst <- as.numeric(global_fst)
    if (length(global_fst) != 1L || !is.finite(global_fst)) stop("global_fst must be one finite value.", call. = FALSE)
  }
  if (!is.character(result_fingerprint) || length(result_fingerprint) != 1L || is.na(result_fingerprint) || !nzchar(result_fingerprint)) {
    stop("result_fingerprint must be one non-empty character value.", call. = FALSE)
  }
  populations <- sort(unique(c(pairwise[[spec$population1_column]], pairwise[[spec$population2_column]])))
  output <- list(
    record_type = "popgenvcf_publication_fst_output",
    schema_version = "1.0.0",
    specification_fingerprint = spec$fingerprint,
    result_fingerprint = result_fingerprint,
    figure_binding_fingerprint = if (is.null(figure_binding)) NULL else figure_binding$fingerprint,
    global_fst = global_fst,
    populations = populations,
    pairwise = pairwise,
    source_data = list(pairwise = pairwise)
  )
  output$fingerprint <- .publication_fst_fingerprint(output)
  class(output) <- c("PopgenVCFPublicationFSTOutput", "list")
  validate_publication_fst_output(output, spec)
  output
}

#' Validate a publication FST output
#' @param output A publication FST output.
#' @param spec Originating specification.
#' @return `TRUE`, invisibly.
#' @export
validate_publication_fst_output <- function(output, spec) {
  validate_publication_fst_spec(spec)
  if (!inherits(output, "PopgenVCFPublicationFSTOutput") || !identical(output$specification_fingerprint, spec$fingerprint)) {
    stop("output is not bound to the supplied FST specification.", call. = FALSE)
  }
  if (!identical(output$source_data$pairwise, output$pairwise)) {
    stop("FST source data drifted from the publication output.", call. = FALSE)
  }
  if (!identical(output$fingerprint, .publication_fst_fingerprint(output))) {
    stop("FST output fingerprint mismatch.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Create a deterministic publication FST caption
#' @param output A publication FST output.
#' @param spec Originating specification.
#' @return One manuscript-ready caption string.
#' @export
publication_fst_caption <- function(output, spec) {
  validate_publication_fst_output(output, spec)
  sprintf("Pairwise FST among %d populations across %d population comparisons%s.",
          length(output$populations), nrow(output$pairwise),
          if (is.null(output$global_fst)) "" else sprintf("; global FST = %.4f", output$global_fst))
}

#' Render a deterministic publication FST report
#' @param output A publication FST output.
#' @param spec Originating specification.
#' @return Markdown report lines.
#' @export
publication_fst_report <- function(output, spec) {
  validate_publication_fst_output(output, spec)
  c("# Publication FST output", "",
    sprintf("- Populations: `%d`", length(output$populations)),
    sprintf("- Pairwise comparisons: `%d`", nrow(output$pairwise)),
    sprintf("- Global FST: `%s`", if (is.null(output$global_fst)) "not supplied" else format(output$global_fst, digits = 6)),
    sprintf("- Result fingerprint: `%s`", output$result_fingerprint),
    sprintf("- Output fingerprint: `%s`", output$fingerprint))
}
