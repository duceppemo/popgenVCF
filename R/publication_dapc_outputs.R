# Phase 0.9.6 - deterministic publication DAPC outputs

.publication_dapc_fingerprint <- function(x) {
  candidate <- unclass(x)
  candidate$fingerprint <- NULL
  digest::digest(candidate, algo = "sha256", serialize = TRUE)
}

.publication_dapc_scalar <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    stop(sprintf("%s must be one non-empty character value.", name), call. = FALSE)
  }
  trimws(x)
}

.publication_dapc_table <- function(x, name) {
  if (is.null(x)) return(NULL)
  if (!is.data.frame(x)) stop(sprintf("%s must be a data frame.", name), call. = FALSE)
  as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
}

#' Create a publication DAPC specification
#'
#' @param sample_id_column Sample identity column in discriminant coordinates.
#' @param population_column Optional known-population column.
#' @param cluster_column Optional inferred-cluster column.
#' @param axis_columns Ordered discriminant coordinate columns.
#' @param selected_k Optional selected number of clusters.
#' @param source_data_format Machine-readable source-data format.
#' @param version Specification version.
#' @return A fingerprinted `PopgenVCFPublicationDAPCSpec`.
#' @export
new_publication_dapc_spec <- function(
    sample_id_column = "sample", population_column = "population",
    cluster_column = "cluster", axis_columns = c("LD1", "LD2"),
    selected_k = NULL, source_data_format = "tsv", version = "1.0.0") {
  sample_id_column <- .publication_dapc_scalar(sample_id_column, "sample_id_column")
  population_column <- .publication_dapc_scalar(population_column, "population_column")
  cluster_column <- .publication_dapc_scalar(cluster_column, "cluster_column")
  axis_columns <- as.character(axis_columns)
  if (length(axis_columns) < 2L || anyNA(axis_columns) || any(!nzchar(axis_columns)) || anyDuplicated(axis_columns)) {
    stop("axis_columns must contain at least two unique non-empty names.", call. = FALSE)
  }
  if (!is.null(selected_k)) {
    selected_k <- as.integer(selected_k)
    if (length(selected_k) != 1L || is.na(selected_k) || selected_k < 2L) {
      stop("selected_k must be NULL or one integer of at least two.", call. = FALSE)
    }
  }
  source_data_format <- match.arg(source_data_format, c("tsv", "csv"))
  version <- .publication_dapc_scalar(version, "version")
  spec <- list(
    record_type = "popgenvcf_publication_dapc_spec", schema_version = "1.0.0",
    sample_id_column = sample_id_column, population_column = population_column,
    cluster_column = cluster_column, axis_columns = axis_columns,
    selected_k = selected_k, source_data_format = source_data_format, version = version
  )
  spec$fingerprint <- .publication_dapc_fingerprint(spec)
  class(spec) <- c("PopgenVCFPublicationDAPCSpec", "list")
  validate_publication_dapc_spec(spec)
  spec
}

#' Validate a publication DAPC specification
#' @param spec A publication DAPC specification.
#' @return `TRUE`, invisibly.
#' @export
validate_publication_dapc_spec <- function(spec) {
  if (!inherits(spec, "PopgenVCFPublicationDAPCSpec") || !identical(spec$schema_version, "1.0.0")) {
    stop("spec must be a supported publication DAPC specification.", call. = FALSE)
  }
  if (length(spec$axis_columns) < 2L || anyDuplicated(spec$axis_columns)) {
    stop("Malformed DAPC axes.", call. = FALSE)
  }
  if (!identical(spec$fingerprint, .publication_dapc_fingerprint(spec))) {
    stop("DAPC specification fingerprint mismatch.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Create deterministic publication DAPC source data
#'
#' @param spec A validated DAPC publication specification.
#' @param coordinates Authoritative discriminant coordinates.
#' @param membership Authoritative membership probability matrix or data frame.
#' @param diagnostics Optional authoritative per-K diagnostics.
#' @param cross_validation Optional authoritative cross-validation summary.
#' @param confusion Optional authoritative confusion matrix or long-form table.
#' @param result_fingerprint Fingerprint of the authoritative DAPC result.
#' @param figure_binding Optional publication figure-style binding.
#' @return A fingerprinted `PopgenVCFPublicationDAPCOutput`.
#' @export
new_publication_dapc_output <- function(
    spec, coordinates, membership, diagnostics = NULL, cross_validation = NULL,
    confusion = NULL, result_fingerprint, figure_binding = NULL) {
  validate_publication_dapc_spec(spec)
  result_fingerprint <- .publication_dapc_scalar(result_fingerprint, "result_fingerprint")
  coordinates <- .publication_dapc_table(coordinates, "coordinates")
  required <- c(spec$sample_id_column, spec$axis_columns)
  if (!all(required %in% names(coordinates))) stop("coordinates is missing required DAPC columns.", call. = FALSE)
  ids <- as.character(coordinates[[spec$sample_id_column]])
  if (anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids)) stop("DAPC sample identities must be non-empty and unique.", call. = FALSE)
  for (axis in spec$axis_columns) {
    coordinates[[axis]] <- as.numeric(coordinates[[axis]])
    if (any(!is.finite(coordinates[[axis]]))) stop("DAPC coordinates must be finite numeric values.", call. = FALSE)
  }
  coordinates <- coordinates[order(ids), , drop = FALSE]
  ids <- as.character(coordinates[[spec$sample_id_column]])

  membership <- as.data.frame(membership, stringsAsFactors = FALSE, check.names = FALSE)
  if (is.null(rownames(membership)) || any(!nzchar(rownames(membership))) || anyDuplicated(rownames(membership))) {
    stop("membership must have unique non-empty sample row names.", call. = FALSE)
  }
  if (!setequal(ids, rownames(membership))) stop("membership and coordinate sample identities do not match exactly.", call. = FALSE)
  membership[] <- lapply(membership, as.numeric)
  if (any(!vapply(membership, function(x) all(is.finite(x) & x >= 0 & x <= 1), logical(1L)))) {
    stop("membership probabilities must be finite values between zero and one.", call. = FALSE)
  }
  membership <- membership[ids, , drop = FALSE]
  row_sums <- rowSums(membership)
  if (any(abs(row_sums - 1) > 1e-8)) stop("membership probabilities must sum to one for each sample.", call. = FALSE)

  diagnostics <- .publication_dapc_table(diagnostics, "diagnostics")
  cross_validation <- .publication_dapc_table(cross_validation, "cross_validation")
  confusion <- if (is.matrix(confusion)) as.data.frame.matrix(confusion) else .publication_dapc_table(confusion, "confusion")
  if (!is.null(spec$selected_k) && ncol(membership) != spec$selected_k) {
    stop("membership column count does not match selected_k.", call. = FALSE)
  }
  groups <- sort(colnames(membership))
  if (!is.null(figure_binding)) {
    if (!inherits(figure_binding, "PopgenVCFPublicationFigureStyleBinding")) stop("figure_binding must be a publication figure-style binding.", call. = FALSE)
    if (length(groups) > figure_binding$groups) stop("Figure-style binding lacks capacity for DAPC membership groups.", call. = FALSE)
  }
  source_membership <- data.frame(sample_id = ids, membership, check.names = FALSE, stringsAsFactors = FALSE)
  output <- list(
    record_type = "popgenvcf_publication_dapc_output", schema_version = "1.0.0",
    specification_fingerprint = spec$fingerprint, result_fingerprint = result_fingerprint,
    figure_binding_fingerprint = if (is.null(figure_binding)) NULL else figure_binding$fingerprint,
    selected_k = spec$selected_k, coordinates = coordinates, membership = membership,
    diagnostics = diagnostics, cross_validation = cross_validation, confusion = confusion,
    groups = groups,
    source_data = list(
      coordinates = coordinates, membership = source_membership,
      diagnostics = diagnostics, cross_validation = cross_validation, confusion = confusion
    )
  )
  output$fingerprint <- .publication_dapc_fingerprint(output)
  class(output) <- c("PopgenVCFPublicationDAPCOutput", "list")
  validate_publication_dapc_output(output, spec)
  output
}

#' Validate a publication DAPC output
#' @param output A publication DAPC output.
#' @param spec Originating specification.
#' @return `TRUE`, invisibly.
#' @export
validate_publication_dapc_output <- function(output, spec) {
  validate_publication_dapc_spec(spec)
  if (!inherits(output, "PopgenVCFPublicationDAPCOutput") || !identical(output$specification_fingerprint, spec$fingerprint)) {
    stop("output is not bound to the supplied DAPC specification.", call. = FALSE)
  }
  expected_membership <- data.frame(
    sample_id = as.character(output$coordinates[[spec$sample_id_column]]),
    output$membership, check.names = FALSE, stringsAsFactors = FALSE
  )
  if (!identical(output$source_data$coordinates, output$coordinates) ||
      !identical(output$source_data$membership, expected_membership)) {
    stop("DAPC source data drifted from the publication output.", call. = FALSE)
  }
  if (!identical(output$fingerprint, .publication_dapc_fingerprint(output))) {
    stop("DAPC output fingerprint mismatch.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Create a deterministic publication DAPC caption
#' @param output A validated publication DAPC output.
#' @param spec Originating specification.
#' @return One manuscript-ready caption string.
#' @export
publication_dapc_caption <- function(output, spec) {
  validate_publication_dapc_output(output, spec)
  sprintf(
    "Discriminant analysis of principal components for %d samples across %d membership groups, shown on %s and %s%s.",
    nrow(output$coordinates), length(output$groups), spec$axis_columns[1], spec$axis_columns[2],
    if (is.null(output$selected_k)) "" else sprintf(" (selected K = %d)", output$selected_k)
  )
}

#' Render a deterministic publication DAPC report
#' @param output A publication DAPC output.
#' @param spec Originating specification.
#' @return Markdown report lines.
#' @export
publication_dapc_report <- function(output, spec) {
  validate_publication_dapc_output(output, spec)
  c(
    "# Publication DAPC output", "",
    sprintf("- Samples: `%d`", nrow(output$coordinates)),
    sprintf("- Membership groups: `%d`", length(output$groups)),
    sprintf("- Axes: `%s`", paste(spec$axis_columns, collapse = ", ")),
    sprintf("- Selected K: `%s`", if (is.null(output$selected_k)) "not specified" else output$selected_k),
    sprintf("- Result fingerprint: `%s`", output$result_fingerprint),
    sprintf("- Output fingerprint: `%s`", output$fingerprint)
  )
}
