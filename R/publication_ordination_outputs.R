# Phase 0.9.5 - deterministic publication PCA and ordination outputs

.publication_ordination_fingerprint <- function(x) {
  candidate <- unclass(x)
  candidate$fingerprint <- NULL
  digest::digest(candidate, algo = "sha256", serialize = TRUE)
}

.publication_ordination_scalar <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    stop(sprintf("%s must be one non-empty character value.", name), call. = FALSE)
  }
  trimws(x)
}

.publication_ordination_coordinates <- function(coordinates, sample_id_column, axis_columns) {
  if (!is.data.frame(coordinates)) stop("coordinates must be a data frame.", call. = FALSE)
  if (!sample_id_column %in% names(coordinates)) stop("coordinates is missing the sample identity column.", call. = FALSE)
  if (!all(axis_columns %in% names(coordinates))) stop("coordinates is missing one or more requested axes.", call. = FALSE)
  ids <- as.character(coordinates[[sample_id_column]])
  if (anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids)) stop("sample identities must be non-empty and unique.", call. = FALSE)
  values <- as.data.frame(coordinates[, axis_columns, drop = FALSE], stringsAsFactors = FALSE)
  values[] <- lapply(values, as.numeric)
  if (any(!vapply(values, function(x) all(is.finite(x)), logical(1L)))) stop("ordination coordinates must be finite numeric values.", call. = FALSE)
  out <- data.frame(sample_id = ids, values, check.names = FALSE, stringsAsFactors = FALSE)
  out[order(out$sample_id), , drop = FALSE]
}

#' Create a publication PCA or ordination specification
#'
#' @param method Stable method name, such as `pca`, `pcoa`, or `nmds`.
#' @param axis_columns Ordered coordinate columns to publish.
#' @param sample_id_column Coordinate sample-identity column.
#' @param group_column Optional metadata grouping column.
#' @param region One of `none`, `centroid`, `ellipse`, or `hull`.
#' @param labels Whether sample labels are requested.
#' @param source_data_format Stable machine-readable source-data format.
#' @param version Specification version.
#' @return A fingerprinted `PopgenVCFPublicationOrdinationSpec`.
#' @export
new_publication_ordination_spec <- function(
    method = "pca", axis_columns = c("PC1", "PC2"), sample_id_column = "sample_id",
    group_column = NULL, region = c("none", "centroid", "ellipse", "hull"),
    labels = FALSE, source_data_format = "tsv", version = "1.0.0") {
  method <- .publication_ordination_scalar(method, "method")
  sample_id_column <- .publication_ordination_scalar(sample_id_column, "sample_id_column")
  axis_columns <- as.character(axis_columns)
  if (length(axis_columns) < 2L || anyNA(axis_columns) || any(!nzchar(axis_columns)) || anyDuplicated(axis_columns)) {
    stop("axis_columns must contain at least two unique non-empty names.", call. = FALSE)
  }
  if (!is.null(group_column)) group_column <- .publication_ordination_scalar(group_column, "group_column")
  region <- match.arg(region)
  if (!is.logical(labels) || length(labels) != 1L || is.na(labels)) stop("labels must be one logical value.", call. = FALSE)
  source_data_format <- match.arg(source_data_format, c("tsv", "csv"))
  version <- .publication_ordination_scalar(version, "version")
  spec <- list(
    record_type = "popgenvcf_publication_ordination_spec", schema_version = "1.0.0",
    method = method, axis_columns = axis_columns, sample_id_column = sample_id_column,
    group_column = group_column, region = region, labels = labels,
    source_data_format = source_data_format, version = version
  )
  spec$fingerprint <- .publication_ordination_fingerprint(spec)
  class(spec) <- c("PopgenVCFPublicationOrdinationSpec", "list")
  validate_publication_ordination_spec(spec)
  spec
}

#' Validate a publication ordination specification
#' @param spec A publication ordination specification.
#' @return `TRUE`, invisibly.
#' @export
validate_publication_ordination_spec <- function(spec) {
  if (!inherits(spec, "PopgenVCFPublicationOrdinationSpec") || !identical(spec$schema_version, "1.0.0")) {
    stop("spec must be a supported publication ordination specification.", call. = FALSE)
  }
  .publication_ordination_scalar(spec$method, "method")
  if (length(spec$axis_columns) < 2L || anyDuplicated(spec$axis_columns)) stop("Malformed ordination axes.", call. = FALSE)
  if (!identical(spec$fingerprint, .publication_ordination_fingerprint(spec))) stop("Ordination specification fingerprint mismatch.", call. = FALSE)
  invisible(TRUE)
}

#' Create deterministic publication ordination source data
#'
#' @param spec A validated ordination specification.
#' @param coordinates Authoritative PCA or ordination coordinates.
#' @param metadata Optional sample metadata.
#' @param variance_explained Optional numeric variance proportions or percentages.
#' @param loadings Optional loading matrix or data frame.
#' @param result_fingerprint Fingerprint of the authoritative scientific result.
#' @param figure_binding Optional validated publication figure-style binding.
#' @return A fingerprinted publication ordination output manifest.
#' @export
new_publication_ordination_output <- function(
    spec, coordinates, metadata = NULL, variance_explained = NULL, loadings = NULL,
    result_fingerprint, figure_binding = NULL) {
  validate_publication_ordination_spec(spec)
  result_fingerprint <- .publication_ordination_scalar(result_fingerprint, "result_fingerprint")
  scores <- .publication_ordination_coordinates(coordinates, spec$sample_id_column, spec$axis_columns)
  if (!is.null(metadata)) {
    if (!is.data.frame(metadata) || !spec$sample_id_column %in% names(metadata)) stop("metadata is missing the sample identity column.", call. = FALSE)
    ids <- as.character(metadata[[spec$sample_id_column]])
    if (anyNA(ids) || anyDuplicated(ids)) stop("metadata sample identities must be unique and non-missing.", call. = FALSE)
    missing <- setdiff(scores$sample_id, ids)
    extra <- setdiff(ids, scores$sample_id)
    if (length(missing) || length(extra)) stop("metadata and coordinate sample identities do not match exactly.", call. = FALSE)
    keep <- setdiff(names(metadata), spec$sample_id_column)
    metadata <- data.frame(sample_id = ids, metadata[, keep, drop = FALSE], check.names = FALSE, stringsAsFactors = FALSE)
    metadata <- metadata[match(scores$sample_id, metadata$sample_id), , drop = FALSE]
    scores <- cbind(scores, metadata[, keep, drop = FALSE])
  }
  if (!is.null(spec$group_column) && !spec$group_column %in% names(scores)) stop("group_column is absent after metadata alignment.", call. = FALSE)
  groups <- if (is.null(spec$group_column)) character() else sort(unique(as.character(scores[[spec$group_column]])))
  groups <- groups[!is.na(groups) & nzchar(groups)]
  if (!is.null(figure_binding)) {
    if (!inherits(figure_binding, "PopgenVCFPublicationFigureStyleBinding")) stop("figure_binding must be a publication figure-style binding.", call. = FALSE)
    if (length(groups) > figure_binding$groups) stop("Figure-style binding lacks capacity for the aligned groups.", call. = FALSE)
  }
  variance <- NULL
  if (!is.null(variance_explained)) {
    variance_explained <- as.numeric(variance_explained)
    if (length(variance_explained) < length(spec$axis_columns) || any(!is.finite(variance_explained)) || any(variance_explained < 0)) {
      stop("variance_explained must contain finite nonnegative values for all published axes.", call. = FALSE)
    }
    if (sum(variance_explained) <= 1 + 1e-8) variance_explained <- variance_explained * 100
    if (sum(variance_explained) > 100 + 1e-6) stop("variance_explained cannot exceed 100 percent.", call. = FALSE)
    variance <- data.frame(axis = spec$axis_columns, variance_percent = variance_explained[seq_along(spec$axis_columns)], stringsAsFactors = FALSE)
  }
  normalized_loadings <- NULL
  if (!is.null(loadings)) {
    normalized_loadings <- as.data.frame(loadings, stringsAsFactors = FALSE)
    if (!all(spec$axis_columns %in% names(normalized_loadings))) stop("loadings is missing one or more published axes.", call. = FALSE)
    if (any(!vapply(normalized_loadings[, spec$axis_columns, drop = FALSE], function(x) all(is.finite(as.numeric(x))), logical(1L)))) stop("loadings must contain finite numeric values.", call. = FALSE)
  }
  centroids <- NULL
  if (length(groups)) {
    centroids <- do.call(rbind, lapply(groups, function(group) {
      rows <- scores[[spec$group_column]] == group
      data.frame(group = group, as.list(vapply(scores[rows, spec$axis_columns, drop = FALSE], mean, numeric(1L))), check.names = FALSE)
    }))
    rownames(centroids) <- NULL
  }
  output <- list(
    record_type = "popgenvcf_publication_ordination_output", schema_version = "1.0.0",
    specification_fingerprint = spec$fingerprint, result_fingerprint = result_fingerprint,
    figure_binding_fingerprint = if (is.null(figure_binding)) NULL else figure_binding$fingerprint,
    scores = scores, variance = variance, loadings = normalized_loadings,
    centroids = centroids, groups = groups,
    source_data = list(scores = scores, variance = variance, loadings = normalized_loadings, centroids = centroids)
  )
  output$fingerprint <- .publication_ordination_fingerprint(output)
  class(output) <- c("PopgenVCFPublicationOrdinationOutput", "list")
  validate_publication_ordination_output(output, spec)
  output
}

#' Validate a publication ordination output
#' @param output A publication ordination output.
#' @param spec Originating specification.
#' @return `TRUE`, invisibly.
#' @export
validate_publication_ordination_output <- function(output, spec) {
  validate_publication_ordination_spec(spec)
  if (!inherits(output, "PopgenVCFPublicationOrdinationOutput") || !identical(output$specification_fingerprint, spec$fingerprint)) stop("output is not bound to the supplied ordination specification.", call. = FALSE)
  .publication_ordination_coordinates(output$scores, "sample_id", spec$axis_columns)
  if (!identical(output$source_data$scores, output$scores)) stop("Ordination source-data scores drifted from the publication output.", call. = FALSE)
  if (!identical(output$fingerprint, .publication_ordination_fingerprint(output))) stop("Ordination output fingerprint mismatch.", call. = FALSE)
  invisible(TRUE)
}

#' Create a deterministic ordination caption
#' @param output A validated publication ordination output.
#' @param spec Originating specification.
#' @return One manuscript-ready caption string.
#' @export
publication_ordination_caption <- function(output, spec) {
  validate_publication_ordination_output(output, spec)
  axes <- spec$axis_columns[1:2]
  labels <- axes
  if (!is.null(output$variance)) {
    pct <- output$variance$variance_percent[match(axes, output$variance$axis)]
    labels <- sprintf("%s (%.2f%%)", axes, pct)
  }
  sprintf("%s ordination of %d samples shown on %s and %s%s.", toupper(spec$method), nrow(output$scores), labels[1], labels[2], if (length(output$groups)) sprintf(", grouped into %d categories", length(output$groups)) else "")
}

#' Render a deterministic ordination output report
#' @param output A publication ordination output.
#' @param spec Originating specification.
#' @return Markdown report lines.
#' @export
publication_ordination_report <- function(output, spec) {
  validate_publication_ordination_output(output, spec)
  c("# Publication ordination output", "", sprintf("- Method: `%s`", spec$method), sprintf("- Samples: `%d`", nrow(output$scores)), sprintf("- Axes: `%s`", paste(spec$axis_columns, collapse = ", ")), sprintf("- Groups: `%d`", length(output$groups)), sprintf("- Result fingerprint: `%s`", output$result_fingerprint), sprintf("- Output fingerprint: `%s`", output$fingerprint))
}
