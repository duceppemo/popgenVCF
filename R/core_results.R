#' Create a canonical core analysis result
#'
#' @param analysis Stable analysis identifier.
#' @param payload Named list containing the analysis-specific statistical result.
#' @param parameters Named list of analysis parameters.
#' @param provenance Named list describing software, inputs, commands, and runtime.
#' @param metadata Optional sample or population metadata.
#' @param validation Optional validation table containing `check` and `passed`.
#' @param artifacts Optional `PopgenVCFArtifactManifest`.
#' @param class_name Analysis-specific S3 class name.
#' @return A validated canonical result object.
#' @export
new_core_result <- function(analysis, payload, parameters = list(), provenance = list(),
                            metadata = NULL, validation = NULL,
                            artifacts = new_artifact_manifest(),
                            class_name = NULL) {
  scalar_string <- function(x, label) {
    if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
      stop(label, " must be one non-empty string", call. = FALSE)
    }
    x
  }
  analysis <- tolower(scalar_string(analysis, "analysis"))
  if (!is.list(payload) || is.null(names(payload)) || any(!nzchar(names(payload)))) {
    stop("payload must be a named list", call. = FALSE)
  }
  if (!is.list(parameters) || (length(parameters) && is.null(names(parameters)))) {
    stop("parameters must be a named list", call. = FALSE)
  }
  if (!is.list(provenance) || (length(provenance) && is.null(names(provenance)))) {
    stop("provenance must be a named list", call. = FALSE)
  }
  if (!is.null(metadata) && !is.data.frame(metadata)) {
    stop("metadata must be a data frame or NULL", call. = FALSE)
  }
  if (is.null(validation)) {
    validation <- data.table::data.table(check = "object_schema", passed = TRUE)
  } else {
    validation <- data.table::as.data.table(validation)
    if (!all(c("check", "passed") %in% names(validation))) {
      stop("validation must contain check and passed columns", call. = FALSE)
    }
    validation[, check := as.character(check)]
    validation[, passed := as.logical(passed)]
  }
  validate_artifact_manifest(artifacts)
  class_name <- class_name %||% paste0("PopgenVCF", toupper(substring(analysis, 1, 1)), substring(analysis, 2), "Result")
  x <- structure(list(
    schema_version = "1.0",
    analysis = analysis,
    payload = payload,
    parameters = parameters,
    provenance = provenance,
    metadata = metadata,
    validation = validation,
    artifacts = artifacts
  ), class = c(class_name, "PopgenVCFCoreResult"))
  validate_core_result(x)
}

#' Validate a canonical core analysis result
#' @param x A `PopgenVCFCoreResult`.
#' @return `x`, invisibly, when valid.
#' @export
validate_core_result <- function(x) {
  if (!inherits(x, "PopgenVCFCoreResult")) stop("x must inherit from PopgenVCFCoreResult", call. = FALSE)
  required <- c("schema_version", "analysis", "payload", "parameters", "provenance", "metadata", "validation", "artifacts")
  missing <- setdiff(required, names(x))
  if (length(missing)) stop("core result is missing field(s): ", paste(missing, collapse = ", "), call. = FALSE)
  if (!identical(x$schema_version, "1.0")) stop("unsupported core result schema version", call. = FALSE)
  if (!is.character(x$analysis) || length(x$analysis) != 1L || !nzchar(x$analysis)) stop("analysis is invalid", call. = FALSE)
  if (!is.list(x$payload) || is.null(names(x$payload)) || any(!nzchar(names(x$payload)))) stop("payload must be a named list", call. = FALSE)
  if (!is.list(x$parameters) || (length(x$parameters) && is.null(names(x$parameters)))) stop("parameters must be a named list", call. = FALSE)
  if (!is.list(x$provenance) || (length(x$provenance) && is.null(names(x$provenance)))) stop("provenance must be a named list", call. = FALSE)
  if (!is.null(x$metadata) && !is.data.frame(x$metadata)) stop("metadata must be a data frame or NULL", call. = FALSE)
  if (!is.data.frame(x$validation) || !all(c("check", "passed") %in% names(x$validation))) stop("validation table is invalid", call. = FALSE)
  if (anyNA(x$validation$passed) || !all(x$validation$passed)) stop("core result validation contains failed checks", call. = FALSE)
  validate_artifact_manifest(x$artifacts)
  validate_core_payload(x)
  invisible(x)
}

validate_core_payload <- function(x) {
  p <- x$payload
  finite_matrix <- function(z, label, symmetric = FALSE) {
    if (!is.matrix(z) || !is.numeric(z) || any(!is.finite(z))) stop(label, " must be a finite numeric matrix", call. = FALSE)
    if (symmetric && (!identical(dim(z), rev(dim(z))) || max(abs(z - t(z))) > 1e-8)) stop(label, " must be symmetric", call. = FALSE)
  }
  switch(x$analysis,
    pca = {
      if (!all(c("coordinates", "eigenvalues") %in% names(p))) stop("PCA payload requires coordinates and eigenvalues", call. = FALSE)
      if (!is.data.frame(p$coordinates) || !"sample_id" %in% names(p$coordinates)) stop("PCA coordinates must contain sample_id", call. = FALSE)
      if (!is.numeric(p$eigenvalues) || !length(p$eigenvalues) || any(!is.finite(p$eigenvalues)) || any(p$eigenvalues < 0)) stop("PCA eigenvalues are invalid", call. = FALSE)
    },
    ibs = {
      finite_matrix(p$similarity, "IBS similarity", TRUE)
      finite_matrix(p$distance, "IBS distance", TRUE)
      if (!identical(dim(p$similarity), dim(p$distance))) stop("IBS matrices must have identical dimensions", call. = FALSE)
    },
    tree = {
      if (is.null(p$tree)) stop("tree payload requires tree", call. = FALSE)
    },
    diversity = {
      if (!is.data.frame(p$statistics) || !nrow(p$statistics)) stop("diversity statistics must be a non-empty data frame", call. = FALSE)
    },
    fst = {
      if (!is.numeric(p$global_fst) || length(p$global_fst) != 1L || !is.finite(p$global_fst)) stop("global_fst must be finite", call. = FALSE)
      if (!is.data.frame(p$pairwise)) stop("pairwise FST must be a data frame", call. = FALSE)
    },
    amova = {
      if (!is.data.frame(p$components) || !nrow(p$components)) stop("AMOVA components must be a non-empty data frame", call. = FALSE)
    },
    dapc = {
      if (!is.data.frame(p$coordinates) || !"sample_id" %in% names(p$coordinates)) stop("DAPC coordinates must contain sample_id", call. = FALSE)
    },
    ibd = {
      if (!is.data.frame(p$distances) || !all(c("genetic_distance", "geographic_distance") %in% names(p$distances))) stop("IBD distances require genetic_distance and geographic_distance", call. = FALSE)
      if (!is.numeric(p$mantel_statistic) || length(p$mantel_statistic) != 1L || !is.finite(p$mantel_statistic)) stop("mantel_statistic must be finite", call. = FALSE)
    },
    invisible(NULL)
  )
}

#' Typed canonical result constructors
#' @param ... Passed to `new_core_result()` after analysis-specific payload creation.
#' @export
new_pca_result <- function(coordinates, eigenvalues, ...) new_core_result("pca", list(coordinates = coordinates, eigenvalues = eigenvalues), ..., class_name = "PopgenVCFPCAResult")
#' @export
new_ibs_result <- function(similarity, distance = 1 - similarity, mds = NULL, ...) new_core_result("ibs", list(similarity = similarity, distance = distance, mds = mds), ..., class_name = "PopgenVCFIBSResult")
#' @export
new_tree_result <- function(tree, distances = NULL, ...) new_core_result("tree", list(tree = tree, distances = distances), ..., class_name = "PopgenVCFTreeResult")
#' @export
new_diversity_result <- function(statistics, ...) new_core_result("diversity", list(statistics = statistics), ..., class_name = "PopgenVCFDiversityResult")
#' @export
new_fst_result <- function(global_fst, pairwise, confidence_intervals = NULL, ...) new_core_result("fst", list(global_fst = global_fst, pairwise = pairwise, confidence_intervals = confidence_intervals), ..., class_name = "PopgenVCFFSTResult")
#' @export
new_amova_result <- function(components, statistics = NULL, ...) new_core_result("amova", list(components = components, statistics = statistics), ..., class_name = "PopgenVCFAMOVAResult")
#' @export
new_dapc_result <- function(coordinates, assignments = NULL, discriminant = NULL, ...) new_core_result("dapc", list(coordinates = coordinates, assignments = assignments, discriminant = discriminant), ..., class_name = "PopgenVCFDAPCResult")
#' @export
new_ibd_result <- function(distances, mantel_statistic, p_value = NA_real_, permutations = NA_integer_, ...) new_core_result("ibd", list(distances = distances, mantel_statistic = mantel_statistic, p_value = p_value, permutations = permutations), ..., class_name = "PopgenVCFIBDResult")

#' Adapt a legacy module output to a canonical result
#' @param analysis Analysis identifier.
#' @param x Existing module output.
#' @param parameters,provenance,metadata,validation,artifacts Canonical result metadata.
#' @return A typed canonical result.
#' @export
as_core_result <- function(analysis, x, parameters = list(), provenance = list(), metadata = NULL,
                           validation = NULL, artifacts = new_artifact_manifest()) {
  analysis <- tolower(analysis)
  common <- list(parameters = parameters, provenance = provenance, metadata = metadata,
                 validation = validation, artifacts = artifacts)
  do.call(switch(analysis,
    pca = new_pca_result,
    ibs = new_ibs_result,
    tree = new_tree_result,
    diversity = new_diversity_result,
    fst = new_fst_result,
    amova = new_amova_result,
    dapc = new_dapc_result,
    ibd = new_ibd_result,
    stop("unsupported core analysis: ", analysis, call. = FALSE)
  ), c(if (is.list(x)) x else list(x), common))
}

#' Extract the primary tabular representation of a core result
#' @param x A `PopgenVCFCoreResult`.
#' @return A data table.
#' @export
core_result_table <- function(x) {
  validate_core_result(x)
  p <- x$payload
  out <- switch(x$analysis,
    pca = data.table::as.data.table(p$coordinates),
    ibs = data.table::as.data.table(as.table(p$similarity))[, .(sample_1 = Var1, sample_2 = Var2, similarity = N)],
    tree = data.table::data.table(newick = if (requireNamespace("ape", quietly = TRUE) && inherits(p$tree, "phylo")) ape::write.tree(p$tree) else as.character(p$tree)),
    diversity = data.table::as.data.table(p$statistics),
    fst = data.table::as.data.table(p$pairwise),
    amova = data.table::as.data.table(p$components),
    dapc = data.table::as.data.table(p$coordinates),
    ibd = data.table::as.data.table(p$distances),
    data.table::data.table()
  )
  out
}

#' Save and read canonical core results
#' @param x A `PopgenVCFCoreResult`.
#' @param path File path.
#' @return `path` invisibly for save, or a validated result for read.
#' @export
save_core_result <- function(x, path) {
  validate_core_result(x)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(x, path, version = 3)
  invisible(path)
}
#' @export
read_core_result <- function(path) {
  if (!file.exists(path)) stop("core result file does not exist: ", path, call. = FALSE)
  x <- readRDS(path)
  validate_core_result(x)
  x
}

#' @export
print.PopgenVCFCoreResult <- function(x, ...) {
  cat("<", class(x)[1L], "> analysis=", x$analysis,
      " payload=", paste(names(x$payload), collapse = ","), "\n", sep = "")
  invisible(x)
}
