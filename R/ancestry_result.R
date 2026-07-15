#' Create a canonical ancestry replicate result
#'
#' @param sample_ids Unique sample identifiers in Q-matrix row order.
#' @param q Numeric ancestry coefficient matrix with one row per sample and
#'   one column per inferred cluster.
#' @param backend Analysis backend: `admixture`, `faststructure`, or `snmf`.
#' @param k Number of inferred clusters.
#' @param replicate Positive replicate identifier.
#' @param seed Integer random seed, or `NA_integer_` when unavailable.
#' @param metrics Named numeric fit metrics such as cross-validation error,
#'   marginal likelihood, or cross-entropy. An empty numeric vector is valid
#'   when a backend does not report a fit metric.
#' @param converged Logical convergence status, or `NA` when unavailable.
#' @param runtime_seconds Nonnegative runtime in seconds, or `NA_real_`.
#' @param provenance Named list of executable, version, command, input, and
#'   environment provenance.
#' @param tolerance Allowed absolute deviation of Q-matrix row sums from one.
#' @return A validated `PopgenVCFAncestryReplicate` object.
#' @export
new_ancestry_replicate <- function(sample_ids, q, backend, k = ncol(q),
                                   replicate = 1L, seed = NA_integer_,
                                   metrics = numeric(), converged = NA,
                                   runtime_seconds = NA_real_,
                                   provenance = list(), tolerance = 1e-6) {
  q <- as.matrix(q)
  storage.mode(q) <- "double"
  x <- structure(list(
    sample_ids = as.character(sample_ids),
    q = q,
    backend = tolower(as.character(backend)[1L]),
    k = as.integer(k)[1L],
    replicate = as.integer(replicate)[1L],
    seed = as.integer(seed)[1L],
    metrics = metrics,
    converged = as.logical(converged)[1L],
    runtime_seconds = as.numeric(runtime_seconds)[1L],
    provenance = provenance
  ), class = "PopgenVCFAncestryReplicate")
  validate_ancestry_replicate(x, tolerance = tolerance)
}

#' Validate a canonical ancestry replicate result
#' @param x A `PopgenVCFAncestryReplicate` object.
#' @param tolerance Allowed absolute deviation of row sums from one.
#' @return `x`, invisibly, when valid.
#' @export
validate_ancestry_replicate <- function(x, tolerance = 1e-6) {
  if (!inherits(x, "PopgenVCFAncestryReplicate")) stop("x must be a PopgenVCFAncestryReplicate", call. = FALSE)
  if (!x$backend %in% c("admixture", "faststructure", "snmf")) stop("unsupported ancestry backend", call. = FALSE)
  if (length(x$sample_ids) != nrow(x$q)) stop("sample_ids length must equal Q-matrix rows", call. = FALSE)
  if (!length(x$sample_ids) || anyNA(x$sample_ids) || any(!nzchar(x$sample_ids)) || anyDuplicated(x$sample_ids)) stop("sample_ids must be unique non-empty values", call. = FALSE)
  if (!is.matrix(x$q) || !is.numeric(x$q) || any(!is.finite(x$q))) stop("q must be a finite numeric matrix", call. = FALSE)
  if (ncol(x$q) != x$k || x$k < 1L) stop("k must equal the number of Q-matrix columns", call. = FALSE)
  if (any(x$q < -tolerance) || any(x$q > 1 + tolerance)) stop("Q-matrix entries must lie in [0, 1]", call. = FALSE)
  if (any(abs(rowSums(x$q) - 1) > tolerance)) stop("Q-matrix rows must sum to one", call. = FALSE)
  if (is.na(x$replicate) || x$replicate < 1L) stop("replicate must be a positive integer", call. = FALSE)
  if (!is.numeric(x$metrics) || any(!is.finite(x$metrics))) stop("metrics must be a finite numeric vector", call. = FALSE)
  if (length(x$metrics) && (is.null(names(x$metrics)) || any(!nzchar(names(x$metrics))))) stop("non-empty metrics must be named", call. = FALSE)
  if (!is.na(x$runtime_seconds) && (!is.finite(x$runtime_seconds) || x$runtime_seconds < 0)) stop("runtime_seconds must be nonnegative or NA", call. = FALSE)
  if (!is.list(x$provenance)) stop("provenance must be a list", call. = FALSE)
  invisible(x)
}

#' Create an ancestry result collection
#' @param replicates List of `PopgenVCFAncestryReplicate` objects.
#' @return A validated `PopgenVCFAncestryResult` object.
#' @export
new_ancestry_result <- function(replicates) {
  x <- structure(list(replicates = unname(replicates)), class = "PopgenVCFAncestryResult")
  validate_ancestry_result(x)
}

#' Validate an ancestry result collection
#' @param x A `PopgenVCFAncestryResult` object.
#' @return `x`, invisibly, when valid.
#' @export
validate_ancestry_result <- function(x) {
  if (!inherits(x, "PopgenVCFAncestryResult")) stop("x must be a PopgenVCFAncestryResult", call. = FALSE)
  if (!length(x$replicates)) stop("ancestry result must contain at least one replicate", call. = FALSE)
  invisible(lapply(x$replicates, validate_ancestry_replicate))
  ids <- vapply(x$replicates, function(z) paste(z$backend, z$k, z$replicate, sep = "::"), character(1L))
  if (anyDuplicated(ids)) stop("backend, K, and replicate combinations must be unique", call. = FALSE)
  sample_ids <- lapply(x$replicates, `[[`, "sample_ids")
  if (!all(vapply(sample_ids[-1L], identical, logical(1L), sample_ids[[1L]]))) stop("all ancestry replicates must use identical sample IDs and order", call. = FALSE)
  invisible(x)
}

#' Convert ancestry replicates to a summary table
#' @param x A replicate or ancestry result collection.
#' @return A `data.table` with one row per replicate and metric.
#' @export
ancestry_result_table <- function(x) {
  reps <- if (inherits(x, "PopgenVCFAncestryReplicate")) list(x) else {
    validate_ancestry_result(x)
    x$replicates
  }
  rows <- lapply(reps, function(z) {
    base <- data.table::data.table(backend = z$backend, k = z$k, replicate = z$replicate,
      seed = z$seed, converged = z$converged, runtime_seconds = z$runtime_seconds,
      metric = NA_character_, value = NA_real_)
    if (!length(z$metrics)) return(base)
    data.table::data.table(backend = z$backend, k = z$k, replicate = z$replicate,
      seed = z$seed, converged = z$converged, runtime_seconds = z$runtime_seconds,
      metric = names(z$metrics), value = unname(z$metrics))
  })
  data.table::rbindlist(rows, use.names = TRUE)
}

#' Convert an ancestry Q matrix to long-form source data
#' @param x A `PopgenVCFAncestryReplicate` object.
#' @return A `data.table` with sample, cluster, and ancestry coefficient.
#' @export
ancestry_q_table <- function(x) {
  validate_ancestry_replicate(x)
  out <- data.table::as.data.table(x$q)
  data.table::setnames(out, paste0("cluster_", seq_len(x$k)))
  out[, sample_id := x$sample_ids]
  data.table::melt(out, id.vars = "sample_id", variable.name = "cluster", value.name = "ancestry")
}

#' @export
print.PopgenVCFAncestryReplicate <- function(x, ...) {
  cat("<PopgenVCFAncestryReplicate>", x$backend, "K=", x$k, "replicate=", x$replicate, "samples=", nrow(x$q), "\n")
  invisible(x)
}

#' @export
print.PopgenVCFAncestryResult <- function(x, ...) {
  tab <- ancestry_result_table(x)
  cat("<PopgenVCFAncestryResult>", length(x$replicates), "replicates; backends:", paste(unique(tab$backend), collapse = ", "), "\n")
  invisible(x)
}
