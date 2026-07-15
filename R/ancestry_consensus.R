#' Build a consensus ancestry estimate from replicate Q matrices
#'
#' Replicates are aligned to a deterministic reference before calculating mean
#' and median ancestry coefficients, uncertainty intervals, and stability
#' diagnostics. All replicates must use the same backend, K, sample identifiers,
#' and sample order.
#'
#' @param x A `PopgenVCFAncestryResult` or list of
#'   `PopgenVCFAncestryReplicate` objects.
#' @param confidence Confidence level for empirical per-cell intervals.
#' @param reference_replicate Optional replicate identifier to use as the
#'   alignment reference. By default the smallest replicate identifier is used.
#' @param tolerance Numerical tolerance used by ancestry validation and
#'   alignment.
#' @return A validated `PopgenVCFAncestryConsensus` object.
#' @export
consensus_ancestry <- function(x, confidence = 0.95,
                               reference_replicate = NULL,
                               tolerance = 1e-6) {
  reps <- if (inherits(x, "PopgenVCFAncestryResult")) {
    validate_ancestry_result(x)
    x$replicates
  } else {
    x
  }
  if (!is.list(reps) || !length(reps)) {
    stop("x must contain at least one ancestry replicate", call. = FALSE)
  }
  invisible(lapply(reps, validate_ancestry_replicate, tolerance = tolerance))
  if (!is.numeric(confidence) || length(confidence) != 1L ||
      !is.finite(confidence) || confidence <= 0 || confidence >= 1) {
    stop("confidence must be a single value strictly between zero and one", call. = FALSE)
  }

  backends <- vapply(reps, `[[`, character(1L), "backend")
  ks <- vapply(reps, `[[`, integer(1L), "k")
  if (length(unique(backends)) != 1L) {
    stop("all consensus replicates must use the same backend", call. = FALSE)
  }
  if (length(unique(ks)) != 1L) {
    stop("all consensus replicates must use the same K", call. = FALSE)
  }
  sample_ids <- lapply(reps, `[[`, "sample_ids")
  if (!all(vapply(sample_ids[-1L], identical, logical(1L), sample_ids[[1L]]))) {
    stop("all consensus replicates must use identical sample IDs and order", call. = FALSE)
  }

  replicate_ids <- vapply(reps, `[[`, integer(1L), "replicate")
  order_idx <- order(replicate_ids)
  reps <- reps[order_idx]
  replicate_ids <- replicate_ids[order_idx]
  if (is.null(reference_replicate)) {
    reference_idx <- 1L
  } else {
    reference_idx <- match(as.integer(reference_replicate)[1L], replicate_ids)
    if (is.na(reference_idx)) {
      stop("reference_replicate is not present in x", call. = FALSE)
    }
  }
  reference <- reps[[reference_idx]]

  alignments <- lapply(reps, function(rep) {
    if (identical(rep$replicate, reference$replicate)) {
      align_ancestry_replicate(reference, reference, tolerance = tolerance)
    } else {
      align_ancestry_replicate(rep, reference, tolerance = tolerance)
    }
  })
  aligned <- lapply(alignments, `[[`, "aligned_q")
  n <- nrow(reference$q)
  k <- reference$k
  r <- length(aligned)
  q_array <- array(unlist(aligned, use.names = FALSE), dim = c(n, k, r))

  mean_q <- apply(q_array, c(1L, 2L), mean)
  median_q <- apply(q_array, c(1L, 2L), stats::median)
  variance_q <- if (r == 1L) matrix(0, n, k) else apply(q_array, c(1L, 2L), stats::var)
  sd_q <- sqrt(variance_q)
  alpha <- (1 - confidence) / 2
  lower_q <- apply(q_array, c(1L, 2L), stats::quantile,
                   probs = alpha, names = FALSE, type = 8)
  upper_q <- apply(q_array, c(1L, 2L), stats::quantile,
                   probs = 1 - alpha, names = FALSE, type = 8)

  rownames(mean_q) <- rownames(median_q) <- rownames(variance_q) <-
    rownames(sd_q) <- rownames(lower_q) <- rownames(upper_q) <- reference$sample_ids
  cluster_names <- paste0("cluster_", seq_len(k))
  colnames(mean_q) <- colnames(median_q) <- colnames(variance_q) <-
    colnames(sd_q) <- colnames(lower_q) <- colnames(upper_q) <- cluster_names

  alignment_table <- data.table::rbindlist(lapply(seq_along(reps), function(i) {
    data.table::data.table(
      backend = reps[[i]]$backend,
      k = reps[[i]]$k,
      replicate = reps[[i]]$replicate,
      reference_replicate = reference$replicate,
      alignment_score = alignments[[i]]$alignment_score,
      correlation_score = alignments[[i]]$correlation_score,
      cosine_score = alignments[[i]]$cosine_score,
      rmsd = alignments[[i]]$rmsd
    )
  }))
  cluster_stability <- data.table::data.table(
    cluster = cluster_names,
    mean_sd = colMeans(sd_q),
    max_sd = apply(sd_q, 2L, max),
    stability = pmax(0, 1 - colMeans(sd_q))
  )
  sample_uncertainty <- data.table::data.table(
    sample_id = reference$sample_ids,
    mean_sd = rowMeans(sd_q),
    max_sd = apply(sd_q, 1L, max),
    uncertainty = rowMeans(sd_q)
  )

  out <- structure(list(
    backend = reference$backend,
    k = k,
    sample_ids = reference$sample_ids,
    reference_replicate = reference$replicate,
    replicate_ids = replicate_ids,
    confidence = confidence,
    mean_q = mean_q,
    median_q = median_q,
    variance_q = variance_q,
    sd_q = sd_q,
    lower_q = lower_q,
    upper_q = upper_q,
    aligned_replicates = lapply(alignments, `[[`, "aligned_replicate"),
    alignments = alignments,
    alignment_table = alignment_table,
    cluster_stability = cluster_stability,
    sample_uncertainty = sample_uncertainty,
    global_stability = max(0, 1 - mean(sd_q))
  ), class = "PopgenVCFAncestryConsensus")
  validate_ancestry_consensus(out, tolerance = tolerance)
}

#' Validate a consensus ancestry result
#' @param x A `PopgenVCFAncestryConsensus` object.
#' @param tolerance Numerical tolerance for simplex checks.
#' @return `x`, invisibly, when valid.
#' @export
validate_ancestry_consensus <- function(x, tolerance = 1e-6) {
  if (!inherits(x, "PopgenVCFAncestryConsensus")) {
    stop("x must be a PopgenVCFAncestryConsensus", call. = FALSE)
  }
  matrices <- c("mean_q", "median_q", "variance_q", "sd_q", "lower_q", "upper_q")
  dims <- lapply(matrices, function(nm) dim(x[[nm]]))
  if (!all(vapply(dims[-1L], identical, logical(1L), dims[[1L]]))) {
    stop("consensus matrices must have identical dimensions", call. = FALSE)
  }
  if (!identical(dims[[1L]], c(length(x$sample_ids), x$k))) {
    stop("consensus matrix dimensions do not match sample IDs and K", call. = FALSE)
  }
  if (any(!is.finite(x$mean_q)) || any(!is.finite(x$median_q)) ||
      any(!is.finite(x$variance_q)) || any(!is.finite(x$sd_q)) ||
      any(!is.finite(x$lower_q)) || any(!is.finite(x$upper_q))) {
    stop("consensus matrices must be finite", call. = FALSE)
  }
  if (any(x$mean_q < -tolerance) || any(x$mean_q > 1 + tolerance) ||
      any(abs(rowSums(x$mean_q) - 1) > tolerance)) {
    stop("mean consensus Q matrix must satisfy ancestry simplex constraints", call. = FALSE)
  }
  if (any(x$variance_q < -tolerance) || any(x$sd_q < -tolerance) ||
      any(x$lower_q > x$upper_q + tolerance)) {
    stop("consensus uncertainty estimates are invalid", call. = FALSE)
  }
  if (!is.finite(x$global_stability) || x$global_stability < 0 || x$global_stability > 1) {
    stop("global_stability must lie in [0, 1]", call. = FALSE)
  }
  invisible(x)
}

#' Convert consensus ancestry estimates to long form
#' @param x A `PopgenVCFAncestryConsensus` object.
#' @return A `data.table` with mean, median, variance, standard deviation, and
#'   confidence interval values for every sample and cluster.
#' @export
ancestry_consensus_table <- function(x) {
  validate_ancestry_consensus(x)
  rows <- vector("list", x$k)
  for (j in seq_len(x$k)) {
    rows[[j]] <- data.table::data.table(
      sample_id = x$sample_ids,
      cluster = paste0("cluster_", j),
      mean = x$mean_q[, j],
      median = x$median_q[, j],
      variance = x$variance_q[, j],
      sd = x$sd_q[, j],
      lower = x$lower_q[, j],
      upper = x$upper_q[, j]
    )
  }
  data.table::rbindlist(rows)
}

#' @export
print.PopgenVCFAncestryConsensus <- function(x, ...) {
  cat("<PopgenVCFAncestryConsensus>", x$backend,
      "K=", x$k, "replicates=", length(x$replicate_ids),
      "stability=", format(x$global_stability, digits = 4), "\n")
  invisible(x)
}
