#' Align ancestry Q matrices across label permutations
#'
#' Uses a Hungarian assignment on a nonnegative similarity matrix combining
#' column-wise Pearson correlation and cosine similarity. Sample order must be
#' identical and both matrices must have the same number of clusters.
#'
#' @param target Numeric Q matrix or `PopgenVCFAncestryReplicate` to align.
#' @param reference Numeric Q matrix or `PopgenVCFAncestryReplicate` defining
#'   the desired cluster order.
#' @param tolerance Numerical tolerance for Q-matrix validation.
#' @return A `PopgenVCFAncestryAlignment` containing the aligned Q matrix,
#'   permutation, permutation matrix, similarity matrices, RMSD, cosine score,
#'   correlation score, and combined alignment score.
#' @export
align_ancestry_replicate <- function(target, reference, tolerance = 1e-6) {
  target_rep <- inherits(target, "PopgenVCFAncestryReplicate")
  reference_rep <- inherits(reference, "PopgenVCFAncestryReplicate")

  if (target_rep) validate_ancestry_replicate(target, tolerance)
  if (reference_rep) validate_ancestry_replicate(reference, tolerance)
  if (xor(target_rep, reference_rep)) {
    stop("target and reference must both be matrices or ancestry replicates", call. = FALSE)
  }

  target_q <- if (target_rep) target$q else as.matrix(target)
  reference_q <- if (reference_rep) reference$q else as.matrix(reference)
  storage.mode(target_q) <- storage.mode(reference_q) <- "double"

  if (target_rep && !identical(target$sample_ids, reference$sample_ids)) {
    stop("target and reference must use identical sample IDs and order", call. = FALSE)
  }
  if (!identical(dim(target_q), dim(reference_q))) {
    stop("target and reference Q matrices must have identical dimensions", call. = FALSE)
  }
  if (!nrow(target_q) || !ncol(target_q) || any(!is.finite(target_q)) || any(!is.finite(reference_q))) {
    stop("Q matrices must be non-empty and finite", call. = FALSE)
  }

  cosine <- ancestry_column_cosine(reference_q, target_q)
  correlation <- ancestry_column_correlation(reference_q, target_q)
  correlation_scaled <- (correlation + 1) / 2
  similarity <- (cosine + correlation_scaled) / 2
  similarity[!is.finite(similarity)] <- 0
  similarity[similarity < 0] <- 0
  similarity[similarity > 1] <- 1

  permutation <- as.integer(clue::solve_LSAP(similarity, maximum = TRUE))
  aligned_q <- target_q[, permutation, drop = FALSE]
  k <- ncol(target_q)
  permutation_matrix <- matrix(0, nrow = k, ncol = k)
  permutation_matrix[cbind(permutation, seq_len(k))] <- 1

  matched <- cbind(seq_len(k), permutation)
  rmsd <- sqrt(mean((aligned_q - reference_q)^2))
  correlation_score <- mean(correlation[matched])
  cosine_score <- mean(cosine[matched])
  alignment_score <- mean(similarity[matched])

  aligned_replicate <- NULL
  if (target_rep) {
    aligned_replicate <- target
    aligned_replicate$q <- aligned_q
    aligned_replicate$provenance$alignment <- list(
      reference_backend = reference$backend,
      reference_k = reference$k,
      reference_replicate = reference$replicate,
      permutation = permutation,
      score = alignment_score
    )
    validate_ancestry_replicate(aligned_replicate, tolerance)
  }

  structure(list(
    aligned_q = aligned_q,
    aligned_replicate = aligned_replicate,
    permutation = permutation,
    permutation_matrix = permutation_matrix,
    similarity = similarity,
    correlation = correlation,
    cosine = cosine,
    rmsd = rmsd,
    correlation_score = correlation_score,
    cosine_score = cosine_score,
    alignment_score = alignment_score
  ), class = "PopgenVCFAncestryAlignment")
}

ancestry_column_cosine <- function(reference, target) {
  cross <- crossprod(reference, target)
  ref_norm <- sqrt(colSums(reference^2))
  target_norm <- sqrt(colSums(target^2))
  denom <- outer(ref_norm, target_norm)
  out <- cross / denom
  out[denom == 0] <- 0
  out
}

ancestry_column_correlation <- function(reference, target) {
  out <- suppressWarnings(stats::cor(reference, target, use = "pairwise.complete.obs"))
  out[!is.finite(out)] <- 0
  out
}

#' @export
print.PopgenVCFAncestryAlignment <- function(x, ...) {
  cat("<PopgenVCFAncestryAlignment> K=", ncol(x$aligned_q),
      " score=", format(x$alignment_score, digits = 4),
      " RMSD=", format(x$rmsd, digits = 4), "\n", sep = "")
  invisible(x)
}
