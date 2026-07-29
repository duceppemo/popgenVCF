normalize_q_matrix <- function(q, tolerance = 1e-8) {
  x <- as.matrix(q)
  storage.mode(x) <- "double"
  if (!length(x) || nrow(x) < 1L || ncol(x) < 2L) stop("Q matrix must have at least one row and two clusters", call. = FALSE)
  if (any(!is.finite(x)) || any(x < -tolerance)) stop("Q matrix contains invalid membership values", call. = FALSE)
  x[x < 0] <- 0
  totals <- rowSums(x)
  if (any(!is.finite(totals) | totals <= 0)) stop("Q matrix contains invalid row totals", call. = FALSE)
  x <- x / totals
  colnames(x) <- paste0("cluster_", seq_len(ncol(x)))
  x
}

permutations_small <- function(x) {
  if (length(x) == 1L) return(matrix(x, nrow = 1L))
  do.call(rbind, lapply(seq_along(x), function(i) {
    cbind(x[i], permutations_small(x[-i]))
  }))
}

cluster_similarity_matrix <- function(target, reference) {
  target <- normalize_q_matrix(target)
  reference <- normalize_q_matrix(reference)
  if (!identical(dim(target), dim(reference))) stop("Q matrices must have identical dimensions", call. = FALSE)
  k <- ncol(target)
  out <- matrix(NA_real_, k, k)
  for (i in seq_len(k)) for (j in seq_len(k)) {
    a <- target[, i]; b <- reference[, j]
    out[i, j] <- if (stats::sd(a) == 0 || stats::sd(b) == 0) -sqrt(mean((a - b)^2)) else stats::cor(a, b)
  }
  out
}

solve_cluster_assignment <- function(similarity) {
  similarity <- as.matrix(similarity)
  storage.mode(similarity) <- "double"
  k <- nrow(similarity)
  if (ncol(similarity) != k) stop("Similarity matrix must be square", call. = FALSE)
  if (!length(similarity) || any(!is.finite(similarity))) {
    stop("Similarity matrix must contain only finite values", call. = FALSE)
  }

  # clue::solve_LSAP() requires nonnegative entries. Adding the same constant
  # to every entry preserves the maximizing assignment because each complete
  # assignment contains exactly k entries.
  assignment_score <- similarity
  minimum <- min(assignment_score)
  if (minimum < 0) assignment_score <- assignment_score - minimum

  if (requireNamespace("clue", quietly = TRUE)) {
    return(as.integer(clue::solve_LSAP(assignment_score, maximum = TRUE)))
  }
  if (k <= 8L) {
    p <- permutations_small(seq_len(k))
    score <- apply(p, 1L, function(z) sum(similarity[cbind(seq_len(k), z)]))
    return(as.integer(p[which.max(score), ]))
  }
  # Deterministic greedy fallback for large K when clue is unavailable.
  assignment <- integer(k); available <- seq_len(k)
  order_rows <- order(apply(similarity, 1L, max), decreasing = TRUE)
  for (i in order_rows) {
    j <- available[which.max(similarity[i, available])]
    assignment[i] <- j
    available <- setdiff(available, j)
  }
  assignment
}

#' Align ancestry or membership clusters across replicate matrices
#'
#' @param target Numeric sample-by-K membership matrix to reorder.
#' @param reference Numeric sample-by-K reference membership matrix.
#' @return A list containing the aligned matrix, permutation, and similarity.
#' @export
align_cluster_labels <- function(target, reference) {
  target <- normalize_q_matrix(target)
  reference <- normalize_q_matrix(reference)
  similarity <- cluster_similarity_matrix(target, reference)
  mapping_target_to_reference <- solve_cluster_assignment(similarity)
  order_target <- match(seq_len(ncol(target)), mapping_target_to_reference)
  aligned <- target[, order_target, drop = FALSE]
  colnames(aligned) <- colnames(reference)
  list(aligned = aligned, permutation = order_target, similarity = similarity,
       assignment = mapping_target_to_reference)
}

#' Compare two ancestry or membership matrices
#'
#' @param target Numeric sample-by-K membership matrix.
#' @param reference Numeric sample-by-K reference membership matrix.
#' @return Comparison metrics after label-switching alignment.
#' @export
compare_q_matrices <- function(target, reference) {
  z <- align_cluster_labels(target, reference)
  reference <- normalize_q_matrix(reference)
  delta <- z$aligned - reference
  correlations <- vapply(seq_len(ncol(reference)), function(j) {
    a <- z$aligned[, j]; b <- reference[, j]
    if (stats::sd(a) == 0 || stats::sd(b) == 0) as.numeric(max(abs(a - b)) < 1e-12) else stats::cor(a, b)
  }, numeric(1))
  list(aligned = z$aligned, permutation = z$permutation,
       rmse = sqrt(mean(delta^2)), maximum_absolute_difference = max(abs(delta)),
       cluster_correlations = correlations, minimum_correlation = min(correlations))
}

#' Assess population-structure reproducibility across seeds or replicates
#'
#' @param matrices Named list of membership matrices with equal dimensions.
#' @param reference Name or index of the reference replicate.
#' @return Per-replicate and consensus reproducibility summaries.
#' @export
structure_reproducibility <- function(matrices, reference = 1L) {
  if (!is.list(matrices) || length(matrices) < 2L) stop("At least two membership matrices are required", call. = FALSE)
  ref <- normalize_q_matrix(matrices[[reference]])
  nm <- names(matrices); if (is.null(nm)) nm <- paste0("replicate_", seq_along(matrices))
  aligned <- vector("list", length(matrices)); metrics <- vector("list", length(matrices))
  for (i in seq_along(matrices)) {
    cmp <- compare_q_matrices(matrices[[i]], ref)
    aligned[[i]] <- cmp$aligned
    metrics[[i]] <- data.table::data.table(replicate = nm[i], rmse = cmp$rmse,
      maximum_absolute_difference = cmp$maximum_absolute_difference,
      minimum_cluster_correlation = cmp$minimum_correlation,
      permutation = paste(cmp$permutation, collapse = ","))
  }
  consensus <- Reduce(`+`, aligned) / length(aligned)
  list(metrics = data.table::rbindlist(metrics), aligned = stats::setNames(aligned, nm), consensus = consensus)
}

#' Select K from one or more population-structure diagnostics
#'
#' @param diagnostics Data frame containing K and one or more of cv_error, BIC,
#'   cross_entropy, or mean_success.
#' @param additional_votes Optional named integer vector or data frame with
#'   `method` and `K` columns containing backend-native recommendations.
#' @param tolerance_fraction Fraction of the observed metric range used by the
#'   parsimonious near-optimum rule when standard errors are unavailable.
#' @return List containing method-specific optima and consensus K.
#' @export
select_structure_k <- function(diagnostics, additional_votes = NULL,
                               tolerance_fraction = 0.02) {
  select_structure_k_consensus(
    diagnostics, additional_votes, tolerance_fraction
  )
}

parse_faststructure_k <- function(text) {
  hit <- regmatches(text, gregexpr("[0-9]+", text))[[1]]
  unique(as.integer(hit[nzchar(hit)]))
}

snmf_project_arguments <- function(geno_file, k_values, repetitions, entropy,
                                   project_mode, threads, seed) {
  threads <- as.integer(threads)[1L]
  repetitions <- as.integer(repetitions)[1L]
  if (is.na(threads) || threads < 1L) stop("sNMF threads must be >= 1", call. = FALSE)
  if (is.na(repetitions) || repetitions < 1L) stop("sNMF repetitions must be >= 1", call. = FALSE)
  k_values <- as.integer(k_values)
  workers <- min(threads, max(1L, length(k_values) * repetitions))
  list(
    input.file = geno_file,
    K = k_values,
    repetitions = repetitions,
    entropy = isTRUE(entropy),
    project = project_mode,
    CPU = workers,
    seed = as.integer(seed)
  )
}

#' Run LEA sNMF across K values
#'
#' @param geno_file LEA .geno input file.
#' @param k_values Integer K values.
#' @param repetitions Number of repetitions per K.
#' @param entropy Use cross-entropy criterion.
#' @param seed Random seed.
#' @param project_mode LEA project mode.
#' @param threads Number of CPUs used by LEA across K and repetition runs.
#' @return sNMF project, diagnostics, and best-run Q matrices.
#' @export
run_snmf <- function(geno_file, k_values, repetitions = 5L, entropy = TRUE,
                     seed = 42L, project_mode = "new", threads = 1L) {
  if (!requireNamespace("LEA", quietly = TRUE)) stop("Package 'LEA' is required for sNMF", call. = FALSE)
  if (!file.exists(geno_file)) stopf("LEA geno file not found: %s", geno_file)
  set.seed(seed)
  project <- do.call(LEA::snmf, snmf_project_arguments(
    geno_file, k_values, repetitions, entropy, project_mode, threads, seed
  ))
  diagnostics <- data.table::rbindlist(lapply(as.integer(k_values), function(k) {
    ce <- if (isTRUE(entropy)) LEA::cross.entropy(project, K = k) else numeric()
    complete_snmf_cross_entropy(k, repetitions, ce)
  }))
  q <- lapply(as.integer(k_values), function(k) {
    rows <- diagnostics[K == k]
    finite <- which(is.finite(rows$cross_entropy))
    best <- if (length(finite)) {
      rows$run[finite[[which.min(rows$cross_entropy[finite])]]]
    } else {
      rows$run[[1L]]
    }
    normalize_q_matrix(LEA::Q(project, K = k, run = best))
  })
  names(q) <- as.character(k_values)
  list(project = project, diagnostics = diagnostics, q = q)
}

complete_snmf_cross_entropy <- function(k, repetitions, values = numeric()) {
  values <- suppressWarnings(as.numeric(values))
  repetitions <- max(as.integer(repetitions), length(values), 1L)
  if (length(values) < repetitions) {
    values <- c(values, rep(NA_real_, repetitions - length(values)))
  }
  data.table::data.table(
    K = rep(as.integer(k), repetitions),
    run = seq_len(repetitions),
    cross_entropy = values
  )
}

summarize_snmf_k_diagnostics <- function(diagnostics) {
  x <- data.table::copy(data.table::as.data.table(diagnostics))
  if (!"K" %in% names(x)) {
    stop("sNMF diagnostics require a K column", call. = FALSE)
  }
  if (!"cross_entropy" %in% names(x)) {
    x[, cross_entropy := NA_real_]
  } else {
    x[, cross_entropy := suppressWarnings(as.numeric(cross_entropy))]
  }
  x[, {
    finite <- cross_entropy[is.finite(cross_entropy)]
    list(
      cross_entropy = if (length(finite)) mean(finite) else NA_real_,
      cross_entropy_se = if (length(finite) > 1L) {
        stats::sd(finite) / sqrt(length(finite))
      } else if (length(finite) == 1L) {
        0
      } else {
        NA_real_
      }
    )
  }, by = K]
}

synthetic_structure_membership <- function(n_per_cluster = 20L, k = 3L, noise = 0.02, seed = 42L) {
  set.seed(seed)
  labels <- rep(seq_len(k), each = n_per_cluster)
  q <- matrix(noise / max(1, k - 1), length(labels), k)
  q[cbind(seq_along(labels), labels)] <- 1 - noise
  normalize_q_matrix(q)
}

#' Run deterministic population-structure validation
#'
#' @param integration Run optional adegenet DAPC validation.
#' @param seed Random seed.
#' @return Validation checks, reproducibility diagnostics, and pass flag.
#' @export
run_population_structure_validation <- function(integration = FALSE, seed = 42L) {
  q <- synthetic_structure_membership(seed = seed)
  permuted <- q[, c(3, 1, 2), drop = FALSE]
  noisy <- normalize_q_matrix(q + matrix(stats::runif(length(q), 0, 0.005), nrow(q)))
  cmp <- compare_q_matrices(permuted, q)
  rep <- structure_reproducibility(list(reference = q, permuted = permuted, noisy = noisy))
  checks <- data.table::rbindlist(list(
    data.table::data.table(label = "label-switching alignment", passed = cmp$maximum_absolute_difference < 1e-12,
                           metric = cmp$maximum_absolute_difference, tolerance = 1e-12),
    data.table::data.table(label = "structure replicate reproducibility", passed = max(rep$metrics$rmse) < 0.01,
                           metric = max(rep$metrics$rmse), tolerance = 0.01)
  ))
  details <- list(q = q, comparison = cmp, reproducibility = rep)
  if (isTRUE(integration)) {
    # Validate DAPC classification on a strongly separated synthetic genotype matrix.
    set.seed(seed)
    groups <- factor(rep(c("A", "B", "C"), each = 12L))
    geno <- matrix(0, nrow = length(groups), ncol = 60L)
    for (g in seq_len(3L)) {
      idx <- which(groups == levels(groups)[g])
      block <- ((g - 1L) * 20L + 1L):(g * 20L)
      geno[idx, block] <- 2
      geno[idx, -block] <- matrix(stats::rbinom(length(idx) * 40L, 2, 0.03), nrow = length(idx))
    }
    rownames(geno) <- paste0("s", seq_len(nrow(geno)))
    metadata <- data.table::data.table(sample = rownames(geno), population = as.character(groups))
    dapc <- run_dapc_analysis(geno, rownames(geno), metadata, 3L, seed, cross_validate = FALSE,
                              replicate_seeds = seed + 0:2)
    acc <- dapc$diagnostics$assignment_accuracy[1]
    checks <- data.table::rbindlist(list(checks,
      data.table::data.table(label = "DAPC synthetic classification", passed = is.finite(acc) && acc >= 0.9,
                             metric = acc, tolerance = 0.9)))
    details$dapc <- dapc
  }
  list(checks = checks, details = details, passed = all(checks$passed))
}
