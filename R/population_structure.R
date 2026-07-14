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
#' @return List containing method-specific optima and consensus K.
#' @export
select_structure_k <- function(diagnostics) {
  x <- data.table::as.data.table(diagnostics)
  if (!"K" %in% names(x) || anyDuplicated(x$K)) stop("Diagnostics require unique K values", call. = FALSE)
  choices <- list()
  minimize <- intersect(c("cv_error", "BIC", "cross_entropy"), names(x))
  maximize <- intersect(c("mean_success", "silhouette"), names(x))
  for (nm in minimize) if (any(is.finite(x[[nm]]))) choices[[nm]] <- x$K[which.min(x[[nm]])]
  for (nm in maximize) if (any(is.finite(x[[nm]]))) choices[[nm]] <- x$K[which.max(x[[nm]])]
  votes <- unlist(choices, use.names = FALSE)
  consensus <- if (length(votes)) as.integer(names(sort(table(votes), decreasing = TRUE))[1]) else NA_integer_
  list(best_by_method = data.table::data.table(method = names(choices), K = as.integer(unlist(choices))),
       consensus_k = consensus)
}

parse_faststructure_k <- function(text) {
  hit <- regmatches(text, gregexpr("[0-9]+", text))[[1]]
  unique(as.integer(hit[nzchar(hit)]))
}

#' Run external fastStructure across K values
#'
#' @param structure_executable Path or command for structure.py.
#' @param choosek_executable Path or command for chooseK.py.
#' @param plink_prefix PLINK BED/BIM/FAM prefix.
#' @param k_values Integer K values.
#' @param output_dir Output directory.
#' @param seed Random seed.
#' @return A list containing run records, Q matrices, and chooseK output.
#' @export
run_faststructure <- function(structure_executable = "structure.py", choosek_executable = "chooseK.py",
                              plink_prefix, k_values, output_dir = ".", seed = 42L) {
  exe <- Sys.which(structure_executable); choose <- Sys.which(choosek_executable)
  if (!nzchar(exe)) stopf("fastStructure executable not found: %s", structure_executable)
  if (!file.exists(paste0(plink_prefix, ".bed"))) stop("fastStructure requires PLINK BED/BIM/FAM input", call. = FALSE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  prefix <- file.path(output_dir, "faststructure")
  runs <- list(); q <- list()
  for (k in as.integer(k_values)) {
    args <- c("-K", k, "--input", normalizePath(plink_prefix), "--output", prefix,
              "--seed", as.integer(seed + k), "--format", "bed")
    out <- system2(exe, args, stdout = TRUE, stderr = TRUE)
    log <- file.path(output_dir, sprintf("fastStructure_K%d.log", k)); writeLines(out, log)
    qfile <- sprintf("%s.%d.meanQ", prefix, k)
    if (file.exists(qfile)) q[[as.character(k)]] <- normalize_q_matrix(data.table::fread(qfile, header = FALSE))
    runs[[as.character(k)]] <- data.table::data.table(K = k, exit_status = attr(out, "status") %||% 0L,
                                                       log_file = log, q_file = qfile)
  }
  choose_text <- character()
  if (nzchar(choose)) choose_text <- system2(choose, c("--input", prefix), stdout = TRUE, stderr = TRUE)
  list(runs = data.table::rbindlist(runs), q = q, choose_k_text = choose_text,
       suggested_k = parse_faststructure_k(paste(choose_text, collapse = "\n")))
}

#' Run LEA sNMF across K values
#'
#' @param geno_file LEA .geno input file.
#' @param k_values Integer K values.
#' @param repetitions Number of repetitions per K.
#' @param entropy Use cross-entropy criterion.
#' @param seed Random seed.
#' @param project_mode LEA project mode.
#' @return sNMF project, diagnostics, and best-run Q matrices.
#' @export
run_snmf <- function(geno_file, k_values, repetitions = 5L, entropy = TRUE,
                     seed = 42L, project_mode = "new") {
  if (!requireNamespace("LEA", quietly = TRUE)) stop("Package 'LEA' is required for sNMF", call. = FALSE)
  if (!file.exists(geno_file)) stopf("LEA geno file not found: %s", geno_file)
  set.seed(seed)
  project <- LEA::snmf(geno_file, K = as.integer(k_values), repetitions = as.integer(repetitions),
                       entropy = isTRUE(entropy), project = project_mode, CPU = 1L)
  diagnostics <- data.table::rbindlist(lapply(as.integer(k_values), function(k) {
    ce <- LEA::cross.entropy(project, K = k)
    data.table::data.table(K = k, run = seq_along(ce), cross_entropy = ce)
  }))
  q <- lapply(as.integer(k_values), function(k) {
    rows <- diagnostics[K == k]; best <- rows$run[which.min(rows$cross_entropy)]
    normalize_q_matrix(LEA::Q(project, K = k, run = best))
  })
  names(q) <- as.character(k_values)
  list(project = project, diagnostics = diagnostics, q = q)
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
