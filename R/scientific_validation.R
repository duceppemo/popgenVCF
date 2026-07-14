# Scientific validation helpers -------------------------------------------------

#' Default scientific-validation tolerances
#'
#' @return A named list of absolute and relative tolerances.
#' @export
validation_tolerances <- function() {
  list(
    missingness_abs = 0,
    allele_frequency_abs = 1e-12,
    heterozygosity_abs = 1e-12,
    ibs_abs = 1e-8,
    pca_variance_rel = 1e-6,
    pca_scores_abs = 1e-6,
    fst_abs = 1e-6,
    mantel_abs = 1e-6
  )
}

validation_fixture_paths <- function() {
  base <- system.file("extdata", "validation", package = "popgenVCF")
  if (!nzchar(base)) stop("Installed validation fixtures were not found", call. = FALSE)
  list(
    directory = base,
    vcf = file.path(base, "core_validation.vcf"),
    metadata = file.path(base, "core_validation_metadata.tsv"),
    dosage = file.path(base, "core_validation_dosage.tsv"),
    expected_variant_qc = file.path(base, "expected_variant_qc.tsv"),
    expected_sample = file.path(base, "expected_sample_qc_diversity.tsv"),
    expected_ld_ids = file.path(base, "expected_ld_retained_variant_ids.txt")
  )
}

#' Calculate deterministic validation statistics from a dosage matrix
#'
#' @param genotype Numeric sample-by-variant matrix containing 0, 1, 2, or NA.
#' @param sample_ids Optional sample identifiers.
#' @param variant_ids Optional variant identifiers.
#' @return A list with per-variant and per-sample tables.
#' @export
validation_statistics <- function(genotype, sample_ids = rownames(genotype), variant_ids = colnames(genotype)) {
  genotype <- as.matrix(genotype)
  storage.mode(genotype) <- "double"
  if (is.null(sample_ids)) sample_ids <- paste0("sample_", seq_len(nrow(genotype)))
  if (is.null(variant_ids)) variant_ids <- paste0("variant_", seq_len(ncol(genotype)))
  if (length(sample_ids) != nrow(genotype) || length(variant_ids) != ncol(genotype)) {
    stop("Identifier dimensions do not match the genotype matrix", call. = FALSE)
  }
  called_variant <- colSums(!is.na(genotype))
  alt_count <- colSums(genotype, na.rm = TRUE)
  af <- ifelse(called_variant > 0, alt_count / (2 * called_variant), NA_real_)
  variant <- data.table::data.table(
    variant_id = variant_ids,
    alternate_allele_frequency = af,
    maf = pmin(af, 1 - af),
    missing_rate = 1 - called_variant / nrow(genotype)
  )
  called_sample <- rowSums(!is.na(genotype))
  het <- rowSums(genotype == 1, na.rm = TRUE)
  sample <- data.table::data.table(
    sample = sample_ids,
    loci_called = called_sample,
    missing_rate = 1 - called_sample / ncol(genotype),
    heterozygous_calls = het,
    observed_heterozygosity = ifelse(called_sample > 0, het / called_sample, NA_real_)
  )
  list(variant = variant, sample = sample)
}

#' Align PCA score signs to a reference solution
#'
#' Eigenvectors are equivalent after sign reversal. This helper flips each
#' target component to maximize its correlation with the corresponding
#' reference component.
#'
#' @param target Numeric matrix of PCA scores.
#' @param reference Numeric matrix of reference PCA scores.
#' @return Sign-aligned target matrix.
#' @export
align_pca_signs <- function(target, reference) {
  target <- as.matrix(target); reference <- as.matrix(reference)
  if (!identical(dim(target), dim(reference))) stop("PCA matrices must have identical dimensions", call. = FALSE)
  out <- target
  for (j in seq_len(ncol(out))) {
    r <- suppressWarnings(stats::cor(out[, j], reference[, j], use = "complete.obs"))
    if (is.finite(r) && r < 0) out[, j] <- -out[, j]
  }
  out
}

#' Compare numerical validation outputs
#'
#' @param observed Numeric vector or matrix.
#' @param expected Numeric vector or matrix.
#' @param tolerance Absolute tolerance.
#' @param label Comparison label.
#' @return One-row data table with comparison metrics.
#' @export
compare_validation_values <- function(observed, expected, tolerance, label = "comparison") {
  observed <- as.numeric(observed); expected <- as.numeric(expected)
  if (length(observed) != length(expected)) {
    return(data.table::data.table(label = label, passed = FALSE, n = max(length(observed), length(expected)),
      max_absolute_difference = Inf, tolerance = tolerance, message = "length mismatch"))
  }
  difference <- abs(observed - expected)
  both_na <- is.na(observed) & is.na(expected)
  difference[both_na] <- 0
  difference[xor(is.na(observed), is.na(expected))] <- Inf
  maximum <- if (length(difference)) max(difference) else 0
  data.table::data.table(label = label, passed = is.finite(maximum) && maximum <= tolerance,
    n = length(observed), max_absolute_difference = maximum, tolerance = tolerance,
    message = if (is.finite(maximum) && maximum <= tolerance) "within tolerance" else "outside tolerance")
}



#' Calculate an IBS similarity matrix directly from genotype dosages
#'
#' At each jointly called locus, identical diploid dosages contribute 1,
#' dosages differing by one allele contribute 0.5, and opposite homozygotes
#' contribute 0. The pairwise mean is returned.
#'
#' @param genotype Numeric sample-by-variant dosage matrix.
#' @return Symmetric identity-by-state similarity matrix.
#' @export
manual_ibs_matrix <- function(genotype) {
  genotype <- as.matrix(genotype)
  storage.mode(genotype) <- "double"
  n <- nrow(genotype)
  ids <- rownames(genotype)
  if (is.null(ids)) ids <- paste0("sample_", seq_len(n))
  out <- matrix(NA_real_, n, n, dimnames = list(ids, ids))
  for (i in seq_len(n)) {
    for (j in i:n) {
      called <- is.finite(genotype[i, ]) & is.finite(genotype[j, ])
      value <- if (any(called)) {
        mean(1 - abs(genotype[i, called] - genotype[j, called]) / 2)
      } else {
        NA_real_
      }
      out[i, j] <- out[j, i] <- value
    }
  }
  diag(out) <- 1
  out
}

#' Compare two PCA subspaces
#'
#' PCA axes may be sign-reversed, and nearly tied eigenvalues may rotate within
#' the same valid eigenspace. This function compares orthonormal bases through
#' their canonical correlations.
#'
#' @param observed Numeric score or eigenvector matrix.
#' @param expected Numeric reference matrix with matching rows.
#' @param components Number of leading components to compare.
#' @return A list with canonical correlations and their minimum.
#' @export
compare_pca_subspaces <- function(observed, expected, components = 2L) {
  observed <- as.matrix(observed)
  expected <- as.matrix(expected)
  if (nrow(observed) != nrow(expected)) {
    stop("PCA matrices must contain the same number of samples", call. = FALSE)
  }
  k <- min(as.integer(components), ncol(observed), ncol(expected))
  if (k < 1L) stop("No PCA components are available for comparison", call. = FALSE)
  qo <- qr.Q(qr(observed[, seq_len(k), drop = FALSE]))
  qe <- qr.Q(qr(expected[, seq_len(k), drop = FALSE]))
  values <- svd(crossprod(qo, qe), nu = 0L, nv = 0L)$d
  list(canonical_correlations = values, minimum = min(values), components = k)
}


#' Validate PCA eigenvectors against a covariance matrix
#'
#' This checks the eigen equation directly and is invariant to arbitrary sign
#' changes and eigenvalue scaling conventions. For every supplied vector, the
#' Rayleigh quotient is used as the corresponding eigenvalue and the relative
#' residual ||Gv - lambda v|| / ||Gv|| is reported.
#'
#' @param covariance Numeric square covariance or genetic relationship matrix.
#' @param eigenvectors Numeric matrix with samples in rows and components in columns.
#' @param components Number of leading components to validate.
#' @return A list containing relative residuals, orthonormality error and maxima.
#' @export
pca_eigen_residuals <- function(covariance, eigenvectors, components = 2L) {
  g <- as.matrix(covariance)
  v <- as.matrix(eigenvectors)
  if (nrow(g) != ncol(g)) stop("PCA covariance matrix must be square", call. = FALSE)
  if (nrow(v) != nrow(g)) stop("PCA eigenvectors and covariance matrix have incompatible dimensions", call. = FALSE)
  k <- min(as.integer(components), ncol(v))
  if (k < 1L) stop("No PCA components are available for validation", call. = FALSE)
  v <- v[, seq_len(k), drop = FALSE]
  residuals <- vapply(seq_len(k), function(i) {
    z <- v[, i]
    gz <- as.numeric(g %*% z)
    lambda <- sum(z * gz) / sum(z * z)
    denom <- max(sqrt(sum(gz * gz)), .Machine$double.eps)
    sqrt(sum((gz - lambda * z)^2)) / denom
  }, numeric(1))
  gram <- crossprod(v)
  target <- diag(diag(gram), nrow = k)
  orthogonality_error <- max(abs(gram - target))
  list(
    residuals = residuals,
    maximum_residual = max(residuals),
    orthogonality_error = orthogonality_error,
    components = k
  )
}

manual_standardized_pca <- function(genotype) {
  x <- as.matrix(genotype)
  storage.mode(x) <- "double"
  p <- colMeans(x, na.rm = TRUE) / 2
  for (j in seq_len(ncol(x))) x[is.na(x[, j]), j] <- 2 * p[j]
  denom <- sqrt(2 * p * (1 - p))
  keep <- is.finite(denom) & denom > 0
  x <- sweep(x[, keep, drop = FALSE], 2L, 2 * p[keep], "-")
  x <- sweep(x, 2L, denom[keep], "/")
  sv <- svd(x, nu = min(nrow(x), ncol(x)), nv = 0L)
  scores <- sv$u %*% diag(sv$d, nrow = length(sv$d))
  rownames(scores) <- rownames(genotype)
  variance <- sv$d^2 / sum(sv$d^2)
  list(scores = scores, vectors = sv$u, variance = variance)
}

#' Calculate population diversity directly from genotype dosages
#'
#' @param genotype Numeric sample-by-variant dosage matrix.
#' @param populations Population label for every sample row.
#' @return Population-level observed and unbiased expected heterozygosity.
#' @export
manual_population_diversity <- function(genotype, populations) {
  genotype <- as.matrix(genotype)
  if (length(populations) != nrow(genotype)) {
    stop("Population labels must match genotype rows", call. = FALSE)
  }
  pops <- sort(unique(as.character(populations)))
  data.table::rbindlist(lapply(pops, function(pop) {
    x <- genotype[populations == pop, , drop = FALSE]
    n_called <- colSums(!is.na(x))
    gene_copies <- 2 * n_called
    p <- ifelse(gene_copies > 0, colSums(x, na.rm = TRUE) / gene_copies, NA_real_)
    ho <- ifelse(n_called > 0, colSums(x == 1, na.rm = TRUE) / n_called, NA_real_)
    he <- 2 * p * (1 - p)
    he_unbiased <- ifelse(gene_copies > 1, he * gene_copies / (gene_copies - 1), NA_real_)
    mean_ho <- mean(ho, na.rm = TRUE)
    mean_he <- mean(he_unbiased, na.rm = TRUE)
    data.table::data.table(
      population = pop,
      n_samples = nrow(x),
      polymorphic_loci = sum(is.finite(p) & p > 0 & p < 1),
      observed_heterozygosity = mean_ho,
      expected_heterozygosity = mean_he,
      inbreeding_coefficient = if (is.finite(mean_he) && mean_he > 0) 1 - mean_ho / mean_he else NA_real_
    )
  }))
}


manual_wc84_components <- function(genotype, populations) {
  genotype <- as.matrix(genotype)
  populations <- factor(populations)
  if (length(populations) != nrow(genotype)) {
    stop("Population labels must match genotype rows", call. = FALSE)
  }
  loci <- lapply(seq_len(ncol(genotype)), function(j) {
    x <- genotype[, j]
    lev <- levels(populations)
    n_i <- p_i <- h_i <- numeric(length(lev))
    for (i in seq_along(lev)) {
      z <- x[populations == lev[i]]
      z <- z[is.finite(z)]
      n_i[i] <- length(z)
      p_i[i] <- if (n_i[i] > 0) sum(z) / (2 * n_i[i]) else NA_real_
      h_i[i] <- if (n_i[i] > 0) mean(z == 1) else NA_real_
    }
    keep <- n_i > 0 & is.finite(p_i) & is.finite(h_i)
    n_i <- n_i[keep]; p_i <- p_i[keep]; h_i <- h_i[keep]
    r <- length(n_i)
    if (r < 2L) return(c(a = NA_real_, b = NA_real_, c = NA_real_, fst = NA_real_))
    n_bar <- mean(n_i)
    if (!is.finite(n_bar) || n_bar <= 1) {
      return(c(a = NA_real_, b = NA_real_, c = NA_real_, fst = NA_real_))
    }
    p_bar <- sum(n_i * p_i) / (r * n_bar)
    h_bar <- sum(n_i * h_i) / (r * n_bar)
    s2 <- sum(n_i * (p_i - p_bar)^2) / ((r - 1) * n_bar)
    n_c <- (r * n_bar - sum(n_i^2) / (r * n_bar)) / (r - 1)
    if (!is.finite(n_c) || n_c <= 0) {
      return(c(a = NA_real_, b = NA_real_, c = NA_real_, fst = NA_real_))
    }
    a <- (n_bar / n_c) * (s2 - (p_bar * (1 - p_bar) -
      ((r - 1) / r) * s2 - h_bar / 4) / (n_bar - 1))
    b <- (n_bar / (n_bar - 1)) * (p_bar * (1 - p_bar) -
      ((r - 1) / r) * s2 - ((2 * n_bar - 1) / (4 * n_bar)) * h_bar)
    c_comp <- h_bar / 2
    den <- a + b + c_comp
    c(a = a, b = b, c = c_comp, fst = if (is.finite(den) && den != 0) a / den else NA_real_)
  })
  comp <- do.call(rbind, loci)
  den <- rowSums(comp[, c("a", "b", "c"), drop = FALSE], na.rm = FALSE)
  valid <- is.finite(comp[, "a"]) & is.finite(den) & den != 0
  global <- if (any(valid)) sum(comp[valid, "a"]) / sum(den[valid]) else NA_real_
  list(global = global, per_locus = comp)
}

manual_wc84_fst <- function(genotype, populations) {
  populations <- factor(populations)
  lev <- levels(populations)
  global <- manual_wc84_components(genotype, populations)$global
  mat <- matrix(0, length(lev), length(lev), dimnames = list(lev, lev))
  if (length(lev) >= 2L) {
    pairs <- utils::combn(lev, 2, simplify = FALSE)
    for (pp in pairs) {
      keep <- populations %in% pp
      value <- manual_wc84_components(genotype[keep, , drop = FALSE],
                                      droplevels(populations[keep]))$global
      mat[pp[1], pp[2]] <- mat[pp[2], pp[1]] <- value
    }
  }
  list(global = global, pairwise = mat)
}

pca_covariance_reference <- function(gds, sample_ids, snp_ids, components, threads) {
  z <- SNPRelate::snpgdsPCA(
    gds, sample.id = sample_ids, snp.id = snp_ids, autosome.only = FALSE,
    eigen.cnt = max(as.integer(components), 2L), num.thread = threads,
    need.genmat = TRUE, verbose = FALSE
  )
  if (is.null(z$genmat)) stop("SNPRelate did not return the requested genetic covariance matrix", call. = FALSE)
  e <- eigen(as.matrix(z$genmat), symmetric = TRUE)
  list(pca = z, vectors = e$vectors, values = e$values)
}

hierfstat_reference_fst <- function(genotype, populations) {
  encoded <- as.data.frame(genotype, check.names = FALSE)
  encoded[] <- lapply(encoded, function(x) {
    out <- rep(NA_integer_, length(x))
    out[x == 0] <- 11L
    out[x == 1] <- 12L
    out[x == 2] <- 22L
    out
  })
  dat <- data.frame(pop = as.integer(factor(populations)), encoded, check.names = FALSE)
  pairwise <- as.matrix(hierfstat::pairwise.WCfst(dat))
  labels <- levels(factor(populations))
  if (nrow(pairwise) == length(labels)) rownames(pairwise) <- labels
  if (ncol(pairwise) == length(labels)) colnames(pairwise) <- labels
  wc <- hierfstat::wc(dat)
  list(global = as.numeric(wc$FST), pairwise = pairwise)
}

#' Run bundled deterministic scientific validation
#'
#' This performs hand-calculated QC and diversity comparisons. With
#' `integration = TRUE`, it additionally validates SNPRelate QC, exact LD
#' pruning, PCA, IBS/MDS, diversity, and FST against independent calculations.
#'
#' @param integration Run the complete integration validation suite.
#' @param threads Requested threads; LD validation is capped at four.
#' @return A list containing detailed comparisons and an overall pass flag.
#' @export
run_scientific_validation <- function(integration = FALSE, threads = 1L) {
  paths <- validation_fixture_paths()
  dosage <- data.table::fread(paths$dosage, na.strings = "NA")
  sample_ids <- as.character(dosage[[1L]])
  genotype <- as.matrix(dosage[, -1L])
  rownames(genotype) <- sample_ids
  stats <- validation_statistics(genotype)
  exp_variant <- data.table::fread(paths$expected_variant_qc)
  exp_sample <- data.table::fread(paths$expected_sample)
  metadata <- read_metadata(paths$metadata, "yes")
  tol <- validation_tolerances()
  checks <- data.table::rbindlist(list(
    compare_validation_values(stats$variant$alternate_allele_frequency, exp_variant$alternate_allele_frequency,
      tol$allele_frequency_abs, "hand-calculated alternate allele frequency"),
    compare_validation_values(stats$variant$maf, exp_variant$maf, tol$allele_frequency_abs, "hand-calculated MAF"),
    compare_validation_values(stats$variant$missing_rate, exp_variant$missing_rate,
      tol$missingness_abs, "hand-calculated variant missingness"),
    compare_validation_values(stats$sample$missing_rate, exp_sample$missing_rate,
      tol$missingness_abs, "hand-calculated sample missingness"),
    compare_validation_values(stats$sample$observed_heterozygosity, exp_sample$observed_heterozygosity,
      tol$heterozygosity_abs, "hand-calculated observed heterozygosity")
  ))
  integration_details <- NULL
  if (isTRUE(integration)) {
    gds_file <- tempfile(fileext = ".gds")
    on.exit(unlink(gds_file), add = TRUE)
    SNPRelate::snpgdsVCF2GDS(paths$vcf, gds_file, method = "biallelic.only", verbose = FALSE)
    gds <- SNPRelate::snpgdsOpen(gds_file, readonly = TRUE)
    on.exit(try(SNPRelate::snpgdsClose(gds), silent = TRUE), add = TRUE)
    ids <- get_gds_ids(gds)
    vq <- variant_qc(gds, metadata$sample, ids, 0.05, 0.2)
    expected_key <- paste(exp_variant$chromosome, exp_variant$position, sep = ":")
    observed_key <- paste(vq$chromosome, vq$position, sep = ":")
    observed_order <- match(observed_key, expected_key)
    if (anyNA(observed_order)) stop("Could not map validation variants by chromosome and position", call. = FALSE)
    checks <- data.table::rbindlist(list(checks,
      compare_validation_values(vq$maf, exp_variant$maf[observed_order], tol$allele_frequency_abs, "SNPRelate MAF"),
      compare_validation_values(vq$missing_rate, exp_variant$missing_rate[observed_order], tol$missingness_abs, "SNPRelate missingness")
    ))
    ld <- ld_prune_exact(gds, metadata$sample, 0.05, threads, 42L)
    ld_chr <- ids$chromosome[match(ld, ids$snp)]
    ld_pos <- ids$position[match(ld, ids$snp)]
    ld_names <- exp_variant$variant_id[match(paste(ld_chr, ld_pos, sep = ":"), expected_key)]
    expected_ids <- readLines(paths$expected_ld_ids, warn = FALSE)
    ld_pass <- identical(sort(ld_names), sort(expected_ids))
    checks <- data.table::rbindlist(list(checks, data.table::data.table(
      label = "SNPRelate exact LD-retained marker IDs", passed = ld_pass,
      n = length(ld_names), max_absolute_difference = if (ld_pass) 0 else Inf,
      tolerance = 0, message = paste(ld_names, collapse = ",")
    )))

    # Independent IBS and MDS validation on the exact LD-pruned set.
    ld_genotype <- genotype[, match(expected_ids, colnames(genotype)), drop = FALSE]
    manual_ibs <- manual_ibs_matrix(ld_genotype)
    ibs <- run_ibs(gds, metadata$sample, ld, metadata, min(as.integer(threads), 4L))
    ibs_order <- match(rownames(ibs$similarity), rownames(manual_ibs))
    checks <- data.table::rbindlist(list(checks,
      compare_validation_values(ibs$similarity, manual_ibs[ibs_order, ibs_order], tol$ibs_abs,
        "SNPRelate IBS versus hand calculation")
    ))
    manual_mds <- stats::cmdscale(stats::as.dist(1 - manual_ibs), k = 2L, eig = TRUE)
    observed_mds <- as.matrix(ibs$mds[, .(MDS1, MDS2)])
    reference_mds <- manual_mds$points[match(ibs$mds$sample, rownames(manual_mds$points)), , drop = FALSE]
    mds_space <- compare_pca_subspaces(observed_mds, reference_mds, 2L)
    checks <- data.table::rbindlist(list(checks, data.table::data.table(
      label = "MDS eigenspace equivalence", passed = mds_space$minimum >= 1 - 1e-8,
      n = mds_space$components, max_absolute_difference = 1 - mds_space$minimum,
      tolerance = 1e-8, message = paste(signif(mds_space$canonical_correlations, 8), collapse = ",")
    )))

    # PCA implementation validation against the genetic covariance matrix
    # returned by SNPRelate. A standardized-dosage SVD is retained as a
    # cross-method diagnostic because its normalization is not identical.
    pca <- run_pca(gds, metadata$sample, ld, metadata, 5L, min(as.integer(threads), 4L))
    cov_ref <- pca_covariance_reference(gds, metadata$sample, ld, 5L, min(as.integer(threads), 4L))
    observed_vectors <- pca$object$eigenvect[match(cov_ref$pca$sample.id, pca$object$sample.id), , drop = FALSE]
    pca_consistency <- pca_eigen_residuals(cov_ref$pca$genmat, observed_vectors, 2L)
    checks <- data.table::rbindlist(list(checks, data.table::data.table(
      label = "PCA eigen-equation residual",
      passed = pca_consistency$maximum_residual <= 1e-8,
      n = pca_consistency$components,
      max_absolute_difference = pca_consistency$maximum_residual,
      tolerance = 1e-8,
      message = paste(signif(pca_consistency$residuals, 8), collapse = ",")
    )))
    manual_pca <- manual_standardized_pca(ld_genotype)
    standardized_pca_diagnostic <- compare_pca_subspaces(observed_vectors, manual_pca$vectors, 2L)
    covariance_eigendecomposition_diagnostic <- compare_pca_subspaces(
      observed_vectors, cov_ref$vectors, 2L
    )

    # Diversity calculated by package versus direct dosage arithmetic.
    qc_names <- exp_variant[pass_combined == TRUE, variant_id]
    qc_key <- expected_key[exp_variant$pass_combined == TRUE]
    qc_ids <- ids$snp[match(qc_key, paste(ids$chromosome, ids$position, sep = ":"))]
    div <- compute_diversity(gds, metadata$sample, qc_ids, metadata, ids)
    manual_div <- manual_population_diversity(genotype[, match(qc_names, colnames(genotype)), drop = FALSE], metadata$population)
    div_order <- match(div$population$population, manual_div$population)
    checks <- data.table::rbindlist(list(checks,
      compare_validation_values(div$population$observed_heterozygosity,
        manual_div$observed_heterozygosity[div_order], tol$heterozygosity_abs,
        "population observed heterozygosity"),
      compare_validation_values(div$population$expected_heterozygosity,
        manual_div$expected_heterozygosity[div_order], tol$heterozygosity_abs,
        "population unbiased expected heterozygosity"),
      compare_validation_values(div$population$inbreeding_coefficient,
        manual_div$inbreeding_coefficient[div_order], tol$heterozygosity_abs,
        "population inbreeding coefficient")
    ))

    # FST validation separates implementation invariants from cross-method
    # diagnostics. Exact equality across packages is not assumed because
    # missing-data and multilocus aggregation conventions can differ.
    fst <- run_fst(gds, qc_ids, metadata)
    fst_genotype <- genotype[, match(qc_names, colnames(genotype)), drop = FALSE]
    manual_fst <- manual_wc84_fst(fst_genotype, metadata$population)
    pair_mask <- upper.tri(fst$matrix)
    pair_values <- fst$matrix[pair_mask]
    two_population_consistency <- if (length(pair_values) == 1L) {
      compare_validation_values(
        fst$global, pair_values, tol$fst_abs,
        "SNPRelate global and pairwise FST consistency"
      )
    } else {
      data.table::data.table(
        label = "SNPRelate pairwise FST matrix symmetry",
        passed = isTRUE(all.equal(fst$matrix, t(fst$matrix), tolerance = tol$fst_abs)),
        n = length(pair_values),
        max_absolute_difference = max(abs(fst$matrix - t(fst$matrix)), na.rm = TRUE),
        tolerance = tol$fst_abs,
        message = "matrix symmetry"
      )
    }
    checks <- data.table::rbindlist(list(checks, two_population_consistency))
    hf <- if (requireNamespace("hierfstat", quietly = TRUE))
      hierfstat_reference_fst(fst_genotype, metadata$population) else NULL
    manual_pair <- manual_fst$pairwise[rownames(fst$matrix), colnames(fst$matrix), drop = FALSE]
    fst_cross_method_diagnostic <- list(
      manual_global = manual_fst$global,
      snprelate_global = fst$global,
      manual_global_difference = fst$global - manual_fst$global,
      manual_pairwise_difference = fst$matrix - manual_pair,
      interpretation = paste(
        "Diagnostic only: exact agreement is not required until missing-data",
        "and multilocus aggregation conventions are demonstrated equivalent."
      )
    )
    hierfstat_diagnostic <- if (!is.null(hf)) list(
      global_difference = fst$global - hf$global,
      pairwise_difference = fst$matrix - hf$pairwise[rownames(fst$matrix), colnames(fst$matrix), drop = FALSE]
    ) else NULL

    integration_details <- list(
      variant_qc = vq, ld_variant_ids = ld_names, ibs = ibs,
      pca = pca[c("scores", "variance")],
      pca_standardized_dosage_diagnostic = standardized_pca_diagnostic,
      pca_covariance_eigendecomposition_diagnostic = covariance_eigendecomposition_diagnostic,
      pca_internal_consistency = pca_consistency,
      diversity = div[c("sample", "population")],
      fst = fst, manual_fst = manual_fst, hierfstat = hf,
      fst_cross_method_diagnostic = fst_cross_method_diagnostic,
      hierfstat_diagnostic = hierfstat_diagnostic
    )
  }
  list(passed = all(checks$passed), checks = checks, integration = integration_details,
    fixture = paths, tolerances = tol)
}
