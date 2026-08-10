# LD-based contemporary effective population size (Waples 2006; Waples & Do
# 2008, the "LDNe"/NeEstimator method). Formula and bias-correction
# coefficients verified against independent secondary sources (not just
# recalled) before shipping -- see NEWS.md for the exact citations checked.
# Given bias-corrected r-squared (r2_drift, attributable to drift alone,
# after subtracting the expected sampling-noise contribution):
#   Ne = (1/3 + sqrt(1/9 - 2.76 * r2_drift)) / (2 * r2_drift)
# The sampling-noise correction depends on harmonic-mean sample size S
# (Waples 2006):
#   S <= 30: E(r2) = 0.0018 + 0.907/S + 4.44/S^2
#   S >  30: E(r2) = 1/S
# The empirical bias correction was developed assuming rare alleles (< 5%
# frequency) are excluded -- satisfied here for free by this package's own
# QC MAF filter (qc.maf, default 0.05), no separate parameter needed.
ne_ld_bias_correction <- function(s) ifelse(s <= 30, 0.0018 + 0.907 / s + 4.44 / s^2, 1 / s)

ne_ld_from_r2_drift <- function(r2_drift) {
  if (!is.finite(r2_drift) || r2_drift <= 0) {
    # No detectable drift signal above sampling noise -- reported as
    # infinite, matching NeEstimator's own convention, not a fabricated
    # finite number.
    return(list(ne = Inf, status = "ok"))
  }
  discriminant <- 1 / 9 - 2.76 * r2_drift
  if (discriminant < 0) {
    # Implies an extremely small Ne, outside this quadratic formula's valid
    # domain -- reported as NA with an explicit status rather than a
    # complex/nonsensical root.
    return(list(ne = NA_real_, status = "below_formula_domain"))
  }
  list(ne = (1 / 3 + sqrt(discriminant)) / (2 * r2_drift), status = "ok")
}

# Cross-chromosome SNP pairs only: the drift-only interpretation requires
# effectively unlinked loci, and same-chromosome pairs can carry real
# physical linkage (exactly the opposite selection from ld_decay, which
# wants physically close pairs). Requires SNPRelate::snpgdsLDMat()'s "S"
# per-pair sample size is not exposed by this API, so the harmonic mean is
# taken over each pair's min(per-locus non-missing count) -- a documented
# approximation to the literature's joint-non-missing count, expected to
# differ only when two loci's missingness patterns don't fully overlap.
ne_ld_one_population <- function(gds, pop_sample_ids, snp_ids, ids, max_snps, seed, min_pairs = 10L) {
  insufficient <- function(status) data.table::data.table(
    n_samples = length(pop_sample_ids), n_snps = 0L, n_chromosomes = 0L, n_pairs = 0L,
    harmonic_mean_n = NA_real_, mean_r2 = NA_real_, mean_r2_drift = NA_real_,
    ne = NA_real_, ne_status = status
  )
  if (length(pop_sample_ids) < 2L) return(insufficient("fewer_than_two_samples"))

  chromosome <- as.character(ids$chromosome[match(snp_ids, ids$snp)])
  if (data.table::uniqueN(chromosome) < 2L) return(insufficient("fewer_than_two_chromosomes"))

  set.seed(seed)
  use_snps <- if (length(snp_ids) > max_snps) sort(sample(snp_ids, max_snps)) else snp_ids

  geno <- SNPRelate::snpgdsGetGeno(
    gds, sample.id = pop_sample_ids, snp.id = use_snps, snpfirstdim = FALSE, with.id = TRUE, verbose = FALSE
  )
  n_called <- colSums(!is.na(geno$genotype))

  z <- SNPRelate::snpgdsLDMat(
    gds, sample.id = pop_sample_ids, snp.id = use_snps, slide = -1, method = "r", with.id = TRUE, verbose = FALSE
  )
  chromosome_z <- as.character(ids$chromosome[match(z$snp.id, ids$snp)])
  n_called_z <- n_called[match(z$snp.id, geno$snp.id)]

  cross_chromosome <- outer(chromosome_z, chromosome_z, `!=`)
  include <- cross_chromosome & upper.tri(z$LD)
  n_pairs <- sum(include)
  if (n_pairs < min_pairs) return(insufficient("too_few_unlinked_pairs"))

  r2 <- (z$LD[include])^2
  pair_n <- outer(n_called_z, n_called_z, pmin)[include]
  harmonic_mean_n <- length(pair_n) / sum(1 / pair_n)
  mean_r2 <- mean(r2)
  r2_drift <- mean_r2 - ne_ld_bias_correction(harmonic_mean_n)
  fit <- ne_ld_from_r2_drift(r2_drift)

  data.table::data.table(
    n_samples = length(pop_sample_ids), n_snps = length(use_snps),
    n_chromosomes = data.table::uniqueN(chromosome_z), n_pairs = n_pairs,
    harmonic_mean_n = harmonic_mean_n, mean_r2 = mean_r2, mean_r2_drift = r2_drift,
    ne = fit$ne, ne_status = fit$status
  )
}

compute_ne_ld <- function(gds, sample_ids, snp_ids, ids, metadata, max_snps = 2000L, seed = 42L) {
  populations <- sort(unique(metadata$population))
  out <- data.table::rbindlist(lapply(populations, function(pop) {
    idx <- match(metadata[population == pop, sample], sample_ids)
    idx <- idx[!is.na(idx)]
    pop_sample_ids <- sample_ids[idx]
    result <- ne_ld_one_population(gds, pop_sample_ids, snp_ids, ids, max_snps, seed)
    result[, population := pop]
    result
  }))
  data.table::setcolorder(out, c("population", setdiff(names(out), "population")))
  out[]
}

plot_ne_ld <- function(result, cfg, dirs) {
  fmts <- cfg$output$figure_formats; dpi <- cfg$output$dpi
  style <- figure_style_name(cfg)
  finite <- result[is.finite(ne)]
  if (!nrow(finite)) return(invisible(NULL))

  ordered <- data.table::copy(finite)
  data.table::setorder(ordered, ne)
  ordered[, population := factor(population, levels = population)]
  p <- ggplot2::ggplot(ordered, ggplot2::aes(population, ne, colour = population)) +
    ggplot2::geom_point(size = 2.6) +
    ggplot2::scale_y_log10(labels = scales::label_comma()) +
    ggplot2::scale_colour_manual(values = population_palette(ordered$population, style)) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "LD-based effective population size",
      subtitle = "Waples & Do (2008); populations with too few unlinked marker pairs or a non-finite estimate are omitted",
      x = NULL, y = expression(hat(N)[e])
    ) + theme_publication(figure_base_size(cfg)) +
    ggplot2::theme(legend.position = "none")
  save_plot(p, "45_Ne_LD", dirs, fmts, 8, max(4, nrow(ordered) * 0.35), dpi)
}
