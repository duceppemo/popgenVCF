compute_diversity <- function(gds, sample_ids, snp_ids, metadata, ids) {
  geno <- SNPRelate::snpgdsGetGeno(gds, sample.id = sample_ids, snp.id = snp_ids,
                                   snpfirstdim = FALSE, verbose = FALSE)
  called <- rowSums(!is.na(geno)); het <- rowSums(geno == 1, na.rm = TRUE)
  sample <- data.table::data.table(
    sample = sample_ids,
    population = metadata[match(sample_ids, sample), population],
    loci_called = called,
    missing_rate = ifelse(ncol(geno) > 0, 1 - called / ncol(geno), NA_real_),
    observed_heterozygosity = ifelse(called > 0, het / called, NA_real_),
    heterozygous_calls = het,
    homozygous_reference_calls = rowSums(geno == 0, na.rm = TRUE),
    homozygous_alternate_calls = rowSums(geno == 2, na.rm = TRUE)
  )
  loci <- lapply(sort(unique(metadata$population)), function(pop) {
    smp <- metadata[population == pop, sample]
    idx <- match(smp, sample_ids); idx <- idx[!is.na(idx)]
    x <- geno[idx, , drop = FALSE]
    n_called <- colSums(!is.na(x)); gene_copies <- 2 * n_called
    alt_count <- colSums(x, na.rm = TRUE)
    p <- ifelse(gene_copies > 0, alt_count / gene_copies, NA_real_)
    ho <- ifelse(n_called > 0, colSums(x == 1, na.rm = TRUE) / n_called, NA_real_)
    he <- 2 * p * (1 - p)
    he_unbiased <- ifelse(gene_copies > 1, he * gene_copies / (gene_copies - 1), NA_real_)
    data.table::data.table(
      population = pop, snp_id = snp_ids,
      chromosome = ids$chromosome[match(snp_ids, ids$snp)],
      position = ids$position[match(snp_ids, ids$snp)],
      n_called = n_called, alternate_allele_count = alt_count,
      alternate_allele_frequency = p, maf = pmin(p, 1 - p),
      observed_heterozygosity = ho, expected_heterozygosity = he,
      unbiased_expected_heterozygosity = he_unbiased,
      polymorphic = is.finite(p) & p > 0 & p < 1
    )
  })
  locus <- data.table::rbindlist(loci)
  population <- locus[, {
    mho <- mean(observed_heterozygosity, na.rm = TRUE)
    mhe <- mean(unbiased_expected_heterozygosity, na.rm = TRUE)
    .(n_samples = metadata[population == .BY$population, .N],
      n_loci = .N,
      polymorphic_loci = sum(polymorphic, na.rm = TRUE),
      polymorphic_fraction = mean(polymorphic, na.rm = TRUE),
      observed_heterozygosity = mho,
      expected_heterozygosity = mhe,
      inbreeding_coefficient = if (is.finite(mhe) && mhe > 0) 1 - mho / mhe else NA_real_,
      mean_minor_allele_frequency = mean(maf, na.rm = TRUE),
      mean_locus_call_rate = mean(n_called / metadata[population == .BY$population, .N], na.rm = TRUE))
  }, by = population]
  list(genotype = geno, sample = sample, locus = locus, population = population)
}

bootstrap_diversity <- function(locus_stats, replicates, seed, unit = "chromosome") {
  if (replicates <= 0L) return(data.table::data.table())
  set.seed(seed)
  pops <- unique(locus_stats$population)
  out <- lapply(pops, function(pop) {
    x <- locus_stats[population == pop]
    groups <- if (unit == "chromosome") split(seq_len(nrow(x)), x$chromosome) else as.list(seq_len(nrow(x)))
    if (length(groups) < 2L) return(data.table::data.table(population = pop, metric = character(), estimate = numeric(), lower = numeric(), upper = numeric()))
    boot <- replicate(replicates, {
      chosen <- sample(seq_along(groups), length(groups), replace = TRUE)
      idx <- unlist(groups[chosen], use.names = FALSE)
      c(Ho = mean(x$observed_heterozygosity[idx], na.rm = TRUE),
        He = mean(x$unbiased_expected_heterozygosity[idx], na.rm = TRUE))
    })
    est <- c(Ho = mean(x$observed_heterozygosity, na.rm = TRUE), He = mean(x$unbiased_expected_heterozygosity, na.rm = TRUE))
    data.table::data.table(population = pop, metric = names(est), estimate = est,
                           lower = apply(boot, 1, stats::quantile, 0.025, na.rm = TRUE),
                           upper = apply(boot, 1, stats::quantile, 0.975, na.rm = TRUE))
  })
  data.table::rbindlist(out, fill = TRUE)
}

plot_diversity <- function(div, ci, cfg, dirs) {
  fmts <- cfg$output$figure_formats; dpi <- cfg$output$dpi
  p1 <- ggplot2::ggplot(div$sample, ggplot2::aes(population, observed_heterozygosity, fill = population)) +
    ggplot2::geom_boxplot(outlier.shape = NA, alpha = .75) + ggplot2::geom_jitter(width = .15, alpha = .65) +
    ggplot2::scale_fill_manual(values = population_palette(div$sample$population)) +
    ggplot2::labs(title = "Observed heterozygosity by population", x = "Population", y = expression(H[O])) +
    theme_publication() + ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_text(angle = 35, hjust = 1))
  save_plot(p1, "05_sample_heterozygosity", dirs, fmts, 8, 5.5, dpi)
  long <- data.table::melt(div$population[, .(population, observed_heterozygosity, expected_heterozygosity)],
                           id.vars = "population", variable.name = "metric", value.name = "value")
  p2 <- ggplot2::ggplot(long, ggplot2::aes(population, value, fill = metric)) +
    ggplot2::geom_col(position = "dodge") + ggplot2::labs(title = "Population genetic diversity", x = "Population", y = "Heterozygosity") +
    theme_publication() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1))
  save_plot(p2, "06_population_diversity", dirs, fmts, 8, 5.5, dpi)
}
