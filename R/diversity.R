compute_diversity <- function(gds, sample_ids, snp_ids, metadata, ids) {
  geno <- SNPRelate::snpgdsGetGeno(gds, sample.id = sample_ids, snp.id = snp_ids,
                                   snpfirstdim = FALSE, verbose = FALSE)
  called <- rowSums(!is.na(geno)); het <- rowSums(geno == 1, na.rm = TRUE)
  sample <- data.table::data.table(
    sample = public_sample_ids(metadata, sample_ids),
    vcf_sample = sample_ids,
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
  style <- figure_style_name(cfg)
  population_colours <- population_palette(div$sample$population, style)
  jitter_seed <- as.integer(cfg$compute$seed %||% 42L)
  p1 <- ggplot2::ggplot(div$sample, ggplot2::aes(population, observed_heterozygosity, fill = population)) +
    ggplot2::geom_boxplot(
      outlier.shape = NA, alpha = .30, width = 0.62,
      colour = "#333333", linewidth = 0.55
    ) +
    ggplot2::geom_point(
      ggplot2::aes(colour = population),
      position = ggplot2::position_jitter(
        width = .12, height = 0, seed = jitter_seed
      ),
      size = 1.8, alpha = .72
    ) +
    ggplot2::scale_fill_manual(values = population_colours) +
    ggplot2::scale_colour_manual(values = population_colours) +
    ggplot2::labs(
      title = "Observed heterozygosity by population",
      x = "Population", y = expression(italic(H)[O])
    ) +
    theme_publication(figure_base_size(cfg)) +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = 35, hjust = 1)
    )
  save_plot(p1, "05_sample_heterozygosity", dirs, fmts, 8, 5.5, dpi)
  long <- data.table::melt(div$population[, .(population, observed_heterozygosity, expected_heterozygosity)],
                           id.vars = "population", variable.name = "metric", value.name = "value")
  if (nrow(ci)) {
    intervals <- data.table::copy(data.table::as.data.table(ci))
    intervals[, metric := c(
      Ho = "observed_heterozygosity",
      He = "expected_heterozygosity"
    )[as.character(metric)]]
    intervals <- intervals[
      !is.na(metric), .(population, metric, lower, upper)
    ]
    long <- merge(
      long, intervals, by = c("population", "metric"),
      all.x = TRUE, sort = FALSE
    )
  } else {
    long[, `:=`(lower = NA_real_, upper = NA_real_)]
  }
  metric_colours <- diversity_metric_palette(style)
  dodge <- ggplot2::position_dodge(width = 0.55)
  p2 <- ggplot2::ggplot(
    long,
    ggplot2::aes(
      population, value, fill = metric, colour = metric, shape = metric
    )
  ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = lower, ymax = upper),
      position = dodge, width = 0.14, linewidth = 0.6,
      na.rm = TRUE, show.legend = FALSE
    ) +
    ggplot2::geom_point(
      position = dodge, size = 3.1, stroke = 0.55
    ) +
    ggplot2::scale_fill_manual(
      values = metric_colours,
      breaks = names(metric_colours),
      labels = c("Observed heterozygosity", "Expected heterozygosity")
    ) +
    ggplot2::scale_colour_manual(
      values = metric_colours,
      breaks = names(metric_colours),
      labels = c("Observed heterozygosity", "Expected heterozygosity")
    ) +
    ggplot2::scale_shape_manual(
      values = c(
        observed_heterozygosity = 21L,
        expected_heterozygosity = 24L
      ),
      breaks = names(metric_colours),
      labels = c("Observed heterozygosity", "Expected heterozygosity")
    ) +
    ggplot2::labs(
      title = "Population genetic diversity", x = "Population",
      subtitle = if (any(is.finite(long$lower) & is.finite(long$upper))) {
        "Points are estimates; error bars are 95% chromosome-block bootstrap intervals"
      } else {
        "Points are population estimates; confidence intervals were not available"
      },
      y = "Heterozygosity", fill = "Statistic",
      colour = "Statistic", shape = "Statistic"
    ) +
    theme_publication(figure_base_size(cfg)) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 35, hjust = 1)
    )
  save_plot(p2, "06_population_diversity", dirs, fmts, 8, 5.5, dpi)
}
