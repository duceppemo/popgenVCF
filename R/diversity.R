compute_diversity <- function(gds, sample_ids, snp_ids, metadata, ids, hwe_alpha = 0.05,
                               compute_allelic_richness = TRUE, gds_path = NULL, threads = 1L) {
  # SNPRelate::snpgdsGetGeno() silently returns rows/columns in the GDS's own
  # native storage order (verified empirically, same behavior class as
  # snpgdsSampMissRate()'s with.id ordering found for sex_check), not the
  # requested sample.id/snp.id order -- confirmed harmless here only because
  # sample_ids/snp_ids are always order-preserving subsequences of
  # ids$sample/ids$snp (filtered, never reordered, throughout QC), so
  # requested order and native order coincide. Every positional use of
  # `geno`'s columns against `snp_ids` below depends on that invariant.
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
  populations <- sort(unique(metadata$population))

  # Split out so the per-population computation can run either against the
  # shared `gds` handle (sequential path) or a worker's own independent GDS
  # connection (parallel path, below) -- everything except the
  # snpgdsHWE() call operates on `geno`, already fully extracted into memory
  # above, so `gds_conn` is only ever touched for that one read.
  diversity_locus_stats <- function(pop, gds_conn) {
    smp <- metadata[population == pop, sample]
    idx <- match(smp, sample_ids); idx <- idx[!is.na(idx)]
    x <- geno[idx, , drop = FALSE]
    n_called <- colSums(!is.na(x)); gene_copies <- 2 * n_called
    alt_count <- colSums(x, na.rm = TRUE)
    p <- ifelse(gene_copies > 0, alt_count / gene_copies, NA_real_)
    ho <- ifelse(n_called > 0, colSums(x == 1, na.rm = TRUE) / n_called, NA_real_)
    he <- 2 * p * (1 - p)
    he_unbiased <- ifelse(gene_copies > 1, he * gene_copies / (gene_copies - 1), NA_real_)
    # Kimura and Crow's (1964) effective number of alleles, Ae = 1 / (1 - He),
    # from the unbiased He estimate for consistency with the rest of this
    # table. Bounded in [1, 2] for a biallelic locus at the true population
    # allele frequency, but the small-sample bias correction on He_unbiased
    # can push it to (or, for very small n, past) 1 -- reported as Inf, not a
    # fabricated finite number, matching this codebase's Nm/Ne(LD) convention.
    effective_alleles <- ifelse(is.na(he_unbiased), NA_real_,
                                ifelse(he_unbiased < 1, 1 / (1 - he_unbiased), Inf))
    polymorphic <- is.finite(p) & p > 0 & p < 1
    # SNPRelate's exact HWE test (Wigginton et al. 2005) reports a trivial p = 1
    # for monomorphic input; report NA instead of p = 1 for loci monomorphic
    # within this specific population, since "not tested" is more honest than a
    # spurious pile-up of p = 1 in the summary counts and histogram.
    hwe_pvalue <- SNPRelate::snpgdsHWE(gds_conn, sample.id = sample_ids[idx], snp.id = snp_ids)
    hwe_pvalue[!polymorphic] <- NA_real_
    data.table::data.table(
      population = pop, snp_id = snp_ids,
      chromosome = ids$chromosome[match(snp_ids, ids$snp)],
      position = ids$position[match(snp_ids, ids$snp)],
      n_called = n_called, alternate_allele_count = alt_count,
      alternate_allele_frequency = p, maf = pmin(p, 1 - p),
      observed_heterozygosity = ho, expected_heterozygosity = he,
      unbiased_expected_heterozygosity = he_unbiased,
      effective_alleles = effective_alleles,
      polymorphic = polymorphic, hwe_pvalue = hwe_pvalue
    )
  }

  # A real production incident: on a real 50-sample/561,767-locus cohort,
  # this loop's dominant cost (a fresh snpgdsHWE() scan per population) made
  # `diversity` alone take 57 minutes. Parallelizing across populations is
  # only safe with `gds_path` set: gdsfmt's own documentation warns that
  # `mclapply`'s forked children inherit the *same* underlying file
  # descriptor as the parent, sharing its current read position -- "wrong
  # reading, even program crashes" -- when they read the same GDS file
  # concurrently. Rather than relying on gdsfmt's own `allow.fork` mitigation
  # on a single shared handle (undocumented in enough detail to trust
  # blindly), each worker opens its own fully independent GDS connection, the
  # unambiguously safe pattern -- verified to reproduce the exact sequential
  # result before shipping (see test-diversity-parallel.R).
  workers <- if (is.null(gds_path)) 1L else fork_worker_count(length(populations), threads)
  loci <- if (workers <= 1L) {
    lapply(populations, function(pop) diversity_locus_stats(pop, gds))
  } else {
    results <- parallel::mclapply(populations, function(pop) {
      worker_gds <- SNPRelate::snpgdsOpen(
        gds_path, readonly = TRUE, allow.duplicate = TRUE, allow.fork = TRUE
      )
      on.exit(SNPRelate::snpgdsClose(worker_gds), add = TRUE)
      diversity_locus_stats(pop, worker_gds)
    }, mc.cores = workers, mc.preschedule = FALSE, mc.set.seed = FALSE)
    check_mclapply_results(results, populations, "diversity computation")
    results
  }
  locus <- data.table::rbindlist(loci)

  # Private alleles: an allele is private to a population when every copy of it,
  # genome-wide across all retained populations, is found in that one population.
  # Requires at least two populations to compare against; standard presence/
  # absence definition matching hierfstat's/poppr's private.alleles().
  if (data.table::uniqueN(locus$population) >= 2L) {
    locus[, reference_allele_count := 2L * n_called - alternate_allele_count]
    locus[, total_alt := sum(alternate_allele_count), by = snp_id]
    locus[, total_ref := sum(reference_allele_count), by = snp_id]
    locus[, private_allele := data.table::fcase(
      alternate_allele_count > 0 & alternate_allele_count == total_alt &
        reference_allele_count > 0 & reference_allele_count == total_ref, "both",
      alternate_allele_count > 0 & alternate_allele_count == total_alt, "alt",
      reference_allele_count > 0 & reference_allele_count == total_ref, "ref",
      default = "none"
    )]
    locus[, c("total_alt", "total_ref") := NULL]
  } else {
    locus[, reference_allele_count := 2L * n_called - alternate_allele_count]
    locus[, private_allele := NA_character_]
  }

  # Rarefaction-corrected allelic richness (hierfstat::allelic.richness(),
  # Suggests-only -- optional, like the LEA/ADMIXTURE/fastStructure ancestry
  # backends -- so this skips transparently rather than erroring when
  # hierfstat is not installed). Matched back to locus/population by NAME
  # (hierfstat preserves the exact colnames() passed in as Ar's rownames,
  # verified empirically), never positionally, matching this codebase's
  # established discipline for every SNPRelate/hierfstat function whose
  # return order is not guaranteed to match the request.
  #
  # A real, measured cost, not a theoretical one: hierfstat::allelic.richness()
  # alone took 3.08s on a tiny 60-sample/2000-SNP synthetic benchmark fixture
  # (bisected as the sole cause of a 9-12x runtime / ~4x memory regression in
  # the pipeline-core-analyses continuous benchmark after this was added
  # unconditionally). compute_allelic_richness lets callers that don't need
  # this column (population_assignment's reuse of compute_diversity(),
  # scientific_validation's internal reference check, and the continuous
  # benchmark harness, which must keep measuring the same core PCA/IBS/
  # diversity/FST cost it always has for the historical trend to stay
  # meaningful) skip it; run_module_diversity, the only caller that writes
  # allelic richness to output tables/figures, still defaults it on.
  locus[, allelic_richness := NA_real_]
  allelic_richness_available <- FALSE
  if (isTRUE(compute_allelic_richness) && requireNamespace("hierfstat", quietly = TRUE)) {
    population_factor <- metadata[match(sample_ids, sample), population]
    encoded <- hierfstat_encode_genotype(geno)
    colnames(encoded) <- as.character(snp_ids)
    ar <- hierfstat::allelic.richness(data.frame(pop = population_factor, encoded, check.names = FALSE))
    ar_idx <- match(as.character(snp_ids), rownames(ar$Ar))
    if (!anyNA(ar_idx)) {
      ar_matrix <- as.matrix(ar$Ar[ar_idx, , drop = FALSE])
      ar_long <- data.table::data.table(
        snp_id = rep(snp_ids, times = ncol(ar_matrix)),
        population = rep(colnames(ar_matrix), each = nrow(ar_matrix)),
        allelic_richness = as.vector(ar_matrix)
      )
      locus[ar_long, allelic_richness := i.allelic_richness, on = c("population", "snp_id")]
      allelic_richness_available <- TRUE
    }
  }

  population <- locus[, {
    mho <- mean(observed_heterozygosity, na.rm = TRUE)
    mhe <- mean(unbiased_expected_heterozygosity, na.rm = TRUE)
    tested <- is.finite(hwe_pvalue)
    fdr <- if (any(tested)) stats::p.adjust(hwe_pvalue[tested], method = "BH") else numeric()
    ar_finite <- allelic_richness[is.finite(allelic_richness)]
    ae_finite <- effective_alleles[is.finite(effective_alleles)]
    .(n_samples = metadata[population == .BY$population, .N],
      n_loci = .N,
      polymorphic_loci = sum(polymorphic, na.rm = TRUE),
      polymorphic_fraction = mean(polymorphic, na.rm = TRUE),
      observed_heterozygosity = mho,
      expected_heterozygosity = mhe,
      inbreeding_coefficient = if (is.finite(mhe) && mhe > 0) 1 - mho / mhe else NA_real_,
      mean_minor_allele_frequency = mean(maf, na.rm = TRUE),
      mean_locus_call_rate = mean(n_called / metadata[population == .BY$population, .N], na.rm = TRUE),
      hwe_tested_loci = sum(tested),
      hwe_significant_loci = sum(hwe_pvalue[tested] < hwe_alpha),
      hwe_significant_loci_fdr = sum(fdr < hwe_alpha),
      private_allele_loci = sum(private_allele != "none", na.rm = TRUE),
      mean_allelic_richness = if (length(ar_finite)) mean(ar_finite) else NA_real_,
      mean_effective_alleles = if (length(ae_finite)) mean(ae_finite) else NA_real_)
  }, by = population]
  list(genotype = geno, sample = sample, locus = locus, population = population,
       allelic_richness_available = allelic_richness_available,
       allelic_richness_min_alleles = if (allelic_richness_available) ar$min.all else NA_real_)
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

  hwe_alpha <- cfg$analyses$hwe_alpha %||% 0.05
  tested <- if (!is.null(div$locus) && "hwe_pvalue" %in% names(div$locus)) {
    div$locus[is.finite(hwe_pvalue)]
  } else {
    data.table::data.table()
  }
  if (nrow(tested)) {
    accent <- unname(expand_figure_palette(figure_style_profile(style), 1L, "fills"))
    p3 <- ggplot2::ggplot(tested, ggplot2::aes(hwe_pvalue)) +
      ggplot2::geom_histogram(
        bins = 30, fill = accent, colour = "white", linewidth = 0.2
      ) +
      ggplot2::geom_vline(
        xintercept = hwe_alpha, colour = "#B2182B",
        linetype = "dashed", linewidth = 0.65
      ) +
      ggplot2::scale_y_continuous(
        labels = scales::label_comma(),
        expand = ggplot2::expansion(mult = c(0, 0.06))
      ) +
      ggplot2::facet_wrap(~population) +
      ggplot2::labs(
        title = "Hardy-Weinberg equilibrium exact-test p-values",
        subtitle = sprintf("Dashed line: significance threshold (alpha = %.3g); monomorphic-within-population loci excluded", hwe_alpha),
        x = "Exact-test p-value", y = "Number of loci"
      ) + theme_publication(figure_base_size(cfg))
    save_plot(p3, "19_HWE_pvalues", dirs, fmts, 8, 5, dpi)
  }

  if (sum(div$population$private_allele_loci) > 0L) {
    p4 <- ggplot2::ggplot(div$population, ggplot2::aes(population, private_allele_loci, fill = population)) +
      ggplot2::geom_col(width = 0.62) +
      ggplot2::scale_fill_manual(values = population_colours) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.08))) +
      ggplot2::labs(
        title = "Private alleles by population",
        x = "Population", y = "Loci with a private allele"
      ) +
      theme_publication(figure_base_size(cfg)) +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_text(angle = 35, hjust = 1)
      )
    save_plot(p4, "20_private_alleles", dirs, fmts, 7, 5, dpi)
  }

  if (isTRUE(div$allelic_richness_available) &&
      !is.null(div$locus) && "allelic_richness" %in% names(div$locus) &&
      any(is.finite(div$locus$allelic_richness))) {
    ar_tested <- div$locus[is.finite(allelic_richness)]
    p5 <- ggplot2::ggplot(ar_tested, ggplot2::aes(population, allelic_richness, fill = population)) +
      ggplot2::geom_boxplot(
        outlier.shape = NA, alpha = .30, width = 0.62,
        colour = "#333333", linewidth = 0.55
      ) +
      ggplot2::geom_point(
        ggplot2::aes(colour = population),
        position = ggplot2::position_jitter(width = .12, height = 0, seed = jitter_seed),
        size = 1.3, alpha = .5
      ) +
      ggplot2::scale_fill_manual(values = population_colours) +
      ggplot2::scale_colour_manual(values = population_colours) +
      ggplot2::labs(
        title = "Allelic richness by population",
        subtitle = sprintf(
          "Rarefied to %s allele copies (hierfstat::allelic.richness())",
          scales::comma(div$allelic_richness_min_alleles)
        ),
        x = "Population", y = "Allelic richness"
      ) +
      theme_publication(figure_base_size(cfg)) +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_text(angle = 35, hjust = 1)
      )
    save_plot(p5, "44_allelic_richness", dirs, fmts, 8, 5.5, dpi)
  }
}
