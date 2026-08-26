# Genome-wide outlier scan for local adaptation/selection (Luu et al. 2016;
# Prive et al. 2020), via the `pcadapt` package (a new optional Suggests
# dependency). Found by a standard-toolkit gap audit and confirmed genuinely
# distinct from the existing genome_scan module's windowed FST table: that
# table is confirmed (by reading its own code) to be just the top-20
# windows by raw FST magnitude, with no null distribution -- its own figure
# subtitle already says so ("exploratory outlier flagging, not a
# significance test"). pcadapt instead is a real per-locus statistical
# test with a calibrated null: PCA is fit on the genotype matrix, each
# locus's genotypes are regressed on the K retained PC scores to get a
# vector of z-scores, and a robust Mahalanobis distance of that vector
# (genomic-control corrected via pcadapt's own `gif`) is tested against a
# chi-squared distribution with K degrees of freedom.
#
# Notably, and unlike almost every other module in this pipeline, pcadapt
# does not require population metadata at all -- it is an unsupervised,
# PCA-based method that works directly from genotypes. Population count is
# used only as an optional heuristic for choosing K when the user has not
# configured one explicitly (see resolve_pcadapt_k()); the module still
# runs correctly with no population column present, and is deliberately not
# added to R/metadata_capabilities.R's population-gated module lists.
#
# Operates on the same full QC-passing, *not* LD-pruned, marker set
# genome_scan/AMOVA/clonality reuse from compute_diversity() -- but,
# deliberately, extracted independently in run_module_pcadapt() rather than
# depending on the "diversity" module's own output: diversity is
# population-gated, and pcadapt must not be (see pcadapt_module_spec()).
# Using the full unpruned marker set matches pcadapt's own documented
# practice of scanning for outliers across the whole panel rather than a
# structure-optimized pruned subset, which could remove the very loci a
# selection scan is looking for. `LD.clumping` (pcadapt's own optional
# local-LD thinning refinement) is not enabled by default in this first
# version -- an honest, documented v1 scope decision, not an oversight.
#
# FDR control uses stats::p.adjust(method = "BH"), the base-R equivalent of
# Storey's q-value (the Bioconductor `qvalue` package every pcadapt
# tutorial uses) -- deliberately chosen to avoid a Bioconductor dependency
# chain this package does not otherwise need.

# K defaults to (number of populations - 1) when population metadata is
# available with at least two populations -- a standard population-genetics
# heuristic (the number of structure-describing PCs tends to track group
# count minus one, the same logic behind why DAPC often needs K near
# cluster count - 1 in PCA space) -- or pcadapt's own function default of 2
# otherwise. Always bounded to a numerically sane range.
resolve_pcadapt_k <- function(configured_k, n_populations, n_samples, n_snps) {
  k <- if (!is.null(configured_k) && is.finite(configured_k)) {
    as.integer(configured_k)
  } else if (is.finite(n_populations) && n_populations >= 2L) {
    as.integer(n_populations - 1L)
  } else {
    2L
  }
  max(1L, min(k, n_samples - 1L, n_snps - 1L, 10L))
}

run_pcadapt_scan <- function(genotype, snp_ids, chromosome, position, k = NULL,
                             n_populations = NA_integer_, min_maf = 0.05,
                             fdr_alpha = 0.05) {
  if (ncol(genotype) < 2L) {
    stop("pcadapt outlier scan requires at least two loci; ", ncol(genotype), " available", call. = FALSE)
  }
  k_used <- resolve_pcadapt_k(k, n_populations, nrow(genotype), ncol(genotype))
  # pcadapt defaults on for every user and needs no population metadata --
  # unlike ml_tree (opt-in, where a hard failure is the right, loud
  # outcome), a missing optional package here must degrade the same way the
  # numerical-stability failure below already does, not stop() and take the
  # whole pipeline down with it (a real production incident: a v1.0.0
  # container image that omitted the optional pcadapt conda package lost an
  # entire real run's ~45 minutes of completed upstream results to this
  # module's stop() alone).
  if (!requireNamespace("pcadapt", quietly = TRUE)) {
    empty <- data.table::data.table(
      snp_id = snp_ids, chromosome = chromosome, position = position,
      maf = NA_real_, mahalanobis_stat = NA_real_, chi2_stat = NA_real_,
      p_value = NA_real_, q_value = NA_real_, outlier = FALSE
    )
    return(list(
      table = empty, k = k_used, gif = NA_real_, n_tested = 0L, n_outliers = 0L,
      failed = TRUE, reason = "package_missing"
    ))
  }

  # pcadapt expects loci (rows) x samples (columns); compute_diversity()'s
  # genotype matrix is samples (rows) x loci (columns) like everywhere else
  # in this codebase, hence the transpose.
  mat <- t(genotype)
  gt <- pcadapt::read.pcadapt(mat, type = "pcadapt")
  # A real, directly-observed numerical-stability failure mode (matching
  # ml_tree's own precedent for a different third-party method): pcadapt's
  # internal per-locus regression against K PC scores can hit an exactly
  # singular design matrix with too little data relative to K (confirmed
  # against this package's own tiny CI validation fixture, 8 samples x 5
  # loci: "system is computationally singular"). Unlike ml_tree (an
  # explicitly opt-in module where a hard failure is the right, loud
  # outcome), pcadapt defaults on for every user, so the graceful-skip
  # convention this codebase already uses for other too-small-data cases
  # (sex_check, ne_ld, Mantel/IBD) is the correct choice here, not a hard
  # error that would break the whole pipeline over one exploratory module.
  fit <- tryCatch(
    pcadapt::pcadapt(gt, K = k_used, method = "mahalanobis", min.maf = min_maf),
    error = function(e) NULL
  )
  if (is.null(fit)) {
    empty <- data.table::data.table(
      snp_id = snp_ids, chromosome = chromosome, position = position,
      maf = NA_real_, mahalanobis_stat = NA_real_, chi2_stat = NA_real_,
      p_value = NA_real_, q_value = NA_real_, outlier = FALSE
    )
    return(list(
      table = empty, k = k_used, gif = NA_real_, n_tested = 0L, n_outliers = 0L,
      failed = TRUE, reason = "numerical_instability"
    ))
  }

  # fit$pvalues/$maf/$stat/$chi2.stat are all returned in the same order and
  # length as the input loci (NA-filled for loci pcadapt's own min.maf
  # filter excluded), so no re-indexing via fit$pass is needed here.
  q_value <- stats::p.adjust(fit$pvalues, method = "BH")
  table <- data.table::data.table(
    snp_id = snp_ids, chromosome = chromosome, position = position,
    maf = fit$maf, mahalanobis_stat = fit$stat, chi2_stat = fit$chi2.stat,
    p_value = fit$pvalues, q_value = q_value,
    outlier = !is.na(q_value) & q_value < fdr_alpha
  )
  list(
    table = table, k = k_used, gif = fit$gif,
    n_tested = sum(!is.na(fit$pvalues)), n_outliers = sum(table$outlier, na.rm = TRUE),
    failed = FALSE
  )
}

plot_pcadapt <- function(result, cfg, dirs) {
  tested <- result$table[!is.na(p_value)]
  if (!nrow(tested)) return(invisible(NULL))
  fmts <- cfg$output$figure_formats; dpi <- cfg$output$dpi
  style <- figure_style_name(cfg)
  profile <- figure_style_profile(style)

  layout <- manhattan_layout(tested$chromosome, tested$position)
  bp_breaks <- manhattan_bp_breaks(tested$chromosome, tested$position, layout$offset)
  tested <- data.table::copy(tested)
  tested[, x := layout$x]
  tested[, neg_log10_p := -log10(pmax(p_value, .Machine$double.xmin))]
  colours <- expand_figure_palette(profile, 2L, "colours")
  highlight <- if (identical(style, "grayscale-safe")) "#252525" else "#B2182B"

  p <- ggplot2::ggplot(tested, ggplot2::aes(x = x, y = neg_log10_p)) +
    ggplot2::geom_point(
      data = tested[outlier == FALSE],
      ggplot2::aes(colour = factor(match(chromosome, layout$ticks$chromosome) %% 2L)),
      size = 1, alpha = .7, show.legend = FALSE
    ) +
    ggplot2::scale_colour_manual(values = colours) +
    ggplot2::geom_point(
      data = tested[outlier == TRUE], colour = highlight, size = 1.4, alpha = .9
    ) +
    ggplot2::scale_x_continuous(breaks = bp_breaks$x, labels = bp_breaks$label) +
    ggplot2::labs(
      title = "pcadapt outlier scan",
      subtitle = sprintf(
        "Robust Mahalanobis distance test (K = %d); highlighted points: %s significant at q < %s (Benjamini-Hochberg FDR)",
        result$k, scales::comma(result$n_outliers), format(cfg$analyses$pcadapt_fdr_alpha)
      ),
      caption = sprintf("Genomic inflation factor (gif) = %.3f", result$gif),
      x = "Chromosome position", y = expression(-log[10](italic(p)))
    ) + theme_publication(figure_base_size(cfg))
  p <- manhattan_chromosome_row(
    p, layout$ticks, range(tested$neg_log10_p, na.rm = TRUE), figure_base_size(cfg),
    plot_width_in = 10, panel_height_in = 4.5
  )
  save_plot(p, "59_pcadapt_manhattan", dirs, fmts, 10, 4.5, dpi)
}
