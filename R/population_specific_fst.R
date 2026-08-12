# Population-specific FST (Weir and Goudet 2017, "A unified characterization
# of population structure and relatedness", Genetics): gives each population
# its own single differentiation value (beta_i), rather than only a global
# scalar or pairwise matrix -- a population whose allele frequencies sit far
# from the multi-population average gets a high beta_i (it contributes a lot
# to overall structure); a population close to the average gets a low
# beta_i. The sample-size-weighted mean of the beta_i values equals the
# overall population FST (beta_W), directly analogous to and complementing
# the existing global/pairwise FST and Jost's D already in this module.
#
# Uses hierfstat::betas() (Goudet's own reference implementation of the
# method, already an optional Suggests dependency of this package via
# allelic richness) directly rather than re-deriving the estimator by hand --
# reuses the same hierfstat_encode_genotype() helper diversity.R's allelic
# richness already uses. Skips transparently (like allelic richness and
# every other optional-hierfstat feature) when hierfstat is not installed.

compute_population_specific_fst <- function(genotype, population, snp_ids = NULL) {
  empty <- list(
    available = FALSE, table = data.table::data.table(population = character(), beta = numeric()),
    overall = NA_real_
  )
  if (data.table::uniqueN(population) < 2L) return(empty)
  if (!requireNamespace("hierfstat", quietly = TRUE)) return(empty)

  encoded <- hierfstat_encode_genotype(genotype)
  colnames(encoded) <- if (!is.null(snp_ids)) as.character(snp_ids) else paste0("snp_", seq_len(ncol(genotype)))
  dat <- data.frame(pop = population, encoded, check.names = FALSE)
  res <- hierfstat::betas(dat, nboot = 0)
  list(
    available = TRUE,
    table = data.table::data.table(population = names(res$betaiovl), beta = as.numeric(res$betaiovl)),
    overall = res$betaW
  )
}

plot_population_specific_fst <- function(result, cfg, dirs) {
  if (!isTRUE(result$available) || !nrow(result$table)) return(invisible(NULL))
  style <- figure_style_name(cfg)
  population_colours <- population_palette(result$table$population, style)
  p <- ggplot2::ggplot(
    result$table, ggplot2::aes(stats::reorder(population, -beta), beta, fill = population)
  ) +
    ggplot2::geom_col(colour = "#1A1A1A", linewidth = 0.3, width = 0.7) +
    ggplot2::geom_hline(yintercept = result$overall, linetype = "dashed", colour = "#595959", linewidth = 0.5) +
    ggplot2::scale_fill_manual(values = population_colours, guide = "none") +
    ggplot2::labs(
      title = "Population-specific FST (Weir and Goudet 2017)",
      subtitle = sprintf("Dashed line: overall population FST (beta_W = %.4f)", result$overall),
      x = "Population", y = expression(beta[i])
    ) +
    theme_publication(figure_base_size(cfg)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1))
  save_plot(p, "51_population_specific_fst", dirs, cfg$output$figure_formats, 7.5, 5.5, cfg$output$dpi)
}
