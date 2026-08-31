# Folded site frequency spectrum (per population, segregating loci only) and
# the mode-shift bottleneck-detection test (Luikart and Cornuet 1998): under
# mutation-drift equilibrium, rare alleles are the most common class, so the
# lowest minor-allele-frequency class is expected to hold the most loci (an
# "L-shaped" distribution). A recent bottleneck preferentially removes rare
# alleles, which can shift the modal class away from the lowest one. This is
# the standard qualitative screening version of the test (as implemented in
# the BOTTLENECK software) -- a shifted mode is a signal worth investigating
# further, not a p-value or a confirmed bottleneck; unlike the classic
# heterozygosity-excess bottleneck test, it makes no assumption about the
# locus mutation model (infinite-alleles vs stepwise), so it is a clean fit
# for biallelic SNP data. Reuses `compute_diversity()`'s already-computed
# per-locus `maf`/`polymorphic` columns -- no new per-locus computation.

run_bottleneck_analysis <- function(locus_table, n_bins = 10L) {
  # method = "radix": locale-independent, so the written table's row order
  # is reproducible across machines regardless of ambient LC_COLLATE (a real
  # bug found in a pre-release audit -- population names with mixed case
  # otherwise sort differently under different locales).
  populations <- sort(unique(locus_table$population), method = "radix")
  bin_width <- 0.5 / n_bins
  bin_lower <- seq(0, 0.5 - bin_width, by = bin_width)
  bin_upper <- bin_lower + bin_width

  spectrum <- data.table::rbindlist(lapply(populations, function(p) {
    poly <- locus_table[population == p & polymorphic == TRUE & is.finite(maf) & maf > 0]
    bin <- if (nrow(poly)) {
      pmin(pmax(ceiling(poly$maf / bin_width), 1L), n_bins)
    } else integer()
    counts <- tabulate(bin, nbins = n_bins)
    data.table::data.table(
      population = p, bin = seq_len(n_bins),
      bin_lower = bin_lower, bin_upper = bin_upper, n_loci = counts
    )
  }))

  summary <- spectrum[, {
    n_poly <- sum(n_loci)
    mode_bin <- if (n_poly > 0L) bin[which.max(n_loci)][1L] else NA_integer_
    .(n_polymorphic_loci = n_poly,
      mode_bin = mode_bin,
      mode_bin_lower = if (!is.na(mode_bin)) bin_lower[match(mode_bin, bin)] else NA_real_,
      mode_bin_upper = if (!is.na(mode_bin)) bin_upper[match(mode_bin, bin)] else NA_real_,
      mode_shifted = if (!is.na(mode_bin)) mode_bin != 1L else NA)
  }, by = population]

  list(spectrum = spectrum, summary = summary, n_bins = n_bins)
}

plot_bottleneck <- function(result, cfg, dirs) {
  spec <- result$spectrum
  if (!nrow(spec) || !any(spec$n_loci > 0L)) return(invisible(NULL))
  spec <- data.table::copy(spec)
  spec[, bin_label := sprintf("%.2f-%.2f", bin_lower, bin_upper)]
  spec[, bin_label := factor(bin_label, levels = unique(bin_label[order(bin_lower)]))]
  spec[result$summary, is_mode := bin == i.mode_bin, on = "population"]
  spec[is.na(is_mode), is_mode := FALSE]

  style <- figure_style_name(cfg)
  accent <- unname(expand_figure_palette(figure_style_profile(style), 1L, "fills"))
  highlight <- if (identical(style, "grayscale-safe")) "#252525" else "#B2182B"

  p <- ggplot2::ggplot(spec, ggplot2::aes(bin_label, n_loci, fill = is_mode)) +
    ggplot2::geom_col(colour = "white", linewidth = 0.15) +
    ggplot2::scale_fill_manual(values = c(`TRUE` = highlight, `FALSE` = accent), guide = "none") +
    ggplot2::scale_y_continuous(labels = scales::label_comma(), expand = ggplot2::expansion(mult = c(0, 0.08))) +
    ggplot2::facet_wrap(~population, scales = "free_y") +
    ggplot2::labs(
      title = "Site frequency spectrum (folded) and mode-shift bottleneck screen",
      subtitle = "Highlighted bar: each population's modal minor-allele-frequency class",
      caption = "A mode away from the lowest class is a possible recent-bottleneck signature (Luikart and Cornuet 1998)",
      x = "Minor allele frequency class", y = "Segregating loci"
    ) +
    theme_publication(figure_base_size(cfg)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1, size = ggplot2::rel(0.65)))
  save_plot(p, "48_site_frequency_spectrum", dirs, cfg$output$figure_formats, 9, 6.5, cfg$output$dpi)
}
