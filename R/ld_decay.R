# Uses the QC-passed, autosome-restricted SNP set (context$qc_snps), never the
# LD-pruned set (context$final_snps): the pruned set is specifically selected
# to have LOW pairwise LD by construction, so decay computed on it would be a
# systematic, meaningless underestimate. This mirrors run_genome_scan_fst()'s
# and compute_diversity()'s existing choice of context$qc_snps.
compute_ld_decay <- function(gds, sample_ids, snp_ids, ids, max_distance_bp, bin_bp, slide) {
  empty_binned <- function() data.table::data.table(
    distance_bin_start = integer(), distance_bin_end = integer(),
    n_pairs = integer(), mean_r2 = numeric()
  )
  chromosome <- as.character(ids$chromosome[match(snp_ids, ids$snp)])
  position <- as.numeric(ids$position[match(snp_ids, ids$snp)])
  # SNPRelate::snpgdsLDMat() bands adjacency by the ORDER of the snp.id vector
  # passed in, not by chromosome/position -- explicit sort here (rather than
  # trusting GDS file order) so cross-chromosome pairs are only ever adjacent
  # at true chromosome boundaries, where the chromosome_i != chromosome_j
  # filter below discards them.
  ord <- order(natural_sort_key(chromosome), position)
  snp_ids <- snp_ids[ord]; chromosome <- chromosome[ord]; position <- position[ord]
  n <- length(snp_ids)
  if (n < 2L) return(list(binned = empty_binned(), n_snps = n, n_pairs = 0L))

  z <- SNPRelate::snpgdsLDMat(
    gds, sample.id = sample_ids, snp.id = snp_ids, slide = min(slide, n - 1L),
    method = "r", with.id = TRUE, verbose = FALSE
  )
  slide_used <- z$slide
  pairs <- data.table::rbindlist(lapply(seq_len(slide_used), function(k) {
    j <- seq_len(n - k)
    data.table::data.table(
      chromosome_i = chromosome[j], chromosome_j = chromosome[j + k],
      position_i = position[j], position_j = position[j + k],
      r = z$LD[k, j]
    )
  }))
  pairs <- pairs[chromosome_i == chromosome_j & is.finite(r)]
  if (!nrow(pairs)) return(list(binned = empty_binned(), n_snps = n, n_pairs = 0L))
  pairs[, distance_bp := abs(position_j - position_i)]
  pairs <- pairs[distance_bp <= max_distance_bp]
  if (!nrow(pairs)) return(list(binned = empty_binned(), n_snps = n, n_pairs = 0L))
  pairs[, r2 := r * r]
  pairs[, distance_bin_start := as.integer((distance_bp %/% bin_bp) * bin_bp)]
  binned <- pairs[, .(n_pairs = .N, mean_r2 = mean(r2)), by = distance_bin_start]
  binned[, distance_bin_end := distance_bin_start + as.integer(bin_bp) - 1L]
  data.table::setcolorder(binned, c("distance_bin_start", "distance_bin_end", "n_pairs", "mean_r2"))
  data.table::setorder(binned, distance_bin_start)
  # Pairs beyond the SNP-index `slide` window are never computed even if they
  # fall within max_distance_bp -- a real limitation for sparse marker sets,
  # documented rather than silently truncated; n_pairs/n_snps let a caller
  # gauge coverage.
  list(binned = binned[], n_snps = n, n_pairs = nrow(pairs))
}

plot_ld_decay <- function(result, cfg, dirs) {
  fmts <- cfg$output$figure_formats; dpi <- cfg$output$dpi
  style <- figure_style_name(cfg)
  profile <- figure_style_profile(style)
  binned <- result$binned
  if (!nrow(binned)) return(invisible(NULL))

  accent <- unname(expand_figure_palette(profile, 1L, "colours"))
  p <- ggplot2::ggplot(binned, ggplot2::aes(distance_bin_start, mean_r2)) +
    ggplot2::geom_point(size = 1.6, alpha = .8, colour = accent) +
    ggplot2::geom_line(colour = accent, alpha = .6) +
    ggplot2::scale_x_continuous(labels = scales::label_comma()) +
    ggplot2::scale_y_continuous(limits = c(0, NA)) +
    ggplot2::labs(
      title = "Linkage disequilibrium decay",
      subtitle = sprintf(
        "Mean r^2 by physical distance between SNP pairs (%s pairs across %s SNPs)",
        scales::comma(result$n_pairs), scales::comma(result$n_snps)
      ),
      x = "Distance between SNP pair (bp)", y = expression(paste("Mean ", italic(r)^2))
    ) + theme_publication(figure_base_size(cfg))
  save_plot(p, "43_LD_decay", dirs, fmts, 8, 5, dpi)
}
