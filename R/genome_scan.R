genome_scan_windows <- function(chromosome, position, window_bp, step_bp) {
  chromosome <- as.character(chromosome)
  position <- as.numeric(position)
  by_chr <- split(position, chromosome)
  out <- lapply(names(by_chr), function(chr) {
    p <- by_chr[[chr]]
    starts <- seq(min(p), max(p), by = step_bp)
    data.table::data.table(
      chromosome = chr, window_start = as.integer(starts),
      window_end = as.integer(starts + window_bp - 1)
    )
  })
  data.table::setorder(data.table::rbindlist(out), chromosome, window_start)
}

run_genome_scan_fst <- function(gds, snp_ids, ids, metadata, window_bp, step_bp, min_snps) {
  # Named snp_* to avoid colliding with the `windows` table's own
  # chromosome/window_start/window_end columns once inside the per-row `j`
  # expression below -- a bare `chromosome` there resolves to windows' own
  # column (the current window's chromosome), not this per-SNP vector.
  snp_chromosome <- ids$chromosome[match(snp_ids, ids$snp)]
  snp_position <- ids$position[match(snp_ids, ids$snp)]
  windows <- genome_scan_windows(snp_chromosome, snp_position, window_bp, step_bp)
  population <- factor(metadata$population[match(ids$sample, metadata$sample)])
  windows[, c("n_snps", "global_fst") := {
    w <- snp_ids[snp_chromosome == chromosome & snp_position >= window_start & snp_position <= window_end]
    if (length(w) < min_snps) {
      list(length(w), NA_real_)
    } else {
      z <- SNPRelate::snpgdsFst(
        gds, sample.id = ids$sample, snp.id = w, population = population,
        method = "W&C84", autosome.only = FALSE, remove.monosnp = TRUE,
        maf = NaN, missing.rate = NaN, verbose = FALSE
      )
      list(length(w), as.numeric(z$Fst))
    }
  }, by = seq_len(nrow(windows))]
  windows[]
}

run_genome_scan_diversity <- function(locus_table, window_bp, step_bp, min_snps) {
  windows <- genome_scan_windows(locus_table$chromosome, locus_table$position, window_bp, step_bp)
  populations <- sort(unique(locus_table$population))
  out <- data.table::rbindlist(lapply(populations, function(pop) {
    pop_locus <- locus_table[population == pop]
    # Plain vectors, not a second data.table with its own chromosome column
    # -- the same naming-collision hazard as run_genome_scan_fst() above.
    locus_chromosome <- pop_locus$chromosome
    locus_position <- pop_locus$position
    grid <- data.table::copy(windows)
    grid[, population := pop]
    grid[, c("n_snps", "mean_observed_heterozygosity", "mean_expected_heterozygosity") := {
      hit <- locus_chromosome == chromosome & locus_position >= window_start & locus_position <= window_end
      if (sum(hit) < min_snps) {
        list(sum(hit), NA_real_, NA_real_)
      } else {
        list(
          sum(hit), mean(pop_locus$observed_heterozygosity[hit], na.rm = TRUE),
          mean(pop_locus$unbiased_expected_heterozygosity[hit], na.rm = TRUE)
        )
      }
    }, by = seq_len(nrow(grid))]
    grid
  }))
  data.table::setcolorder(out, c("chromosome", "window_start", "window_end", "population", "n_snps"))
  data.table::setorder(out, chromosome, window_start, population)
  out[]
}

plot_genome_scan <- function(fst_windows, diversity_windows, cfg, dirs) {
  fmts <- cfg$output$figure_formats; dpi <- cfg$output$dpi
  style <- figure_style_name(cfg)
  profile <- figure_style_profile(style)

  tested_fst <- fst_windows[is.finite(global_fst)]
  if (nrow(tested_fst)) {
    layout <- manhattan_layout(tested_fst$chromosome, tested_fst$window_start)
    tested_fst <- data.table::copy(tested_fst)
    tested_fst[, x := layout$x]
    tested_fst[, chrom_group := factor(match(chromosome, layout$ticks$chromosome) %% 2L)]
    colours <- expand_figure_palette(profile, 2L, "colours")
    p1 <- ggplot2::ggplot(tested_fst, ggplot2::aes(x = x, y = global_fst, colour = chrom_group)) +
      ggplot2::geom_hline(yintercept = 0, colour = "#D9D9D9", linewidth = 0.35) +
      ggplot2::geom_point(size = 1.3, alpha = .8, show.legend = FALSE) +
      ggplot2::scale_colour_manual(values = colours) +
      ggplot2::scale_x_continuous(breaks = layout$ticks$center, labels = layout$ticks$chromosome) +
      ggplot2::labs(
        title = "Sliding-window FST scan",
        subtitle = "Weir-Cockerham global FST per window; exploratory outlier flagging, not a significance test",
        x = "Chromosome (window start)", y = expression(italic(F)[ST])
      ) + theme_publication(figure_base_size(cfg))
    save_plot(p1, "25_genome_scan_FST_manhattan", dirs, fmts, 10, 4.5, dpi)
  }

  tested_div <- diversity_windows[is.finite(mean_expected_heterozygosity)]
  if (nrow(tested_div)) {
    layout <- manhattan_layout(tested_div$chromosome, tested_div$window_start)
    tested_div <- data.table::copy(tested_div)
    tested_div[, x := layout$x]
    p2 <- ggplot2::ggplot(tested_div, ggplot2::aes(x = x, y = mean_expected_heterozygosity, colour = population)) +
      ggplot2::geom_point(size = 1.1, alpha = .75) +
      ggplot2::scale_colour_manual(values = population_palette(tested_div$population, style)) +
      ggplot2::scale_x_continuous(breaks = layout$ticks$center, labels = layout$ticks$chromosome) +
      ggplot2::labs(
        title = "Sliding-window diversity scan",
        subtitle = "Mean expected heterozygosity per window per population",
        x = "Chromosome (window start)", y = expression(italic(H)[E]), colour = "Population"
      ) + theme_publication(figure_base_size(cfg))
    save_plot(p2, "26_genome_scan_diversity_manhattan", dirs, fmts, 10, 5, dpi)
  }
}
