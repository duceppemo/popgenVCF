# Tajima's D (Tajima 1989), windowed. n is the haploid sample size (gene
# copies); a1/a2/b1/b2/c1/c2/e1/e2 are Tajima's own correction constants.
# Verified against a published worked example before shipping (n=10, S=16,
# pi=3.888889 -> D=-1.446172, matched to 6 decimal places -- see NEWS.md).
tajima_d_constants <- function(n) {
  i <- seq_len(n - 1L)
  a1 <- sum(1 / i)
  a2 <- sum(1 / i^2)
  b1 <- (n + 1) / (3 * (n - 1))
  b2 <- 2 * (n^2 + n + 3) / (9 * n * (n - 1))
  c1 <- b1 - 1 / a1
  c2 <- b2 - (n + 2) / (a1 * n) + a2 / a1^2
  list(a1 = a1, e1 = c1 / a1, e2 = c2 / (a1^2 + a2))
}

# pi: total nucleotide diversity across the window (sum of per-locus unbiased
# expected heterozygosity, not averaged -- the same units theta_W is
# estimated in). s: number of segregating (polymorphic) sites in the window.
# n: haploid sample size (gene copies), constant per population -- the
# classical Tajima (1989) formula assumes uniform sample size across sites,
# the same simplifying assumption vcftools' --TajimaD makes; per-site
# missingness is not separately modeled (a heavier generalization, e.g.
# ANGSD's numerical-integration approach, is out of scope here).
tajima_d_statistic <- function(pi, s, n) {
  if (!is.finite(s) || s <= 0L || !is.finite(n) || n < 2L) return(NA_real_)
  k <- tajima_d_constants(n)
  theta_w <- s / k$a1
  var_d <- k$e1 * s + k$e2 * s * (s - 1)
  if (!is.finite(var_d) || var_d <= 0) return(NA_real_)
  (pi - theta_w) / sqrt(var_d)
}

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
  windows <- data.table::rbindlist(out)
  windows[, .chr_sort_key := natural_sort_key(chromosome)]
  data.table::setorder(windows, .chr_sort_key, window_start)
  windows[, .chr_sort_key := NULL]
  windows[]
}

run_genome_scan_fst <- function(gds, snp_ids, ids, metadata, window_bp, step_bp, min_snps,
                                 gds_path = NULL, threads = 1L) {
  # Named snp_* to avoid colliding with the `windows` table's own
  # chromosome/window_start/window_end columns once inside the per-row `j`
  # expression below -- a bare `chromosome` there resolves to windows' own
  # column (the current window's chromosome), not this per-SNP vector.
  snp_chromosome <- ids$chromosome[match(snp_ids, ids$snp)]
  snp_position <- ids$position[match(snp_ids, ids$snp)]
  windows <- genome_scan_windows(snp_chromosome, snp_position, window_bp, step_bp)
  # `ids$sample` is the raw, pre-sample-QC GDS sample list; `metadata` is
  # already filtered to the QC-retained set (harmonize_samples()), so the
  # retained sample.id/population pair must come from metadata, not ids --
  # using ids$sample here would pass QC-excluded samples straight to
  # snpgdsFst() with an undefined (NA) population, which it rejects outright.
  sample_ids <- metadata$sample
  population <- factor(metadata$population)
  n_windows <- nrow(windows)

  window_fst_stat <- function(i, gds_conn) {
    chromosome <- windows$chromosome[[i]]
    window_start <- windows$window_start[[i]]
    window_end <- windows$window_end[[i]]
    w <- snp_ids[snp_chromosome == chromosome & snp_position >= window_start & snp_position <= window_end]
    if (length(w) < min_snps) return(list(n_snps = length(w), global_fst = NA_real_))
    z <- SNPRelate::snpgdsFst(
      gds_conn, sample.id = sample_ids, snp.id = w, population = population,
      method = "W&C84", autosome.only = FALSE, remove.monosnp = TRUE,
      maf = NaN, missing.rate = NaN, verbose = FALSE
    )
    list(n_snps = length(w), global_fst = as.numeric(z$Fst))
  }

  # A genome-wide scan has far more windows (thousands+) than a per-population
  # task, each individually cheap (one snpgdsFst() call over that window's
  # handful of SNPs) -- forking one process per window (as diversity/fst do
  # per population/pair, where each task is genuinely heavy) would pay fork
  # overhead thousands of times over. Instead windows are split into exactly
  # `workers` contiguous chunks (parallel::splitIndices(), the same
  # balanced-partitioning helper parallel::mclapply() uses internally), and
  # each forked worker opens one independent GDS connection (sharing the
  # parent's is unsafe, see diversity.R) and processes its whole chunk.
  workers <- if (is.null(gds_path)) 1L else fork_worker_count(n_windows, threads)
  results <- if (workers <= 1L) {
    lapply(seq_len(n_windows), window_fst_stat, gds_conn = gds)
  } else {
    chunks <- parallel::splitIndices(n_windows, workers)
    chunk_results <- parallel::mclapply(chunks, function(idx) {
      worker_gds <- SNPRelate::snpgdsOpen(
        gds_path, readonly = TRUE, allow.duplicate = TRUE, allow.fork = TRUE
      )
      on.exit(SNPRelate::snpgdsClose(worker_gds), add = TRUE)
      lapply(idx, window_fst_stat, gds_conn = worker_gds)
    }, mc.cores = workers, mc.preschedule = FALSE, mc.set.seed = FALSE)
    check_mclapply_results(
      chunk_results, paste0("chunk ", seq_along(chunks)), "genome-scan FST computation"
    )
    unlist(chunk_results, recursive = FALSE)
  }
  windows[, n_snps := vapply(results, `[[`, integer(1L), "n_snps")]
  windows[, global_fst := vapply(results, `[[`, numeric(1L), "global_fst")]
  windows[]
}

run_genome_scan_diversity <- function(locus_table, window_bp, step_bp, min_snps, population_n = NULL,
                                       threads = 1L) {
  windows <- genome_scan_windows(locus_table$chromosome, locus_table$position, window_bp, step_bp)
  populations <- sort(unique(locus_table$population))

  diversity_scan_windows <- function(pop) {
    pop_locus <- locus_table[population == pop]
    # Plain vectors, not a second data.table with its own chromosome column
    # -- the same naming-collision hazard as run_genome_scan_fst() above.
    locus_chromosome <- pop_locus$chromosome
    locus_position <- pop_locus$position
    # Haploid sample size (gene copies) for Tajima's D; NA when unknown, so
    # tajima_d_statistic() reports NA rather than guessing.
    n_haploid <- if (!is.null(population_n) && pop %in% names(population_n)) {
      2 * population_n[[pop]]
    } else NA_real_
    grid <- data.table::copy(windows)
    grid[, population := pop]
    grid[, c(
      "n_snps", "mean_observed_heterozygosity", "mean_expected_heterozygosity",
      "segregating_sites", "tajima_d"
    ) := {
      hit <- locus_chromosome == chromosome & locus_position >= window_start & locus_position <= window_end
      if (sum(hit) < min_snps) {
        list(sum(hit), NA_real_, NA_real_, NA_integer_, NA_real_)
      } else {
        s <- sum(pop_locus$polymorphic[hit], na.rm = TRUE)
        pi_total <- sum(pop_locus$unbiased_expected_heterozygosity[hit], na.rm = TRUE)
        list(
          sum(hit), mean(pop_locus$observed_heterozygosity[hit], na.rm = TRUE),
          mean(pop_locus$unbiased_expected_heterozygosity[hit], na.rm = TRUE),
          s, tajima_d_statistic(pi_total, s, n_haploid)
        )
      }
    }, by = seq_len(nrow(grid))]
    grid
  }

  # Pure R/data.table computation over an already-materialized locus subset
  # -- no GDS handle involved, unlike run_genome_scan_fst() above, so this
  # can fork directly with no per-worker connection dance. Each population's
  # full windowed scan is a genuinely heavy, independent task (same profile
  # as diversity's own per-population parallelization), so forking per
  # population (not per window) is the right granularity here.
  workers <- fork_worker_count(length(populations), threads)
  grids <- if (workers <= 1L) {
    lapply(populations, diversity_scan_windows)
  } else {
    results <- parallel::mclapply(
      populations, diversity_scan_windows,
      mc.cores = workers, mc.preschedule = FALSE, mc.set.seed = FALSE
    )
    check_mclapply_results(results, populations, "genome-scan diversity computation")
    results
  }
  out <- data.table::rbindlist(grids)
  data.table::setcolorder(out, c("chromosome", "window_start", "window_end", "population", "n_snps"))
  out[, .chr_sort_key := natural_sort_key(chromosome)]
  data.table::setorder(out, .chr_sort_key, window_start, population)
  out[, .chr_sort_key := NULL]
  out[]
}

plot_genome_scan <- function(fst_windows, diversity_windows, cfg, dirs) {
  fmts <- cfg$output$figure_formats; dpi <- cfg$output$dpi
  style <- figure_style_name(cfg)
  profile <- figure_style_profile(style)

  base_size <- figure_base_size(cfg)

  tested_fst <- fst_windows[is.finite(global_fst)]
  if (nrow(tested_fst)) {
    layout <- manhattan_layout(tested_fst$chromosome, tested_fst$window_start)
    bp_breaks <- manhattan_bp_breaks(tested_fst$chromosome, tested_fst$window_start, layout$offset)
    tested_fst <- data.table::copy(tested_fst)
    tested_fst[, x := layout$x]
    tested_fst[, chrom_group := factor(match(chromosome, layout$ticks$chromosome) %% 2L)]
    colours <- expand_figure_palette(profile, 2L, "colours")
    p1 <- ggplot2::ggplot(tested_fst, ggplot2::aes(x = x, y = global_fst, colour = chrom_group)) +
      ggplot2::geom_hline(yintercept = 0, colour = "#D9D9D9", linewidth = 0.35) +
      ggplot2::geom_point(size = 1.3, alpha = .8, show.legend = FALSE) +
      ggplot2::scale_colour_manual(values = colours) +
      ggplot2::scale_x_continuous(breaks = bp_breaks$x, labels = bp_breaks$label) +
      ggplot2::labs(
        title = "Sliding-window FST scan",
        subtitle = "Weir-Cockerham global FST per window; exploratory outlier flagging, not a significance test",
        x = "Chromosome position", y = expression(italic(F)[ST])
      ) + theme_publication(base_size)
    # geom_hline(yintercept = 0) above expands this panel's rendered
    # y-scale to include 0 whenever the real data doesn't naturally span
    # it -- see the matching comment in ordination.R's
    # plot_pca_loading_manhattan() for the full mechanism/consequence.
    p1 <- manhattan_chromosome_row(
      p1, layout$ticks, range(c(tested_fst$global_fst, 0), na.rm = TRUE), base_size,
      plot_width_in = 10
    )
    save_plot(p1, "25_genome_scan_FST_manhattan", dirs, fmts, 10, 4.5, dpi)
  }

  tested_div <- diversity_windows[is.finite(mean_expected_heterozygosity)]
  if (nrow(tested_div)) {
    layout <- manhattan_layout(tested_div$chromosome, tested_div$window_start)
    bp_breaks <- manhattan_bp_breaks(tested_div$chromosome, tested_div$window_start, layout$offset)
    tested_div <- data.table::copy(tested_div)
    tested_div[, x := layout$x]
    p2 <- ggplot2::ggplot(tested_div, ggplot2::aes(x = x, y = mean_expected_heterozygosity, colour = population)) +
      ggplot2::geom_point(size = 1.1, alpha = .75) +
      ggplot2::scale_colour_manual(values = population_palette(tested_div$population, style)) +
      ggplot2::scale_x_continuous(breaks = bp_breaks$x, labels = bp_breaks$label) +
      ggplot2::labs(
        title = "Sliding-window diversity scan",
        subtitle = "Mean expected heterozygosity per window per population",
        x = "Chromosome position", y = expression(italic(H)[E]), colour = "Population"
      ) + theme_publication(base_size)
    p2 <- manhattan_chromosome_row(
      p2, layout$ticks, range(tested_div$mean_expected_heterozygosity, na.rm = TRUE),
      base_size, plot_width_in = 10
    )
    save_plot(p2, "26_genome_scan_diversity_manhattan", dirs, fmts, 10, 5, dpi)
  }

  tested_tajima <- diversity_windows[is.finite(tajima_d)]
  if (nrow(tested_tajima)) {
    layout <- manhattan_layout(tested_tajima$chromosome, tested_tajima$window_start)
    bp_breaks <- manhattan_bp_breaks(tested_tajima$chromosome, tested_tajima$window_start, layout$offset)
    tested_tajima <- data.table::copy(tested_tajima)
    tested_tajima[, x := layout$x]
    p3 <- ggplot2::ggplot(tested_tajima, ggplot2::aes(x = x, y = tajima_d, colour = population)) +
      ggplot2::geom_hline(yintercept = 0, colour = "#D9D9D9", linewidth = 0.35) +
      ggplot2::geom_point(size = 1.1, alpha = .75) +
      ggplot2::scale_colour_manual(values = population_palette(tested_tajima$population, style)) +
      ggplot2::scale_x_continuous(breaks = bp_breaks$x, labels = bp_breaks$label) +
      ggplot2::labs(
        title = "Sliding-window Tajima's D scan",
        subtitle = "Per window per population; a neutrality-test statistic, not itself an outlier-significance test",
        x = "Chromosome position", y = "Tajima's D", colour = "Population"
      ) + theme_publication(base_size)
    # geom_hline(yintercept = 0) above expands this panel's rendered
    # y-scale to include 0 whenever the real data doesn't naturally span
    # it -- see the matching comment in ordination.R's
    # plot_pca_loading_manhattan() for the full mechanism/consequence.
    p3 <- manhattan_chromosome_row(
      p3, layout$ticks, range(c(tested_tajima$tajima_d, 0), na.rm = TRUE), base_size,
      plot_width_in = 10
    )
    save_plot(p3, "26b_genome_scan_tajima_d_manhattan", dirs, fmts, 10, 5, dpi)
  }
}
