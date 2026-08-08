# bcftools roh's region rows are unambiguously prefixed "RG\t", distinct from
# its "#"-comment header and informational log lines that share stdout/stderr
# once captured through system2(stdout=TRUE, stderr=TRUE).
roh_parse_regions <- function(lines) {
  hits <- lines[startsWith(lines, "RG\t")]
  if (!length(hits)) {
    return(data.table::data.table(
      sample = character(), chromosome = character(),
      start = integer(), end = integer(), length_bp = integer(),
      n_markers = integer(), quality = numeric()
    ))
  }
  fields <- strsplit(hits, "\t", fixed = TRUE)
  data.table::data.table(
    sample = vapply(fields, `[[`, character(1), 2L),
    chromosome = vapply(fields, `[[`, character(1), 3L),
    start = as.integer(vapply(fields, `[[`, character(1), 4L)),
    end = as.integer(vapply(fields, `[[`, character(1), 5L)),
    length_bp = as.integer(vapply(fields, `[[`, character(1), 6L)),
    n_markers = as.integer(vapply(fields, `[[`, character(1), 7L)),
    quality = as.numeric(vapply(fields, `[[`, character(1), 8L))
  )
}

roh_analyzed_footprint_bp <- function(bcftools, vcf_path) {
  positions <- vcf_command_status(bcftools, c("query", "-f", shQuote("%CHROM\t%POS\n"), shQuote(vcf_path)))
  if (!identical(positions$status, 0L)) {
    stop("Failed to enumerate ROH-analyzed sites: ", paste(positions$output, collapse = "\n"), call. = FALSE)
  }
  lines <- positions$output[nzchar(positions$output)]
  if (!length(lines)) return(list(footprint_bp = 0, n_sites = 0L))
  fields <- strsplit(lines, "\t", fixed = TRUE)
  chromosome <- vapply(fields, `[[`, character(1), 1L)
  position <- as.numeric(vapply(fields, `[[`, character(1), 2L))
  span <- vapply(split(position, chromosome), function(p) max(p) - min(p) + 1, numeric(1))
  list(footprint_bp = sum(span), n_sites = length(lines))
}

run_roh <- function(vcf_path, sample_ids, metadata, missing_rate, gt_error_phred, threads) {
  bcftools <- require_vcf_tool("bcftools")
  work_dir <- tempfile("roh-")
  dir.create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)

  samples_file <- file.path(work_dir, "samples.txt")
  writeLines(as.character(sample_ids), samples_file)
  subset_vcf <- file.path(work_dir, "roh_subset.vcf.gz")
  view_args <- c(
    "view", "--min-alleles", "2", "--max-alleles", "2", "--types", "snps",
    "-S", shQuote(samples_file),
    "--exclude", shQuote(sprintf("F_MISSING > %s", missing_rate)),
    "--output-type", "z", "--output", shQuote(subset_vcf), shQuote(vcf_path)
  )
  viewed <- vcf_command_status(bcftools, view_args)
  if (!identical(viewed$status, 0L) || !file.exists(subset_vcf)) {
    stop("Failed to derive the ROH sample/site subset VCF: ", paste(viewed$output, collapse = "\n"), call. = FALSE)
  }

  public_ids <- public_sample_ids(metadata, sample_ids)
  empty_summary <- function() data.table::data.table(
    sample = public_ids, n_runs = 0L, total_length_bp = 0, mean_length_bp = NA_real_,
    longest_run_bp = 0L, froh = 0
  )

  footprint <- roh_analyzed_footprint_bp(bcftools, subset_vcf)
  if (footprint$n_sites == 0L) {
    log_msg("ROH: no sites remained after biallelic-SNP/missingness filtering; skipping bcftools roh", level = "WARNING")
    return(list(
      runs = data.table::data.table(
        sample = character(), population = character(), chromosome = character(),
        start = integer(), end = integer(), length_bp = integer(),
        n_markers = integer(), quality = numeric()
      ),
      sample_summary = empty_summary(),
      analyzed_footprint_bp = 0
    ))
  }

  roh_args <- c(
    "roh", "-G", as.character(gt_error_phred), "--estimate-AF", "-",
    "--threads", as.character(threads), "-O", "r", shQuote(subset_vcf)
  )
  called <- vcf_command_status(bcftools, roh_args)
  if (!identical(called$status, 0L)) {
    stop("bcftools roh failed: ", paste(called$output, collapse = "\n"), call. = FALSE)
  }
  runs <- roh_parse_regions(called$output)
  runs[, sample := public_ids[match(sample, sample_ids)]]
  if ("population" %in% names(metadata)) {
    raw_lookup <- stats::setNames(sample_ids, public_ids)
    runs[, population := metadata$population[match(raw_lookup[sample], metadata$sample)]]
    data.table::setcolorder(runs, c("sample", "population", "chromosome", "start", "end", "length_bp", "n_markers", "quality"))
  }
  data.table::setorder(runs, sample, chromosome, start)

  agg <- if (nrow(runs)) {
    runs[, .(
      n_runs = .N, total_length_bp = sum(length_bp),
      mean_length_bp = mean(length_bp), longest_run_bp = max(length_bp)
    ), by = sample]
  } else {
    data.table::data.table(sample = character(), n_runs = integer(), total_length_bp = numeric(),
                           mean_length_bp = numeric(), longest_run_bp = integer())
  }
  sample_summary <- merge(
    data.table::data.table(sample = public_ids), agg, by = "sample", all.x = TRUE
  )
  sample_summary[is.na(n_runs), n_runs := 0L]
  sample_summary[is.na(total_length_bp), total_length_bp := 0]
  sample_summary[is.na(longest_run_bp), longest_run_bp := 0L]
  sample_summary[, froh := if (footprint$footprint_bp > 0) total_length_bp / footprint$footprint_bp else 0]
  if ("population" %in% names(metadata)) {
    raw_lookup <- stats::setNames(sample_ids, public_ids)
    sample_summary[, population := metadata$population[match(raw_lookup[sample], metadata$sample)]]
    data.table::setcolorder(sample_summary, c("sample", "population", "n_runs", "total_length_bp",
                                              "mean_length_bp", "longest_run_bp", "froh"))
  }
  data.table::setorder(sample_summary, -froh)

  list(runs = runs, sample_summary = sample_summary, analyzed_footprint_bp = footprint$footprint_bp)
}

plot_roh <- function(result, cfg, dirs) {
  fmts <- cfg$output$figure_formats; dpi <- cfg$output$dpi
  style <- figure_style_name(cfg)
  profile <- figure_style_profile(style)

  if (nrow(result$runs)) {
    accent <- unname(expand_figure_palette(profile, 1L, "fills"))
    p1 <- ggplot2::ggplot(result$runs, ggplot2::aes(length_bp)) +
      ggplot2::geom_histogram(bins = 30, fill = accent, colour = "white", linewidth = 0.2) +
      ggplot2::scale_x_log10(labels = scales::label_comma()) +
      ggplot2::scale_y_continuous(
        labels = scales::label_comma(), expand = ggplot2::expansion(mult = c(0, 0.06))
      ) +
      ggplot2::labs(
        title = "Runs of homozygosity: length distribution",
        subtitle = "Every detected run across all samples",
        x = "Run length (bp, log scale)", y = "Number of runs"
      ) + theme_publication(figure_base_size(cfg))
    save_plot(p1, "23_ROH_length_distribution", dirs, fmts, 8, 5, dpi)
  }

  summary <- data.table::copy(result$sample_summary)
  has_population <- "population" %in% names(summary) && any(!is.na(summary$population))
  data.table::setorder(summary, froh)
  summary[, sample := factor(sample, levels = sample)]
  mapping <- if (has_population) {
    ggplot2::aes(x = sample, y = froh, colour = population, shape = population)
  } else {
    ggplot2::aes(x = sample, y = froh)
  }
  p2 <- ggplot2::ggplot(summary, mapping) +
    ggplot2::geom_point(size = 2.2, alpha = .85) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = expression(paste("Runs of homozygosity: ", italic(F)[ROH], " by sample")),
      subtitle = "Total run length divided by the analyzed genomic footprint (not genome-wide)",
      x = NULL, y = expression(italic(F)[ROH]), colour = "Population", shape = "Population"
    ) + theme_publication(figure_base_size(cfg))
  if (has_population) {
    p2 <- p2 +
      ggplot2::scale_colour_manual(values = population_palette(summary$population, style)) +
      ggplot2::scale_shape_manual(values = population_shapes(summary$population, style))
  }
  n <- nrow(summary)
  save_plot(p2, "24_ROH_FROH_by_sample", dirs, fmts, 8, max(4, n * 0.18), dpi)
}
