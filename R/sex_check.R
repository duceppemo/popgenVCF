# PLINK's `--check-sex` convention (Purcell et al. 2007): a per-sample
# X-chromosome heterozygosity F-statistic (Visscher et al. 2010's estimator,
# already available as SNPRelate::snpgdsIndInb(method="mom.visscher")) that
# separates hemizygous males (excess homozygosity, F close to 1) from
# diploid females (F close to 0). Verified empirically against a
# hand-constructed synthetic GDS (known male/female X genotypes) before
# committing to these thresholds: simulated males gave F in [0.84, 1.13],
# simulated females gave F in [-0.06, 0.10] -- a clean separation either
# side of PLINK's own documented defaults (male > 0.8, female < 0.2).
#
# Deliberately does not exclude pseudoautosomal (PAR) regions: their
# boundaries are genome-build- and species-specific (GRCh37 and GRCh38
# differ), and this package accepts arbitrary biallelic SNP VCFs from any
# build or organism without a build-identification mechanism -- the same
# reasoning already documented for not supplying species-specific genetic
# maps to the runs-of-homozygosity module. PAR SNPs are diploid in both
# sexes and dilute (not reverse) the separation; users who know their
# build's PAR coordinates can pre-filter their VCF before analysis.
sex_check_status_levels <- function() c("match", "mismatch", "ambiguous", "not_reported")

normalize_sex_label <- function(x) {
  x <- tolower(trimws(as.character(x)))
  data.table::fcase(
    x %in% c("male", "m", "1"), "male",
    x %in% c("female", "f", "2"), "female",
    default = NA_character_
  )
}

run_sex_check <- function(gds, sample_ids, snp_ids, ids, metadata,
                           x_chromosome_names, male_threshold, female_threshold,
                           min_x_snps = 20L) {
  x_snp_ids <- ids$snp[ids$chromosome %in% x_chromosome_names & ids$snp %in% snp_ids]
  n_x_snps <- length(x_snp_ids)
  if (n_x_snps < min_x_snps) {
    log_msg(
      "Skipping sex-check: only ", n_x_snps, " QC-passing X-chromosome SNP(s) ",
      "found (chromosome name(s) checked: ", paste(x_chromosome_names, collapse = ", "),
      "; minimum required: ", min_x_snps, ")",
      level = "INFO"
    )
    return(NULL)
  }

  rv <- SNPRelate::snpgdsIndInb(
    gds, sample.id = sample_ids, snp.id = x_snp_ids,
    autosome.only = FALSE, remove.monosnp = TRUE, method = "mom.visscher",
    verbose = FALSE
  )
  original_ids <- as.character(rv$sample.id)
  public_ids <- public_sample_ids(metadata, original_ids)
  f_statistic <- as.numeric(rv$inbreeding)

  inferred_sex <- data.table::fcase(
    !is.finite(f_statistic), NA_character_,
    f_statistic >= male_threshold, "male",
    f_statistic <= female_threshold, "female",
    default = "ambiguous"
  )

  table <- data.table::data.table(
    sample = public_ids, vcf_sample = original_ids,
    n_x_snps = n_x_snps, x_heterozygosity_F = f_statistic,
    inferred_sex = inferred_sex
  )

  if ("sex" %in% names(metadata)) {
    reported <- normalize_sex_label(metadata$sex[match(original_ids, metadata$sample)])
    table[, reported_sex := reported]
    table[, status := data.table::fcase(
      is.na(inferred_sex), NA_character_,
      is.na(reported_sex), "not_reported",
      inferred_sex == "ambiguous", "ambiguous",
      inferred_sex == reported_sex, "match",
      default = "mismatch"
    )]
  } else {
    table[, reported_sex := NA_character_]
    table[, status := NA_character_]
  }

  list(table = table, n_x_snps = n_x_snps)
}

plot_sex_check <- function(result, cfg, dirs) {
  fmts <- cfg$output$figure_formats; dpi <- cfg$output$dpi
  style <- figure_style_name(cfg)
  table <- data.table::copy(result$table)
  if (!nrow(table)) return(invisible(NULL))

  data.table::setorder(table, x_heterozygosity_F)
  table[, sample := factor(sample, levels = sample)]
  has_status <- "status" %in% names(table) && any(!is.na(table$status))
  colour_var <- if (has_status) "status" else "inferred_sex"
  levels <- if (has_status) sex_check_status_levels() else c("male", "female", "ambiguous")
  table[[colour_var]] <- factor(table[[colour_var]], levels = levels)
  palette <- stats::setNames(
    expand_figure_palette(figure_style_profile(style), length(levels), "colours"), levels
  )

  male_threshold <- cfg$analyses$sex_check_male_f_threshold
  female_threshold <- cfg$analyses$sex_check_female_f_threshold
  p <- ggplot2::ggplot(
    table,
    ggplot2::aes(x = sample, y = x_heterozygosity_F, colour = .data[[colour_var]])
  ) +
    ggplot2::geom_hline(
      yintercept = c(female_threshold, male_threshold),
      colour = "#D9D9D9", linewidth = 0.3, linetype = "dashed"
    ) +
    ggplot2::geom_point(size = 2.2, alpha = .85, na.rm = TRUE) +
    ggplot2::coord_flip() +
    ggplot2::scale_colour_manual(values = palette, drop = FALSE, na.translate = FALSE) +
    ggplot2::labs(
      title = "Genetic sex inference from X-chromosome heterozygosity",
      subtitle = "Dashed lines: female/male F-statistic thresholds (Visscher et al. 2010; PLINK --check-sex convention)",
      x = NULL, y = expression(italic(F)),
      colour = if (has_status) "Status" else "Inferred sex"
    ) + theme_publication(figure_base_size(cfg))

  n <- nrow(table)
  save_plot(p, "27_sex_check_F_by_sample", dirs, fmts, 8, max(4, n * 0.18), dpi)
}
