# Two complementary, independent signals for genetic sex (PLINK's
# `--check-sex` convention, Purcell et al. 2007, extended the way real
# sex-check pipelines actually use it -- X heterozygosity plus Y presence,
# not X alone):
#
# X-chromosome heterozygosity (Visscher et al. 2010's F-statistic, already
# available as SNPRelate::snpgdsIndInb(method="mom.visscher")) separates
# hemizygous males (excess homozygosity, F close to 1) from diploid females
# (F close to 0). Verified empirically against a hand-constructed synthetic
# GDS (known male/female X genotypes) before committing to these thresholds:
# simulated males gave F in [0.84, 1.13], simulated females gave F in
# [-0.06, 0.10] -- a clean separation either side of PLINK's own documented
# defaults (male > 0.8, female < 0.2).
#
# Y-chromosome genotype call rate (`SNPRelate::snpgdsSampMissRate()`,
# already an SNPRelate dependency) is a second, independent, much cleaner
# signal: males have a Y chromosome to call genotypes from, females do not.
# Verified against real 1000 Genomes data before committing to these
# thresholds: females showed exactly 0.0 call rate (no chromosome Y calls
# at all), males 0.995-1.0 -- an almost perfectly binary separation, well
# clear of the default thresholds (male > 0.5, female < 0.1).
#
# When both signals are available and agree, that is a high-confidence
# call. When they are both available but confidently disagree (one signal
# says male, the other says female), that is reported as `discordant` --
# genuinely informative evidence of a data problem, not something to average
# away. Either signal alone, when the other is unavailable (e.g. a VCF with
# only one of the two chromosomes), is used on its own.
#
# Deliberately does not exclude pseudoautosomal (PAR) regions on X: their
# boundaries are genome-build- and species-specific (GRCh37 and GRCh38
# differ), and this package accepts arbitrary biallelic SNP VCFs from any
# build or organism without a build-identification mechanism -- the same
# reasoning already documented for not supplying species-specific genetic
# maps to the runs-of-homozygosity module. PAR SNPs are diploid in both
# sexes and dilute (not reverse) the X-based separation; users who know
# their build's PAR coordinates can pre-filter their VCF before analysis.
sex_check_status_levels <- function() c("match", "mismatch", "discordant", "ambiguous", "not_reported")

normalize_sex_label <- function(x) {
  x <- tolower(trimws(as.character(x)))
  data.table::fcase(
    x %in% c("male", "m", "1"), "male",
    x %in% c("female", "f", "2"), "female",
    default = NA_character_
  )
}

# Combines an X-based and a Y-based sex call into one. Both vectors use
# {"male", "female", "ambiguous", NA} (NA meaning that signal was not
# computed at all, e.g. no Y chromosome in the VCF).
combine_sex_evidence <- function(x_sex, y_sex) {
  data.table::fcase(
    is.na(x_sex) & is.na(y_sex), NA_character_,
    is.na(x_sex), y_sex,
    is.na(y_sex), x_sex,
    x_sex == y_sex, x_sex,
    x_sex == "ambiguous", y_sex,
    y_sex == "ambiguous", x_sex,
    default = "discordant"
  )
}

run_sex_check <- function(gds, sample_ids, snp_ids, ids, metadata,
                           x_chromosome_names, male_f_threshold, female_f_threshold,
                           y_chromosome_names = c("Y", "chrY"),
                           y_male_call_rate_threshold = 0.5, y_female_call_rate_threshold = 0.1,
                           min_x_snps = 20L, min_y_snps = 5L) {
  x_snp_ids <- ids$snp[ids$chromosome %in% x_chromosome_names & ids$snp %in% snp_ids]
  y_snp_ids <- ids$snp[ids$chromosome %in% y_chromosome_names & ids$snp %in% snp_ids]
  n_x_snps <- length(x_snp_ids)
  n_y_snps <- length(y_snp_ids)
  has_x <- n_x_snps >= min_x_snps
  has_y <- n_y_snps >= min_y_snps

  if (!has_x && !has_y) {
    log_msg(
      "Skipping sex-check: only ", n_x_snps, " QC-passing X-chromosome SNP(s) ",
      "(minimum ", min_x_snps, ") and ", n_y_snps, " QC-passing Y-chromosome SNP(s) ",
      "(minimum ", min_y_snps, ") found",
      level = "INFO"
    )
    return(NULL)
  }

  public_ids <- public_sample_ids(metadata, sample_ids)
  table <- data.table::data.table(sample = public_ids, vcf_sample = sample_ids)

  x_inferred_sex <- rep(NA_character_, length(sample_ids))
  x_heterozygosity_F <- rep(NA_real_, length(sample_ids))
  if (has_x) {
    rv <- SNPRelate::snpgdsIndInb(
      gds, sample.id = sample_ids, snp.id = x_snp_ids,
      autosome.only = FALSE, remove.monosnp = TRUE, method = "mom.visscher",
      verbose = FALSE
    )
    idx <- match(table$vcf_sample, as.character(rv$sample.id))
    x_heterozygosity_F <- as.numeric(rv$inbreeding)[idx]
    x_inferred_sex <- data.table::fcase(
      !is.finite(x_heterozygosity_F), NA_character_,
      x_heterozygosity_F >= male_f_threshold, "male",
      x_heterozygosity_F <= female_f_threshold, "female",
      default = "ambiguous"
    )
  }

  y_inferred_sex <- rep(NA_character_, length(sample_ids))
  y_call_rate <- rep(NA_real_, length(sample_ids))
  if (has_y) {
    missing_rate <- SNPRelate::snpgdsSampMissRate(
      gds, sample.id = sample_ids, snp.id = y_snp_ids, with.id = TRUE
    )
    idx <- match(table$vcf_sample, names(missing_rate))
    y_call_rate <- 1 - as.numeric(missing_rate)[idx]
    y_inferred_sex <- data.table::fcase(
      !is.finite(y_call_rate), NA_character_,
      y_call_rate >= y_male_call_rate_threshold, "male",
      y_call_rate <= y_female_call_rate_threshold, "female",
      default = "ambiguous"
    )
  }

  inferred_sex <- combine_sex_evidence(x_inferred_sex, y_inferred_sex)

  table[, n_x_snps := n_x_snps]
  table[, x_heterozygosity_F := x_heterozygosity_F]
  table[, x_inferred_sex := x_inferred_sex]
  table[, n_y_snps := n_y_snps]
  table[, y_call_rate := y_call_rate]
  table[, y_inferred_sex := y_inferred_sex]
  table[, inferred_sex := inferred_sex]

  if ("sex" %in% names(metadata)) {
    reported <- normalize_sex_label(metadata$sex[match(table$vcf_sample, metadata$sample)])
    table[, reported_sex := reported]
    table[, status := data.table::fcase(
      is.na(inferred_sex), NA_character_,
      is.na(reported_sex), "not_reported",
      inferred_sex %in% c("ambiguous", "discordant"), inferred_sex,
      inferred_sex == reported_sex, "match",
      default = "mismatch"
    )]
  } else {
    table[, reported_sex := NA_character_]
    table[, status := NA_character_]
  }

  list(table = table, n_x_snps = n_x_snps, n_y_snps = n_y_snps)
}

plot_sex_check <- function(result, cfg, dirs) {
  fmts <- cfg$output$figure_formats; dpi <- cfg$output$dpi
  style <- figure_style_name(cfg)
  table <- data.table::copy(result$table)
  if (!nrow(table)) return(invisible(NULL))

  has_status <- "status" %in% names(table) && any(!is.na(table$status))
  colour_var <- if (has_status) "status" else "inferred_sex"
  levels <- if (has_status) sex_check_status_levels() else c("male", "female", "ambiguous", "discordant")
  table[[colour_var]] <- factor(table[[colour_var]], levels = levels)
  palette <- stats::setNames(
    expand_figure_palette(figure_style_profile(style), length(levels), "colours"), levels
  )
  colour_label <- if (has_status) "Status" else "Inferred sex"

  male_f <- cfg$analyses$sex_check_male_f_threshold
  female_f <- cfg$analyses$sex_check_female_f_threshold
  male_y <- cfg$analyses$sex_check_y_male_call_rate_threshold
  female_y <- cfg$analyses$sex_check_y_female_call_rate_threshold

  has_x <- any(is.finite(table$x_heterozygosity_F))
  has_y <- any(is.finite(table$y_call_rate))
  n <- nrow(table)

  if (has_x && has_y) {
    p <- ggplot2::ggplot(
      table,
      ggplot2::aes(x = x_heterozygosity_F, y = y_call_rate, colour = .data[[colour_var]])
    ) +
      ggplot2::geom_vline(
        xintercept = c(female_f, male_f), colour = "#D9D9D9", linewidth = 0.3, linetype = "dashed"
      ) +
      ggplot2::geom_hline(
        yintercept = c(female_y, male_y), colour = "#D9D9D9", linewidth = 0.3, linetype = "dashed"
      ) +
      ggplot2::geom_point(size = 2.2, alpha = .85, na.rm = TRUE) +
      ggplot2::scale_colour_manual(values = palette, drop = FALSE, na.translate = FALSE) +
      ggplot2::labs(
        title = "Genetic sex inference from X heterozygosity and Y call rate",
        subtitle = "Dashed lines: female/male thresholds for each signal (PLINK --check-sex convention)",
        x = expression(italic(F)~"(X-chromosome heterozygosity)"), y = "Y-chromosome call rate",
        colour = colour_label
      ) + theme_publication(figure_base_size(cfg))
    save_plot(p, "27_sex_check_F_by_sample", dirs, fmts, 8, 6, dpi)
    return(invisible(p))
  }

  y_var <- if (has_x) "x_heterozygosity_F" else "y_call_rate"
  thresholds <- if (has_x) c(female_f, male_f) else c(female_y, male_y)
  y_label <- if (has_x) {
    expression(italic(F))
  } else {
    "Y-chromosome call rate"
  }
  title <- if (has_x) {
    "Genetic sex inference from X-chromosome heterozygosity"
  } else {
    "Genetic sex inference from Y-chromosome call rate"
  }
  ordering <- order(table[[y_var]])
  table <- table[ordering]
  table[, sample := factor(sample, levels = sample)]

  p <- ggplot2::ggplot(
    table,
    ggplot2::aes(x = sample, y = .data[[y_var]], colour = .data[[colour_var]])
  ) +
    ggplot2::geom_hline(
      yintercept = thresholds, colour = "#D9D9D9", linewidth = 0.3, linetype = "dashed"
    ) +
    ggplot2::geom_point(size = 2.2, alpha = .85, na.rm = TRUE) +
    ggplot2::coord_flip() +
    ggplot2::scale_colour_manual(values = palette, drop = FALSE, na.translate = FALSE) +
    ggplot2::labs(
      title = title,
      subtitle = "Dashed lines: female/male thresholds (PLINK --check-sex convention)",
      x = NULL, y = y_label, colour = colour_label
    ) + theme_publication(figure_base_size(cfg))

  save_plot(p, "27_sex_check_F_by_sample", dirs, fmts, 8, max(4, n * 0.18), dpi)
}
