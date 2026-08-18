# Sex-biased dispersal test (Goudet, Perrin, and Waser 2002, "Tests for
# sex-biased dispersal using bi-parentally inherited genetic markers"): the
# standard population-genetic test for whether one sex disperses more than
# the other, from each individual's assignment index (AIc, Favre et al.
# 1997) -- how strongly an individual's genotype matches its own recorded
# population's allele frequencies, corrected for sample size. The
# more-dispersing sex (recent immigrants, or their offspring, genotyped
# before local allele frequencies "catch up") shows a systematically lower
# mean AIc than the more philopatric sex. `hierfstat::sexbias.test()`'s
# default "mAIc" test is a two-sample t-test (or, with `permutations`, a
# within-population permutation test) comparing mean AIc between the two
# recorded sexes; "FST"/"FIS" instead compare Weir & Cockerham's
# within-population differentiation computed separately per sex (requires
# `permutations`, since hierfstat provides no closed-form test for that
# comparison -- enforced in R/config.R).
#
# hierfstat is already an optional (Suggests-only) dependency of this
# package (see hierfstat_encode_genotype(), shared here with the allelic
# richness and population-specific-FST features), so this skips
# transparently, matching those, when hierfstat is not installed.
#
# This is a genuinely different question from sex_check's per-individual
# chromosomal-sex QC: sex_check asks whether one sample's own genotype
# matches its recorded sex; this asks whether the two recorded sexes
# differ, as groups, in how strongly they match their recorded
# population -- so it deliberately uses the metadata "sex" column, not
# sex_check's genetically inferred sex.
run_sexbias <- function(genotype, sample_ids, metadata, test = "mAIc",
                         permutations = 0L, seed = 42L) {
  if (!requireNamespace("hierfstat", quietly = TRUE)) return(NULL)
  if (!("sex" %in% names(metadata))) return(NULL)

  matched <- match(sample_ids, metadata$sample)
  population <- trimws(as.character(metadata$population[matched]))
  sex <- normalize_sex_label(metadata$sex[matched])
  usable <- !is.na(sex) & !is.na(population) & nzchar(population)
  sex_counts <- table(sex[usable])
  if (length(sex_counts) != 2L || any(sex_counts < 2L)) return(NULL)

  geno <- genotype[usable, , drop = FALSE]
  pop <- population[usable]
  sx <- sex[usable]
  public_ids <- public_sample_ids(metadata, sample_ids)[usable]

  encoded <- hierfstat_encode_genotype(geno)
  dat <- data.frame(pop = pop, encoded, check.names = FALSE)
  aic <- hierfstat::AIc(dat)

  nperm <- if (as.integer(permutations) > 0L) as.integer(permutations) else NULL
  set.seed(as.integer(seed))
  test_result <- hierfstat::sexbias.test(dat, sx, nperm = nperm, test = test)

  table <- data.table::data.table(
    sample = public_ids, population = pop, sex = sx, aic = as.numeric(aic)
  )
  data.table::setorder(table, sex, -aic)

  list(
    table = table, test = test,
    statistic = unname(test_result$statistic), p_value = unname(test_result$p.value),
    permutations = if (is.null(nperm)) 0L else nperm,
    n_female = unname(sex_counts["female"]), n_male = unname(sex_counts["male"])
  )
}

plot_sexbias <- function(result, cfg, dirs) {
  if (is.null(result) || !nrow(result$table)) return(invisible(NULL))
  fmts <- cfg$output$figure_formats; dpi <- cfg$output$dpi
  style <- figure_style_name(cfg)
  table <- data.table::copy(result$table)
  levels <- c("female", "male")
  table[, sex := factor(sex, levels = levels)]
  palette <- stats::setNames(
    expand_figure_palette(figure_style_profile(style), length(levels), "colours"), levels
  )

  p <- ggplot2::ggplot(table, ggplot2::aes(sex, aic, fill = sex)) +
    ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.6, width = 0.5, na.rm = TRUE) +
    ggplot2::geom_jitter(width = 0.12, size = 1.8, alpha = 0.75, na.rm = TRUE) +
    ggplot2::scale_fill_manual(values = palette, drop = FALSE, guide = "none") +
    ggplot2::labs(
      title = "Sex-biased dispersal test",
      subtitle = sprintf("%s test: statistic = %.3f, p = %.4f", result$test, result$statistic, result$p_value),
      caption = sprintf(
        "Assignment index (AIc, Favre et al. 1997) by recorded sex; n = %d female, %d male%s",
        result$n_female, result$n_male,
        if (result$permutations > 0L) sprintf(", %d permutations", result$permutations) else ""
      ),
      x = NULL, y = "Assignment index (AIc)"
    ) + theme_publication(figure_base_size(cfg))
  save_plot(p, "60_sexbias_AIc_by_sex", dirs, fmts, 7, 5.5, dpi)
}
