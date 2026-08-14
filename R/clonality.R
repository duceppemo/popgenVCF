# Multilocus genotype (MLG) and clonal diversity analysis. `poppr` is
# already a required dependency, but its actual purpose -- summarizing
# genotypic diversity and detecting samples that share an identical
# multilocus genotype -- was completely unused elsewhere in this package
# (the only existing call, poppr.amova() in R/amova.R, never touches MLGs at
# all). Distinct from kinship (R/kinship.R): kinship scores continuous
# pairwise relatedness from a KING estimator and flags samples above a
# threshold; this module instead asks whether two samples' genotypes are
# exactly identical across the analyzed marker set, the standard signal for
# clonal replicates, accidental resampling, or duplicate submissions in any
# population-genetic dataset -- diploid outbreeding or partially clonal.
#
# adegenet's genlight/poppr's snpclone representation (this package's usual
# object for SNP data -- see genlight_from_gds(), R/dapc.R) is explicitly
# rejected by both poppr() and genotype_curve() ("The poppr function will
# not work with genlight or snpclone objects" / "must be a genind or loci
# object", both confirmed empirically against the installed poppr package),
# so this module builds a `genind` object directly instead. Genotypes are
# coded with synthetic per-locus allele labels ("1"/"2") rather than actual
# ref/alt bases: genotype matching only needs internally-consistent labels
# per locus, not real nucleotide identity (the same simplification ml_tree's
# IUPAC encoding could not make, since that module needs real bases for a
# nucleotide substitution model).

clonality_encode_genind <- function(genotype, sample_ids, population) {
  code <- matrix(NA_character_, nrow(genotype), ncol(genotype), dimnames = dimnames(genotype))
  code[which(!is.na(genotype) & genotype == 0)] <- "1/1"
  code[which(!is.na(genotype) & genotype == 1)] <- "1/2"
  code[which(!is.na(genotype) & genotype == 2)] <- "2/2"
  gid <- adegenet::df2genind(
    code, sep = "/", ncode = 1L, ploidy = 2L, type = "codom",
    ind.names = sample_ids, loc.names = colnames(genotype)
  )
  adegenet::pop(gid) <- factor(population)
  gid
}

# poppr()'s own column names (Pop/N/MLG/eMLG/SE/H/G/lambda/E.5/Hexp/Ia/rbarD,
# plus p.Ia/p.rD when a permutation test was requested) are renamed to
# self-documenting snake_case for the written table; File is dropped (always
# the input object's deparsed name, not a real result).
clonality_rename_summary <- function(summary_df) {
  dt <- data.table::as.data.table(summary_df)
  rename <- c(
    Pop = "population", N = "n", MLG = "mlg", eMLG = "emlg", SE = "emlg_se",
    H = "shannon_h", G = "stoddart_taylor_g", lambda = "simpson_lambda",
    E.5 = "evenness_e5", Hexp = "expected_heterozygosity",
    Ia = "ia", p.Ia = "ia_p_value", rbarD = "rbard", p.rD = "rbard_p_value"
  )
  keep <- intersect(names(rename), names(dt))
  data.table::setnames(dt, keep, unname(rename[keep]))
  dt[, File := NULL]
  dt[]
}

# Groups of samples sharing an identical multilocus genotype, from
# poppr::mlg.id(); only groups with more than one member are informative
# (a size-1 group is just an ordinary unique genotype). `cross_population`
# flags a group whose members carry different recorded population labels --
# a stronger, discrete corroboration of the kind of cross-population
# duplicate/mislabeling issue kinship's continuous score can only suggest.
clonality_group_table <- function(mlg_groups, population, sample_ids) {
  sizes <- lengths(mlg_groups)
  shared <- mlg_groups[sizes > 1L]
  if (!length(shared)) {
    return(data.table::data.table(
      mlg_id = character(), n_members = integer(), samples = character(),
      populations = character(), cross_population = logical()
    ))
  }
  data.table::rbindlist(lapply(seq_along(shared), function(i) {
    members <- shared[[i]]
    pops <- unique(population[match(members, sample_ids)])
    data.table::data.table(
      mlg_id = names(shared)[i] %||% paste0("MLG.", i),
      n_members = length(members),
      samples = paste(members, collapse = ", "),
      populations = paste(sort(pops), collapse = ", "),
      cross_population = length(pops) > 1L
    )
  }))
}

# Genotype accumulation curve (Ia/rbarD's companion diagnostic): resamples
# increasing numbers of loci and counts how many distinct MLGs they resolve,
# summarized as a mean +/- 95% quantile envelope per loci count rather than
# poppr::genotype_curve()'s own per-count boxplots -- a real marker panel
# here has hundreds of loci, where hundreds of individual boxplots are
# illegible; a continuous mean/envelope curve reads cleanly at that scale.
clonality_curve_summary <- function(gc, replicates) {
  if (adegenet::nLoc(gc) < 2L || replicates <= 0L) {
    return(data.table::data.table(n_loci = integer(), mean_mlg = numeric(),
                                  q025_mlg = numeric(), q975_mlg = numeric()))
  }
  raw <- tryCatch(
    poppr::genotype_curve(gc, sample = replicates, plot = FALSE, quiet = TRUE),
    error = function(e) NULL
  )
  if (is.null(raw)) {
    return(data.table::data.table(n_loci = integer(), mean_mlg = numeric(),
                                  q025_mlg = numeric(), q975_mlg = numeric()))
  }
  long <- data.table::data.table(
    n_loci = as.integer(rep(colnames(raw), each = nrow(raw))),
    n_mlg = as.numeric(raw)
  )
  long[, .(mean_mlg = mean(n_mlg), q025_mlg = stats::quantile(n_mlg, 0.025),
          q975_mlg = stats::quantile(n_mlg, 0.975)), by = n_loci][order(n_loci)]
}

run_clonality <- function(genotype, sample_ids, metadata, seed,
                          curve_replicates = 100L, ia_permutations = 0L) {
  matched <- match(sample_ids, metadata$sample)
  population <- trimws(as.character(metadata$population[matched]))
  if (anyNA(population) || any(!nzchar(population))) {
    stop("Clonality analysis requires a non-missing population for every retained sample", call. = FALSE)
  }
  if (ncol(genotype) < 2L) {
    stop("Clonality analysis requires at least two polymorphic loci; ", ncol(genotype), " available", call. = FALSE)
  }
  public_ids <- public_sample_ids(metadata, sample_ids)

  gid <- clonality_encode_genind(genotype, public_ids, population)
  gc <- poppr::as.genclone(gid)

  set.seed(as.integer(seed))
  summary_raw <- poppr::poppr(gc, plot = FALSE, sample = as.integer(ia_permutations))
  summary_dt <- clonality_rename_summary(summary_raw)

  mlg_groups <- poppr::mlg.id(gc)
  groups <- clonality_group_table(mlg_groups, population, public_ids)

  curve <- clonality_curve_summary(gc, as.integer(curve_replicates))

  n_mlg_total <- suppressWarnings(as.integer(summary_dt[population == "Total", mlg]))
  if (!length(n_mlg_total)) n_mlg_total <- NA_integer_

  list(summary = summary_dt, groups = groups, curve = curve,
      n_mlg_total = n_mlg_total, curve_replicates = as.integer(curve_replicates))
}

plot_clonality <- function(result, cfg, dirs) {
  curve <- result$curve
  if (!nrow(curve)) return(invisible(NULL))
  fmts <- cfg$output$figure_formats; dpi <- cfg$output$dpi
  style <- figure_style_name(cfg)
  accent <- unname(expand_figure_palette(figure_style_profile(style), 1L, "colours"))
  highlight <- if (identical(style, "grayscale-safe")) "#252525" else "#B2182B"

  p <- ggplot2::ggplot(curve, ggplot2::aes(n_loci, mean_mlg)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = q025_mlg, ymax = q975_mlg), fill = accent, alpha = 0.25) +
    ggplot2::geom_line(colour = accent, linewidth = 0.7)
  if (is.finite(result$n_mlg_total)) {
    p <- p + ggplot2::geom_hline(yintercept = result$n_mlg_total, linetype = "dashed", colour = highlight)
  }
  p <- p +
    ggplot2::scale_x_continuous(labels = scales::label_comma()) +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    ggplot2::labs(
      title = "Genotype accumulation curve",
      subtitle = sprintf(
        "Distinct multilocus genotypes resolved when subsampling loci (mean and 95%% envelope, %s replicates); dashed line: %s MLGs with the full marker set",
        scales::comma(result$curve_replicates), scales::comma(result$n_mlg_total)
      ),
      x = "Number of loci sampled", y = "Multilocus genotypes (MLG)"
    ) + theme_publication(figure_base_size(cfg))
  save_plot(p, "58_genotype_accumulation_curve", dirs, fmts, 8, 5.5, dpi)
}
