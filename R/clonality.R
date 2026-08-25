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
# Two genuinely different marker sets feed this module, deliberately, not
# out of inconsistency:
#
#   - `mlg.id()` (exact multilocus-genotype identity / duplicate detection)
#     uses the FULL, unpruned QC-passing locus set. Fewer markers make it
#     *more* likely that two genuinely different individuals coincidentally
#     match at every retained locus (a false "duplicate" call) -- exact
#     identity needs maximum discriminating power, so thinning the marker
#     set here would be a real regression in this table's accuracy, not a
#     harmless shortcut.
#
#   - `poppr()`'s Ia/rbarD diversity summary and its companion genotype
#     accumulation curve (`genotype_curve()`) instead use the LD-pruned
#     locus set. Originally these reused the full unpruned set too "for
#     consistency with AMOVA" (see NEWS.md's earlier history), but that
#     turned out to be wrong on two independent grounds, found together via
#     a real production run:
#     1. Performance: a real 50-sample, 561,767-locus unpruned cohort took
#        29+ hours in poppr()'s Ia/rbarD computation alone, still running
#        when investigated. Direct scaling measurement (500 to 20,000
#        synthetic loci, everything else held fixed) showed poppr()'s
#        runtime growing superlinearly in locus count, with the local
#        growth exponent climbing from ~1.0 to ~1.6 across that range --
#        not a flat per-locus cost, and clearly heading toward quadratic at
#        real-world scale. Extrapolating that measured growth curve to
#        561,767 loci predicts ~28.5 hours, matching the real run almost
#        exactly. `genotype_curve()`'s resampling has the same problem
#        (timed out past 5 minutes at just 20,000 loci in the same
#        investigation). `encode`/`as.genclone`/`mlg.id()` are all
#        negligible by comparison at every scale tested (sub-second even at
#        20,000 loci) -- the cost lives entirely inside poppr's own
#        Ia/rbarD and genotype-curve code, not in anything this package
#        does to prepare its input.
#     2. Methodology: Ia/rbarD's own null-model interpretation assumes the
#        input loci start approximately independent -- that is what
#        "non-random association among loci" is testing for. Feeding it
#        hundreds of thousands of physically linked SNPs mechanically
#        inflates the appearance of non-random multilocus association
#        through ordinary linkage disequilibrium, an artifact of marker
#        density, not a real signal of clonality. The wiki's own
#        quickstart walkthrough already flagged this exact caveat before
#        this change ("a positive value here largely reflects ordinary
#        physical linkage among nearby SNPs, not clonality"). Running on
#        the LD-pruned, approximately-independent set is the statistically
#        appropriate input for this specific statistic, not merely a
#        workaround for its performance -- the same set already used for
#        kinship/PCA/DAPC elsewhere in this pipeline.
#
#   Consequence for interpreting the outputs: `56_MLG_diversity_summary.tsv`
#   (MLG/eMLG/Shannon/Simpson/Ia/rbarD) now describes genotypic diversity as
#   measured on the LD-pruned marker panel, while `57_MLG_groups.tsv` (actual
#   sample-sharing-a-genotype pairs/groups) and the MLG count implicit in
#   duplicate detection still reflect the full unpruned panel. These two
#   tables' MLG-related numbers are consequently not directly comparable to
#   each other post-change the way they trivially were when both used the
#   same marker set -- each is now computed on the marker set appropriate to
#   its own statistical question.
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

clonality_empty_curve <- function() {
  data.table::data.table(n_loci = integer(), mean_mlg = numeric(),
                         q025_mlg = numeric(), q975_mlg = numeric())
}

# Genotype accumulation curve (Ia/rbarD's companion diagnostic): resamples
# increasing numbers of loci and counts how many distinct MLGs they resolve,
# summarized as a mean +/- 95% quantile envelope per loci count rather than
# poppr::genotype_curve()'s own per-count boxplots -- a real marker panel
# here has hundreds of loci, where hundreds of individual boxplots are
# illegible; a continuous mean/envelope curve reads cleanly at that scale.
# Runs on the LD-pruned locus set, like poppr()'s own Ia/rbarD summary above
# -- see this file's top-of-file comment for why (performance and
# methodology both point the same way).
clonality_curve_summary <- function(gc, replicates) {
  if (adegenet::nLoc(gc) < 2L || replicates <= 0L) {
    return(clonality_empty_curve())
  }
  raw <- tryCatch(
    poppr::genotype_curve(gc, sample = replicates, plot = FALSE, quiet = TRUE),
    error = function(e) NULL
  )
  if (is.null(raw)) {
    return(clonality_empty_curve())
  }
  long <- data.table::data.table(
    n_loci = as.integer(rep(colnames(raw), each = nrow(raw))),
    n_mlg = as.numeric(raw)
  )
  long[, .(mean_mlg = mean(n_mlg), q025_mlg = stats::quantile(n_mlg, 0.025),
          q975_mlg = stats::quantile(n_mlg, 0.975)), by = n_loci][order(n_loci)]
}

# poppr::poppr()'s Ia/rbarD computation (pair_diffs() -> pairdiffs(), a
# compiled C routine) has a real, empirically-confirmed 32-bit integer
# overflow: found via a real production run generating release-candidate
# evidence against the full, real chr22 1000 Genomes dataset (2504 samples,
# 26 populations, 2028 QC-passing loci). It segfaulted computing poppr's
# automatic pooled-"Total" Ia value -- the flattened pairs-by-locus index
# space for that group, 2504*2503/2 pairs * 2028 loci =~ 6.35 billion,
# overflows a signed 32-bit index (max ~2.15 billion); bisecting the 26
# populations into two independent 13-population/~1250-sample halves (each
# comfortably under the overflow threshold at ~1.56 billion) reproduced
# cleanly with no crash, confirming the mechanism. This is a real bug in
# poppr's own C code, not something this package can correct.
#
# A segfault cannot be caught by R's own tryCatch (it terminates the whole
# process), so poppr::poppr() is run in a forked child process
# (parallel::mcparallel(), Unix-only -- matching this codebase's existing
# use of fork-based parallelism elsewhere, e.g. mclapply()) specifically so
# a crash there costs only this module's summary table, not every
# already-completed result in the entire pipeline run. Verified directly
# against the real crashing dataset: the forked child segfaults exactly as
# before, but the parent process survives and mccollect() reports the
# failure as NULL rather than terminating.
clonality_empty_summary <- function() {
  data.table::data.table(
    population = character(), n = integer(), mlg = integer(), emlg = numeric(),
    emlg_se = numeric(), shannon_h = numeric(), stoddart_taylor_g = numeric(),
    simpson_lambda = numeric(), evenness_e5 = numeric(),
    expected_heterozygosity = numeric(), ia = numeric(), rbard = numeric(),
    ia_p_value = numeric(), rbard_p_value = numeric()
  )
}

clonality_run_poppr_isolated <- function(gc, ia_permutations) {
  call_poppr <- function() poppr::poppr(gc, plot = FALSE, sample = as.integer(ia_permutations))
  if (!identical(.Platform$OS.type, "unix")) {
    return(tryCatch(call_poppr(), error = function(e) NULL))
  }
  job <- parallel::mcparallel(call_poppr())
  result <- suppressWarnings(parallel::mccollect(job, wait = TRUE))[[1L]]
  if (is.null(result) || inherits(result, "try-error")) NULL else result
}

run_clonality <- function(genotype, ld_genotype, sample_ids, metadata, seed,
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

  # Full, unpruned marker set: exact multilocus-genotype identity only (see
  # this file's top-of-file comment for why this set must stay unpruned).
  gid <- clonality_encode_genind(genotype, public_ids, population)
  gc <- poppr::as.genclone(gid)
  mlg_groups <- poppr::mlg.id(gc)
  groups <- clonality_group_table(mlg_groups, population, public_ids)

  # LD-pruned marker set: poppr()'s Ia/rbarD summary and the genotype
  # accumulation curve, both of which assume approximately independent loci
  # and were the entire real-world performance bottleneck at full-panel
  # scale (see this file's top-of-file comment). Degrades gracefully, the
  # same way an actual poppr() crash already does, rather than failing the
  # whole module, on the rare dataset where LD pruning leaves fewer than 2
  # usable loci.
  ld_pruned_usable <- ncol(ld_genotype) >= 2L
  set.seed(as.integer(seed))
  summary_raw <- if (ld_pruned_usable) {
    gid_ld <- clonality_encode_genind(ld_genotype, public_ids, population)
    gc_ld <- poppr::as.genclone(gid_ld)
    clonality_run_poppr_isolated(gc_ld, ia_permutations)
  } else NULL
  poppr_failed <- is.null(summary_raw)
  summary_dt <- if (poppr_failed) clonality_empty_summary() else clonality_rename_summary(summary_raw)
  curve <- if (ld_pruned_usable) clonality_curve_summary(gc_ld, as.integer(curve_replicates)) else clonality_empty_curve()

  n_mlg_total <- suppressWarnings(as.integer(summary_dt[population == "Total", mlg]))
  if (!length(n_mlg_total)) n_mlg_total <- NA_integer_

  list(summary = summary_dt, groups = groups, curve = curve,
      n_mlg_total = n_mlg_total, curve_replicates = as.integer(curve_replicates),
      poppr_failed = poppr_failed, ld_pruned_usable = ld_pruned_usable)
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
      subtitle = "Distinct multilocus genotypes resolved when subsampling LD-pruned loci",
      caption = sprintf(
        "Mean and 95%% envelope across %s replicates; dashed line: %s MLGs with the full LD-pruned marker set",
        scales::comma(result$curve_replicates), scales::comma(result$n_mlg_total)
      ),
      x = "Number of LD-pruned loci sampled", y = "Multilocus genotypes (MLG)"
    ) + theme_publication(figure_base_size(cfg))
  save_plot(p, "58_genotype_accumulation_curve", dirs, fmts, 8, 5.5, dpi)
}
