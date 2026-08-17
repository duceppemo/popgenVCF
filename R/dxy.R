# Nei's (1987) Dxy: absolute nucleotide divergence between two populations
# -- the probability that a randomly drawn allele from population X differs
# from a randomly drawn allele from population Y at the same locus,
# averaged across usable loci. Found by a standard-toolkit gap audit
# (SambaR's calcdistance() and PopGenome both list Dxy alongside FST as a
# core differentiation statistic) and confirmed genuinely distinct from
# every FST-family measure this package already computes (FST, Weir and
# Goudet's population-specific beta, Jost's D): all three are normalized by
# within-population diversity, so a pair can show identically low FST with
# either low Dxy (recent divergence, ongoing gene flow) or high Dxy (old
# divergence, high shared ancestral diversity) -- two different demographic
# histories FST alone cannot distinguish (Cruickshank and Hahn 2014).
#
# No finite-sample bias correction is applied, matching how Dxy is defined
# and reported in the literature (unlike Jost's D's Hs/Ht, which do use a
# bias-corrected estimator). Purely pairwise by construction: "divergence
# between two specific gene pools" has no single natural multi-population
# generalization the way Jost's D's Hs/Ht do, so the reported "global" value
# below is the mean of the pairwise matrix, not a separate multi-population
# estimator -- an intentionally different, weaker claim than FST/Jost's D's
# own "global" values, documented here and in the written output.
#
# Reuses compute_diversity()'s already-computed per-population, per-locus
# allele frequencies (context$diversity_full$locus) -- no new per-locus
# computation. Loci where either population in the pair has zero calls are
# excluded, the same convention compute_jost_d()/build_population_nj_tree()
# already use.

dxy_pair <- function(locus_table, pop1, pop2) {
  sub <- locus_table[population %in% c(pop1, pop2) & n_called > 0L]
  usable <- sub[, .N, by = snp_id][N == 2L, snp_id]
  if (!length(usable)) return(list(dxy = NA_real_, n_snps = 0L))
  sub <- sub[snp_id %in% usable]
  wide <- data.table::dcast(sub, snp_id ~ population, value.var = "alternate_allele_frequency")
  p1 <- wide[[pop1]]; p2 <- wide[[pop2]]
  per_locus <- p1 * (1 - p2) + p2 * (1 - p1)
  list(dxy = mean(per_locus), n_snps = length(usable))
}

compute_dxy <- function(locus_table) {
  populations <- sort(unique(locus_table$population))
  empty_long <- data.table::data.table(
    population_1 = character(), population_2 = character(),
    dxy = numeric(), dxy_n_snps = integer()
  )
  if (length(populations) < 2L) {
    return(list(global = NA_real_, long = empty_long, matrix = matrix(numeric(0), 0L, 0L)))
  }

  pairs <- utils::combn(populations, 2, simplify = FALSE)
  long <- data.table::rbindlist(lapply(pairs, function(pp) {
    res <- dxy_pair(locus_table, pp[1], pp[2])
    data.table::data.table(
      population_1 = pp[1], population_2 = pp[2],
      dxy = res$dxy, dxy_n_snps = res$n_snps
    )
  }))
  global <- if (nrow(long)) mean(long$dxy, na.rm = TRUE) else NA_real_
  mat <- matrix(0, length(populations), length(populations), dimnames = list(populations, populations))
  if (nrow(long)) for (i in seq_len(nrow(long))) {
    mat[long$population_1[i], long$population_2[i]] <- long$dxy[i]
    mat[long$population_2[i], long$population_1[i]] <- long$dxy[i]
  }
  list(global = global, long = long, matrix = mat)
}
