# Jost's (2008) D, an alternative population-differentiation measure to FST
# specifically designed to avoid FST's well-known mathematical constraint by
# within-population heterozygosity (Jost 2008, "GST and its relatives do not
# measure differentiation", Molecular Ecology): a highly polymorphic marker
# set can never show a high FST even under strong differentiation, since
# FST's denominator is bounded by within-population heterozygosity. Uses the
# Nei and Chesser (1983) bias-corrected Hs/Ht estimators, matching the
# standard reference implementation (mmod::D_Jost(), Winter 2012) exactly --
# cross-checked directly against it (both the global and pairwise cases, on
# a synthetic multi-population fixture with unequal sample sizes) to
# floating-point precision before shipping, not just formula-derived by hand
# (see NEWS.md).
#
# Multi-locus estimates average per-locus Hs_est/Ht_est (arithmetic mean --
# this package's choice and mmod's own default `hsht_mean = "arithmetic"`)
# and recompute D from those averages, rather than averaging per-locus D
# values directly.
#
# Reuses compute_diversity()'s already-computed per-population, per-locus
# allele frequencies and sample sizes (context$diversity_full$locus) -- no
# new per-locus computation. Loci where any population in the comparison has
# zero calls are excluded (Hs/Ht are undefined there), the same convention
# population_tree.R already uses for Nei's distance.

jost_hs_ht <- function(freq_alt_mat, n_called_mat) {
  # freq_alt_mat / n_called_mat: populations (rows) x loci (cols).
  n_pop <- nrow(freq_alt_mat)
  harm_n <- n_pop / colSums(1 / n_called_mat)
  he_raw <- 2 * freq_alt_mat * (1 - freq_alt_mat)
  hs_raw <- colMeans(he_raw)
  hs_est <- (2 * harm_n / (2 * harm_n - 1)) * hs_raw
  p_bar <- colMeans(freq_alt_mat)
  ht_raw <- 2 * p_bar * (1 - p_bar)
  ht_est <- ht_raw + hs_est / (2 * harm_n * n_pop)
  list(hs_est = hs_est, ht_est = ht_est)
}

jost_d_from_hs_ht <- function(hs_est, ht_est, n_pop) {
  ifelse(is.na(hs_est) | is.na(ht_est) | hs_est >= 1, NA_real_,
        (ht_est - hs_est) / (1 - hs_est) * (n_pop / (n_pop - 1)))
}

jost_d_group <- function(locus_table, group_populations) {
  n_pop <- length(group_populations)
  sub <- locus_table[population %in% group_populations & n_called > 0L]
  usable <- sub[, .N, by = snp_id][N == n_pop, snp_id]
  if (!length(usable)) return(list(d = NA_real_, n_snps = 0L))
  sub <- sub[snp_id %in% usable]

  wide_p <- data.table::dcast(sub, population ~ snp_id, value.var = "alternate_allele_frequency")
  wide_n <- data.table::dcast(sub, population ~ snp_id, value.var = "n_called")
  data.table::setorder(wide_p, population); data.table::setorder(wide_n, population)
  freq_mat <- as.matrix(wide_p[, -1, with = FALSE])
  n_mat <- as.matrix(wide_n[, -1, with = FALSE])

  hh <- jost_hs_ht(freq_mat, n_mat)
  global_hs <- mean(hh$hs_est); global_ht <- mean(hh$ht_est)
  list(d = jost_d_from_hs_ht(global_hs, global_ht, n_pop), n_snps = length(usable))
}

compute_jost_d <- function(locus_table) {
  populations <- sort(unique(locus_table$population))
  empty_long <- data.table::data.table(
    population_1 = character(), population_2 = character(),
    jost_d = numeric(), jost_d_n_snps = integer()
  )
  if (length(populations) < 2L) {
    return(list(global = NA_real_, long = empty_long,
                matrix = matrix(numeric(0), 0L, 0L)))
  }

  global <- jost_d_group(locus_table, populations)$d

  pairs <- utils::combn(populations, 2, simplify = FALSE)
  long <- data.table::rbindlist(lapply(pairs, function(pp) {
    res <- jost_d_group(locus_table, pp)
    data.table::data.table(
      population_1 = pp[1], population_2 = pp[2],
      jost_d = res$d, jost_d_n_snps = res$n_snps
    )
  }))
  mat <- matrix(0, length(populations), length(populations), dimnames = list(populations, populations))
  if (nrow(long)) for (i in seq_len(nrow(long))) {
    mat[long$population_1[i], long$population_2[i]] <- long$jost_d[i]
    mat[long$population_2[i], long$population_1[i]] <- long$jost_d[i]
  }
  list(global = global, long = long, matrix = mat)
}
