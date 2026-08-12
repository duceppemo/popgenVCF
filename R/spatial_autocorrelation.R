# Spatial autocorrelation analysis (Smouse and Peakall 1999): bins sample
# pairs into geographic-distance classes and computes an autocorrelation
# coefficient r per class from a squared individual genetic-distance matrix,
# producing a correlogram -- a distance-resolved view of isolation by
# distance, complementing the single overall Mantel r the ibd module already
# computes.
#
# Per-locus genetic distance for two diploid individuals reduces
# algebraically to the squared difference in genotype dosage for biallelic
# markers (each individual's per-locus allele-count vector is
# (2 - dosage, dosage); Smouse and Peakall's per-locus term is half the
# squared Euclidean distance between those two-column vectors, which is
# exactly (dosage_i - dosage_j)^2 -- verified directly, not just derived by
# hand, by sourcing the reference implementation's own R source
# (PopGenReport::gd.smouse(), Adamack and Gruber) and comparing its output
# to this reduction on a synthetic fixture: matched to floating-point
# precision. The multilocus distance is `stats::dist(..., "euclidean")^2`,
# base R's own pairwise-complete-missing-data handling (a deliberate,
# documented difference from gd.smouse()'s own locus-mean imputation for
# missing calls).
#
# The r statistic uses the classical Gower (1966) double-centering of the
# squared-distance matrix, matching PopGenReport::spautocor() exactly --
# verified directly against its source (max absolute difference in r,
# ~1e-16, machine precision) on the same synthetic fixture before shipping
# (see NEWS.md). PopGenReport itself is not a usable dependency here (its
# own package dependencies -- raster/terra/gdistance -- do not build in
# this environment and are unrelated to this specific functionality, which
# is pure R with no compiled/geospatial dependencies of its own), so this
# reimplements only the small, self-contained pieces actually needed.
#
# Significance: permutes the assignment of individuals to genetic-distance
# matrix rows/columns jointly (preserving the genetic-distance matrix's own
# symmetry and value multiset), the same standard Mantel-style permutation
# convention `vegan::mantel()` already uses elsewhere in this package (not
# a bit-for-bit reproduction of spautocor()'s own internal shuffle, which
# independently reassigns the lower and upper triangles and can produce an
# asymmetric matrix -- a real quirk in that reference implementation, not
# replicated here).

spatial_autocorrelation_r <- function(gd, ed, bins) {
  dimen <- nrow(gd)
  sgd <- sum(gd, na.rm = TRUE)
  rs <- rowSums(gd, na.rm = TRUE)
  cd <- 0.5 * (-gd + (matrix(rs, dimen, dimen) + matrix(rs, dimen, dimen, byrow = TRUE)) / dimen - sgd / dimen^2)

  ed2 <- ed
  ed2[upper.tri(ed2)] <- NA_real_
  diag(ed2) <- NA_real_
  steps <- signif(diff(range(ed2, na.rm = TRUE)) / bins, 4)
  bin_upper <- steps * seq_len(bins)
  r <- rep(NA_real_, bins); n_pairs <- integer(bins)
  for (d in seq_len(bins)) {
    idx <- which(ed2 <= d * steps & ed2 > (d - 1) * steps, arr.ind = TRUE)
    n_pairs[d] <- nrow(idx)
    if (!nrow(idx)) next
    cx <- sum(cd[idx])
    cxii <- sum(diag(cd)[idx[, 1]])
    cxjj <- sum(diag(cd)[idx[, 2]])
    denom <- cxii + cxjj
    r[d] <- if (denom == 0) NA_real_ else 2 * cx / denom
  }
  data.table::data.table(bin_upper = bin_upper, n_pairs = n_pairs, r = r)
}

run_spatial_autocorrelation <- function(genotype, sample_ids, metadata, geographic_columns,
                                        bins = 10L, permutations = 999L, seed = 42L) {
  if (!all(geographic_columns %in% names(metadata))) return(NULL)
  identity_column <- if ("public_sample" %in% names(metadata)) "public_sample" else "sample"
  m <- metadata[match(sample_ids, metadata[[identity_column]])]
  lat <- as.numeric(m[[geographic_columns[1]]]); lon <- as.numeric(m[[geographic_columns[2]]])
  keep <- is.finite(lat) & is.finite(lon) & abs(lat) <= 90 & abs(lon) <= 180
  if (sum(keep) < 4L) return(NULL)

  labels <- sample_ids[keep]
  gd <- as.matrix(stats::dist(genotype[keep, , drop = FALSE], method = "euclidean"))^2
  dimnames(gd) <- list(labels, labels)
  ed <- haversine_matrix(lat[keep], lon[keep], labels)
  if (!is.finite(diff(range(ed))) || diff(range(ed)) <= 0) return(NULL)

  n <- nrow(gd)
  observed <- spatial_autocorrelation_r(gd, ed, bins)
  if (permutations <= 0L) {
    observed[, `:=`(p_value = NA_real_, null_lower = NA_real_, null_upper = NA_real_)]
    return(observed[])
  }
  set.seed(seed)
  null_r <- matrix(NA_real_, permutations, bins)
  for (p in seq_len(permutations)) {
    perm <- sample.int(n)
    null_r[p, ] <- spatial_autocorrelation_r(gd[perm, perm], ed, bins)$r
  }
  observed[, p_value := vapply(seq_len(bins), function(b) {
    if (is.na(r[b])) return(NA_real_)
    mean(abs(null_r[, b]) >= abs(r[b]), na.rm = TRUE)
  }, numeric(1L))]
  observed[, null_lower := apply(null_r, 2, stats::quantile, probs = 0.025, na.rm = TRUE)]
  observed[, null_upper := apply(null_r, 2, stats::quantile, probs = 0.975, na.rm = TRUE)]
  observed[]
}

plot_spatial_autocorrelation <- function(result, cfg, dirs) {
  if (is.null(result) || !nrow(result)) return(invisible(NULL))
  accent <- unname(expand_figure_palette(
    figure_style_profile(figure_style_name(cfg)), 1L, "colours"
  ))
  p <- ggplot2::ggplot(result, ggplot2::aes(bin_upper, r)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = null_lower, ymax = null_upper),
      fill = accent, alpha = 0.18, na.rm = TRUE
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "#595959", linewidth = 0.4) +
    ggplot2::geom_line(colour = "#1A1A1A", linewidth = 0.6, na.rm = TRUE) +
    ggplot2::geom_point(
      shape = 21, size = 2.4, stroke = 0.4,
      colour = "#1A1A1A", fill = accent, na.rm = TRUE
    ) +
    ggplot2::labs(
      title = "Spatial autocorrelation correlogram",
      subtitle = "Shaded band: 95% permutation envelope under no spatial structure",
      caption = sprintf(
        "Smouse and Peakall (1999); %s equal-width geographic-distance classes, upper bound shown.",
        nrow(result)
      ),
      x = "Geographic distance (km, upper bound of class)", y = "Autocorrelation r"
    ) +
    theme_publication(figure_base_size(cfg))
  save_plot(p, "50_spatial_autocorrelation", dirs, cfg$output$figure_formats, 7.5, 5.5, cfg$output$dpi)
}
