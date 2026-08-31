haversine_matrix <- function(lat, lon, labels) {
  rad <- pi / 180; lat <- lat * rad; lon <- lon * rad
  n <- length(lat); m <- matrix(0, n, n, dimnames = list(labels, labels)); R <- 6371.0088
  for (i in seq_len(n - 1L)) for (j in (i + 1L):n) {
    dlat <- lat[j] - lat[i]; dlon <- lon[j] - lon[i]
    a <- sin(dlat/2)^2 + cos(lat[i]) * cos(lat[j]) * sin(dlon/2)^2
    # a is mathematically bounded in [0, 1], but is a sum of two
    # independently-rounded trig terms -- for widely separated or
    # near-antipodal coordinates, floating-point error can push it
    # fractionally above 1, making sqrt(1 - a) silently return NaN (a real
    # bug found in a pre-release audit) instead of the ~0 the true geometry
    # implies. Clamping before both sqrt() calls is the standard fix for
    # this well-known haversine-formula pitfall.
    a <- min(max(a, 0), 1)
    d <- 2 * R * atan2(sqrt(a), sqrt(1 - a)); m[i,j] <- m[j,i] <- d
  }
  m
}

run_mantel_ibd <- function(genetic_distance, metadata, geographic_columns, permutations = 999L, seed = 42L) {
  if (!all(geographic_columns %in% names(metadata))) return(NULL)
  distance_ids <- rownames(genetic_distance)
  identity_column <- if ("public_sample" %in% names(metadata)) {
    "public_sample"
  } else {
    "sample"
  }
  m <- metadata[match(distance_ids, metadata[[identity_column]])]
  lat <- as.numeric(m[[geographic_columns[1]]]); lon <- as.numeric(m[[geographic_columns[2]]])
  keep <- is.finite(lat) & is.finite(lon) & abs(lat) <= 90 & abs(lon) <= 180
  if (sum(keep) < 4L) return(NULL)
  gd <- genetic_distance[keep, keep, drop = FALSE]
  geo <- haversine_matrix(lat[keep], lon[keep], distance_ids[keep])
  set.seed(seed)
  mantel <- vegan::mantel(stats::as.dist(gd), stats::as.dist(geo), permutations = permutations, method = "pearson")

  # Partial Mantel, controlling for population identity (0 = same
  # population, 1 = different): the standard landscape-genetics follow-up
  # question -- does the IBD correlation hold once population membership is
  # controlled for, or is it purely an artifact of population clustering?
  # Reuses vegan::mantel.partial(), already a dependency (vegan::mantel()
  # above). The binary same/different indicator only fully collapses onto
  # geographic distance when there are exactly two populations (a single
  # between-population distance value); with three or more it leaves real,
  # testable residual variance even when coordinates are population-level
  # representative points rather than individual GPS (verified empirically
  # against a synthetic multi-population fixture before shipping -- see
  # NEWS.md), so this is not degenerate for this package's own quickstart
  # dataset (8 populations).
  partial <- NULL
  if ("population" %in% names(m)) {
    population <- m$population[keep]
    if (data.table::uniqueN(population) >= 2L) {
      pop_dissim <- outer(population, population, `!=`) * 1
      set.seed(seed)
      partial <- vegan::mantel.partial(
        stats::as.dist(gd), stats::as.dist(geo), stats::as.dist(pop_dissim),
        permutations = permutations, method = "pearson"
      )
    }
  }

  idx <- upper.tri(gd)
  pairs <- data.table::data.table(genetic_distance = gd[idx], geographic_distance_km = geo[idx])
  fit <- stats::lm(genetic_distance ~ log1p(geographic_distance_km), data = pairs)
  summary_dt <- data.table::data.table(
    mantel_r = unname(mantel$statistic), mantel_p = mantel$signif,
    slope = stats::coef(fit)[2], r_squared = summary(fit)$r.squared,
    partial_mantel_r = if (!is.null(partial)) unname(partial$statistic) else NA_real_,
    partial_mantel_p = if (!is.null(partial)) partial$signif else NA_real_
  )
  list(mantel = mantel, partial_mantel = partial, pairs = pairs, model = fit, summary = summary_dt)
}

plot_ibd <- function(x, cfg, dirs) {
  if (is.null(x)) return(invisible(NULL))
  accent <- unname(expand_figure_palette(
    figure_style_profile(figure_style_name(cfg)), 1L, "colours"
  ))
  p <- ggplot2::ggplot(x$pairs, ggplot2::aes(geographic_distance_km, genetic_distance)) +
    ggplot2::geom_point(
      shape = 21, size = 2.2, stroke = 0.35,
      colour = "#1A1A1A", fill = accent, alpha = .45
    ) +
    ggplot2::geom_smooth(
      method = "lm", formula = y ~ log1p(x), se = TRUE,
      colour = "#1A1A1A", fill = accent,
      linewidth = 0.8, alpha = 0.18
    ) +
    ggplot2::labs(
      title = "Isolation by distance",
      subtitle = if (is.finite(x$summary$partial_mantel_r)) {
        sprintf(
          "Mantel r = %.3f, p = %.4f (partial, controlling for population: r = %.3f, p = %.4f)",
          x$summary$mantel_r, x$summary$mantel_p,
          x$summary$partial_mantel_r, x$summary$partial_mantel_p
        )
      } else {
        sprintf("Mantel r = %.3f, p = %.4f", x$summary$mantel_r, x$summary$mantel_p)
      },
      caption = sprintf(
        "Curve: linear model of genetic distance on log(1 + geographic distance); %s pairwise comparisons.",
        scales::comma(nrow(x$pairs))
      ),
      x = "Geographic distance (km)", y = "IBS-derived genetic distance"
    ) +
    theme_publication(figure_base_size(cfg))
  save_plot(p, "12_isolation_by_distance", dirs, cfg$output$figure_formats, 7.5, 5.5, cfg$output$dpi)
}
