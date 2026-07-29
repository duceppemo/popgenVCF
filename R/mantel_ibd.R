haversine_matrix <- function(lat, lon, labels) {
  rad <- pi / 180; lat <- lat * rad; lon <- lon * rad
  n <- length(lat); m <- matrix(0, n, n, dimnames = list(labels, labels)); R <- 6371.0088
  for (i in seq_len(n - 1L)) for (j in (i + 1L):n) {
    dlat <- lat[j] - lat[i]; dlon <- lon[j] - lon[i]
    a <- sin(dlat/2)^2 + cos(lat[i]) * cos(lat[j]) * sin(dlon/2)^2
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
  idx <- upper.tri(gd)
  pairs <- data.table::data.table(genetic_distance = gd[idx], geographic_distance_km = geo[idx])
  fit <- stats::lm(genetic_distance ~ log1p(geographic_distance_km), data = pairs)
  list(mantel = mantel, pairs = pairs, model = fit,
       summary = data.table::data.table(mantel_r = unname(mantel$statistic), mantel_p = mantel$signif,
                                        slope = stats::coef(fit)[2], r_squared = summary(fit)$r.squared))
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
      subtitle = sprintf(
        "Mantel r = %.3f, p = %.4f",
        x$summary$mantel_r, x$summary$mantel_p
      ),
      caption = sprintf(
        "Curve: linear model of genetic distance on log(1 + geographic distance); %s pairwise comparisons.",
        scales::comma(nrow(x$pairs))
      ),
      x = "Geographic distance (km)", y = "IBS-derived genetic distance"
    ) +
    theme_publication(figure_base_size(cfg))
  save_plot(p, "12_isolation_by_distance", dirs, cfg$output$figure_formats, 7.5, 5.5, cfg$output$dpi)
}
