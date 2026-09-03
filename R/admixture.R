parse_admixture_cv <- function(text) {
  hit <- regmatches(text, regexpr("CV error \\(K=[0-9]+\\):[[:space:]]*[0-9.eE+-]+", text))
  if (!length(hit) || !nzchar(hit)) return(NULL)
  k <- as.integer(sub(".*K=([0-9]+).*", "\\1", hit))
  value <- as.numeric(sub(".*:[[:space:]]*", "", hit))
  data.table::data.table(K = k, cv_error = value)
}

read_admixture_q <- function(path, sample_file, metadata) {
  if (!file.exists(path)) stopf("Q matrix not found: %s", path)
  if (is.null(sample_file) || !file.exists(sample_file)) {
    stop("An explicit Q sample-order file is required", call. = FALSE)
  }
  if (!"sample" %in% names(metadata)) {
    stop("ADMIXTURE metadata requires a sample column", call. = FALSE)
  }

  ids <- data.table::fread(sample_file, header = FALSE)[[1L]] |> as.character()
  q <- data.table::fread(path, header = FALSE)
  if (nrow(q) != length(ids)) stop("Q rows do not match sample-order file", call. = FALSE)
  q[] <- lapply(q, as.numeric)
  if (any(!is.finite(as.matrix(q)))) stop("Q matrix contains nonnumeric values", call. = FALSE)
  rs <- rowSums(q)
  if (any(rs <= 0)) stop("Q matrix contains zero-sum rows", call. = FALSE)
  q <- q / rs
  data.table::setnames(q, paste0("cluster_", seq_len(ncol(q))))

  q[["sample"]] <- ids
  q <- attach_q_population(q, metadata)
  data.table::setcolorder(
    q, c("sample", intersect("population", names(q)), grep("^cluster_", names(q), value = TRUE))
  )
  q
}

plot_admixture_cv <- function(cv, cfg, dirs) {
  if (!nrow(cv)) return(invisible(NULL))
  accent <- unname(expand_figure_palette(
    figure_style_profile(figure_style_name(cfg)), 1L, "colours"
  ))
  p <- ggplot2::ggplot(cv, ggplot2::aes(K, cv_error)) +
    ggplot2::geom_line(colour = accent, linewidth = 0.8) +
    ggplot2::geom_point(
      shape = 21, size = 3, stroke = 0.55,
      colour = "#1A1A1A", fill = accent
    ) +
    ggplot2::scale_x_continuous(breaks = cv$K) +
    ggplot2::labs(
      title = "Admixture-model cross-validation",
      x = "Number of clusters (K)", y = "Cross-validation error"
    ) + theme_publication(figure_base_size(cfg))
  save_plot(p, "13_ADMIXTURE_CV", dirs, cfg$output$figure_formats, 7, 5, cfg$output$dpi)
}

# Orders populations by similarity of their mean per-cluster ancestry
# proportions (average-linkage hierarchical clustering on Euclidean distance
# between population-mean Q-matrix rows, the same linkage convention already
# used for the kinship dendrogram elsewhere in this package), so that
# populations with similar ancestry composition end up adjacent in the
# population-organized membership plots -- alphabetical order otherwise
# places genetically similar populations arbitrarily far apart. Falls back
# to alphabetical order with fewer than two populations (nothing to cluster).
population_ancestry_similarity_order <- function(x, clusters) {
  pop_means <- x[, lapply(.SD, mean), by = population, .SDcols = clusters]
  data.table::setorder(pop_means, population)
  populations <- pop_means$population
  if (length(populations) < 2L) return(populations)
  mat <- as.matrix(pop_means[, ..clusters])
  rownames(mat) <- populations
  hc <- stats::hclust(stats::dist(mat), method = "average")
  hc$labels[hc$order]
}

plot_q_matrix <- function(q, k, cfg, dirs, prefix = "ADMIXTURE_Q",
                          title = NULL, subtitle = NULL,
                          subtitle_is_warning = FALSE, sample_labels = NULL,
                          order_mode = c("population", "data_driven"),
                          y_label = "Ancestry proportion") {
  order_mode <- match.arg(order_mode)
  clusters <- grep("^cluster_", names(q), value = TRUE)
  x <- data.table::copy(q)
  if (is.null(sample_labels)) sample_labels <- x$sample
  sample_labels <- as.character(sample_labels)
  if (length(sample_labels) != nrow(x) || anyNA(sample_labels) ||
      any(!nzchar(sample_labels)) || anyDuplicated(sample_labels)) {
    stop("Membership plot sample labels must be unique and non-empty", call. = FALSE)
  }
  data.table::set(x, j = "sample_label", value = sample_labels)
  membership <- as.matrix(x[, ..clusters])
  x[, dominant := max.col(membership, ties.method = "first")]
  x[, dominant_membership := apply(membership, 1L, max)]
  if (identical(order_mode, "population")) {
    if (!"population" %in% names(x)) {
      stop("Population-organized membership plots require a population column", call. = FALSE)
    }
    population_levels <- population_ancestry_similarity_order(x, clusters)
    x[, population := factor(population, levels = population_levels)]
    data.table::setorderv(
      x, c("population", "dominant", clusters, "sample_label"),
      c(1, 1, rep(-1, length(clusters)), 1)
    )
  } else {
    data.table::setorderv(
      x, c("dominant", "dominant_membership", clusters, "sample_label"),
      c(1, -1, rep(-1, length(clusters)), 1)
    )
  }
  x[, order := seq_len(.N)]
  cluster_boundaries <- numeric()
  if (identical(order_mode, "data_driven")) {
    cluster_ends <- x[, .(end = max(order)), by = dominant][order(dominant), end]
    if (length(cluster_ends) > 1L) {
      cluster_boundaries <- utils::head(cluster_ends, -1L) + 0.5
    }
  }
  plot_width <- min(48, max(10, nrow(x) * 0.08))
  points_per_sample <- plot_width * 72 / nrow(x)
  axis_label_size <- max(1, min(7, points_per_sample * 0.8))
  # facet_grid(~population, space = "free_x") below gives each population's
  # strip exactly its own panel's width (proportional to its sample count) --
  # a population with very few samples gets a narrow strip with no room for
  # its own label, and ggplot2 clips overflowing strip text rather than
  # wrapping or shrinking it (confirmed directly against a real production
  # report: a 2-sample "Ro2-3" population's strip rendered as just "o2").
  # Sized the same way axis_label_size above already is: from the tightest
  # constraint across every population's own (sample count, label length)
  # pair, floored to stay legible and capped at the theme's own base size so
  # a plot with only large, evenly-sized populations isn't affected.
  strip_text_size <- if (identical(order_mode, "population") && "population" %in% names(x)) {
    pop_sizes <- x[, .N, by = population]
    fits <- (pop_sizes$N * points_per_sample) / (pmax(1L, nchar(as.character(pop_sizes$population))) * 0.62)
    max(4, min(figure_base_size(cfg), min(fits)))
  } else {
    figure_base_size(cfg)
  }
  id_vars <- c("sample", "sample_label", "order")
  if ("population" %in% names(x)) id_vars <- c(id_vars, "population")
  long <- data.table::melt(x, id.vars = id_vars, measure.vars = clusters,
                           variable.name = "cluster", value.name = "ancestry")
  default_title <- sprintf(
    "%s (K = %d)",
    switch(
      prefix,
      fastStructure_Q = "fastStructure ancestry proportions",
      sNMF_Q = "Sparse non-negative matrix factorization ancestry coefficients",
      STRUCTURE_Q = "STRUCTURE membership proportions",
      DAPC_membership = "Discriminant analysis of principal components membership probabilities",
      "Admixture-model ancestry proportions"
    ),
    k
  )
  plot_title <- title %||% default_title
  if (identical(order_mode, "data_driven")) {
    plot_title <- paste0(plot_title, " - data-driven cluster order")
  }
  cluster_colours <- cluster_palette(clusters, figure_style_name(cfg))
  p <- ggplot2::ggplot(
    long, ggplot2::aes(order, ancestry, fill = cluster)
  ) +
    ggplot2::geom_col(width = 1) +
    ggplot2::scale_fill_manual(
      values = cluster_colours,
      breaks = clusters,
      labels = sub("^cluster_", "Cluster ", clusters),
      name = "Ancestry component"
    ) +
    ggplot2::scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
    ggplot2::scale_x_continuous(breaks = x$order, labels = x$sample_label, expand = c(0,0)) +
    ggplot2::labs(
      title = plot_title,
      subtitle = subtitle, x = NULL, y = y_label
    ) +
    theme_publication(figure_base_size(cfg)) + ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5, size = axis_label_size),
      axis.ticks.x = ggplot2::element_line(colour = "grey40"),
      panel.border = ggplot2::element_rect(
        colour = "#4D4D4D", fill = NA, linewidth = 0.45
      ),
      strip.text = ggplot2::element_text(size = strip_text_size),
      # strip_text_size above already removes most of the risk, but an
      # estimated fit (character-width heuristic against an approximate
      # per-facet pixel budget) is not exact, and a genuinely 1-sample
      # facet can still be narrower than even its own shortest legible
      # size -- confirmed directly: a real single-sample "Ro2-3" facet
      # still clipped to "o2-" after the dynamic sizing above. strip.clip
      # (ggplot2 >= 3.5) is the actual fix for that residual case: letting
      # the label spill visibly into neighbouring space beats silently
      # losing characters, which is what clipping (the default) does.
      strip.clip = "off"
    )
  if (length(cluster_boundaries)) {
    p <- p + ggplot2::geom_vline(
      xintercept = cluster_boundaries, colour = "grey20",
      linewidth = 0.45
    )
  }
  if (identical(order_mode, "population")) {
    p <- p + ggplot2::facet_grid(~population, scales = "free_x", space = "free_x")
  }
  if (isTRUE(subtitle_is_warning)) {
    p <- p + ggplot2::theme(
      plot.subtitle = ggplot2::element_text(colour = "#B2182B", face = "bold")
    )
  }
  label_height <- min(8, max(nchar(sample_labels)) * 0.08)
  stem <- if (identical(order_mode, "data_driven")) {
    sprintf("14_%s_data_driven_K%d", prefix, k)
  } else {
    sprintf("14_%s_K%d", prefix, k)
  }
  save_plot(p, stem, dirs, cfg$output$figure_formats,
            plot_width, 6 + label_height, cfg$output$dpi)
}

plot_q_matrix_views <- function(q, k, cfg, dirs, prefix = "ADMIXTURE_Q",
                                title = NULL, subtitle = NULL,
                                subtitle_is_warning = FALSE,
                                sample_labels = NULL,
                                y_label = "Ancestry proportion") {
  common <- list(
    q = q, k = k, cfg = cfg, dirs = dirs, prefix = prefix,
    title = title, subtitle = subtitle,
    subtitle_is_warning = subtitle_is_warning,
    sample_labels = sample_labels, y_label = y_label
  )
  do.call(plot_q_matrix, c(common, list(order_mode = "population")))
  do.call(plot_q_matrix, c(common, list(order_mode = "data_driven")))
  invisible(NULL)
}
