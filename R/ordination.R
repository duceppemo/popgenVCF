pca_component_count <- function(n_pcs, sample_ids, snp_ids) {
  n_pcs <- as.integer(n_pcs)[1L]
  if (is.na(n_pcs) || n_pcs < 2L) {
    stop("n_pcs must request at least two PCA components", call. = FALSE)
  }
  n_samples <- length(sample_ids)
  n_snps <- length(snp_ids)
  available <- min(n_samples - 1L, n_snps)
  requested <- min(n_pcs, available)
  if (requested < 2L) {
    stop(
      sprintf(
        paste0(
          "PCA requires at least two estimable components ",
          "(retained samples=%d; retained SNPs=%d)"
        ),
        n_samples, n_snps
      ),
      call. = FALSE
    )
  }
  requested
}

pca_eigensystem_is_finite <- function(pca, requested_components) {
  if (is.null(pca$eigenval) || is.null(pca$eigenvect)) return(FALSE)
  values <- as.numeric(pca$eigenval)
  vectors <- as.matrix(pca$eigenvect)
  if (length(values) < requested_components || ncol(vectors) < requested_components) {
    return(FALSE)
  }
  if (nrow(vectors) != length(pca$sample.id)) return(FALSE)
  index <- seq_len(requested_components)
  all(is.finite(values[index])) &&
    all(is.finite(vectors[, index, drop = FALSE]))
}

recover_pca_eigensystem <- function(pca, requested_components) {
  if (is.null(pca$genmat)) {
    stop(
      "SNPRelate PCA eigensystem was non-finite and no genetic covariance matrix was returned",
      call. = FALSE
    )
  }
  covariance <- as.matrix(pca$genmat)
  if (nrow(covariance) != ncol(covariance) ||
      nrow(covariance) != length(pca$sample.id)) {
    stop(
      "SNPRelate PCA returned an invalid genetic covariance matrix",
      call. = FALSE
    )
  }
  if (any(!is.finite(covariance))) {
    stop(
      sprintf(
        "SNPRelate PCA genetic covariance matrix contains %d non-finite value(s)",
        sum(!is.finite(covariance))
      ),
      call. = FALSE
    )
  }

  covariance <- (covariance + t(covariance)) / 2
  decomposition <- eigen(covariance, symmetric = TRUE)
  component_count <- min(
    as.integer(requested_components),
    length(decomposition$values),
    ncol(decomposition$vectors)
  )
  if (component_count < 2L) {
    stop(
      "PCA covariance fallback produced fewer than two components",
      call. = FALSE
    )
  }

  index <- seq_len(component_count)
  positive_total <- sum(pmax(decomposition$values, 0))
  pca$eigenval <- decomposition$values[index]
  pca$eigenvect <- decomposition$vectors[, index, drop = FALSE]
  pca$varprop <- if (is.finite(positive_total) && positive_total > 0) {
    pca$eigenval / positive_total
  } else {
    rep(NaN, component_count)
  }
  pca
}

pca_loading_table <- function(loading, ids) {
  # SNPRelate::snpgdsPCASNPLoading() can drop loci (e.g. remove.monosnp), so
  # loading$snp.id is the authoritative retained set -- join by real ID
  # against the full chromosome/position lookup, not by position.
  idx <- match(loading$snp.id, ids$snp)
  contr <- t(as.matrix(loading$snploading))
  colnames(contr) <- paste0("PC", seq_len(ncol(contr)))
  out <- data.table::rbindlist(lapply(colnames(contr), function(axis) {
    data.table::data.table(
      axis = axis, snp_id = as.character(loading$snp.id),
      chromosome = ids$chromosome[idx], position = ids$position[idx],
      contribution = as.numeric(contr[, axis])
    )
  }))
  out[, magnitude := abs(contribution)]
  data.table::setorder(out, axis, -magnitude)
  out
}

run_pca <- function(gds, sample_ids, snp_ids, metadata, n_pcs, threads, ids = NULL) {
  requested_components <- pca_component_count(n_pcs, sample_ids, snp_ids)
  run_snprelate <- function(need_genmat = FALSE) {
    SNPRelate::snpgdsPCA(
      gds,
      sample.id = sample_ids,
      snp.id = snp_ids,
      autosome.only = FALSE,
      remove.monosnp = TRUE,
      maf = NaN,
      missing.rate = NaN,
      eigen.cnt = requested_components,
      num.thread = threads,
      need.genmat = need_genmat,
      verbose = FALSE
    )
  }

  log_msg(
    "PCA inputs: ", length(sample_ids), " retained sample(s), ",
    length(snp_ids), " retained SNP(s), ", requested_components,
    " requested component(s)",
    level = "INFO"
  )
  z <- run_snprelate(FALSE)
  eigensystem_source <- "SNPRelate"
  raw_nonfinite_eigenvalues <- if (is.null(z$eigenval)) {
    requested_components
  } else {
    sum(!is.finite(as.numeric(z$eigenval)))
  }

  if (!pca_eigensystem_is_finite(z, requested_components)) {
    log_msg(
      "SNPRelate returned an incomplete or non-finite PCA eigensystem ",
      "(", raw_nonfinite_eigenvalues, " non-finite eigenvalue(s)); ",
      "recovering from the genetic covariance matrix",
      level = "WARNING"
    )
    z <- recover_pca_eigensystem(run_snprelate(TRUE), requested_components)
    eigensystem_source <- "covariance_eigendecomposition"
  }

  # SNP loadings require a genuine snpgdsPCAClass object; the covariance
  # fallback above patches eigenval/eigenvect from a manual eigendecomposition
  # of the genetic relationship matrix, which snpgdsPCASNPLoading() cannot
  # meaningfully interpret -- loadings are honestly NULL in that rare path.
  loadings <- if (!is.null(ids) && identical(eigensystem_source, "SNPRelate")) {
    loading <- SNPRelate::snpgdsPCASNPLoading(z, gds, num.thread = threads, verbose = FALSE)
    pca_loading_table(loading, ids)
  } else {
    NULL
  }

  eig <- normalize_pca_eigenvalues(z$eigenval)
  if (eig$adjusted_negative > 0L) {
    log_msg(
      "Clamped ", eig$adjusted_negative,
      " negligible negative PCA eigenvalue(s) to zero (tolerance=",
      signif(eig$tolerance, 6), ")",
      level = "WARNING"
    )
  }

  available_components <- which(
    seq_along(eig$values) <= ncol(z$eigenvect) & eig$values > 0
  )
  npc <- min(requested_components, length(available_components))
  if (npc < 2L) {
    stop(
      sprintf(
        paste0(
          "PCA produced only %d positive-variance component(s) after %s; ",
          "at least two are required"
        ),
        npc, eigensystem_source
      ),
      call. = FALSE
    )
  }
  component_index <- available_components[seq_len(npc)]
  if (!is.null(loadings)) {
    keep <- paste0("PC", component_index)
    loadings <- loadings[loadings$axis %in% keep, ]
    loadings[, axis := paste0("PC", match(axis, keep))]
    loadings[, .axis_sort_key := natural_sort_key(axis)]
    data.table::setorder(loadings, .axis_sort_key, -magnitude)
    loadings[, .axis_sort_key := NULL]
  }
  # Percent variance explained must come from the true total variance, not
  # from re-normalizing against only the requested/retained eigenvalues --
  # SNPRelate's own z$varprop is already computed against the true total
  # trace regardless of eigen.cnt (verified against live SNPRelate output:
  # eig$values / sum(eig$values) inflates every percentage by roughly
  # requested_components / (n_samples - 1)). The covariance-eigendecomposition
  # fallback in recover_pca_eigensystem() already sets varprop correctly the
  # same way (dividing its truncated eigenval subset by the *full* spectrum's
  # positive total), so this is correct for both eigensystem_source values.
  variance_proportion <- as.numeric(z$varprop)
  if (length(variance_proportion) < max(component_index)) {
    stop(
      "SNPRelate PCA did not return enough varprop entries for the retained components",
      call. = FALSE
    )
  }

  public_ids <- public_sample_ids(metadata, z$sample.id)
  scores <- data.table::data.table(sample = public_ids, vcf_sample = z$sample.id)
  for (i in seq_len(npc)) {
    scores[[paste0("PC", i)]] <- z$eigenvect[, component_index[[i]]]
  }
  if ("population" %in% names(metadata)) {
    data.table::set(scores, j = "population",
                    value = metadata$population[match(scores$vcf_sample, metadata$sample)])
  }
  variance <- data.table::data.table(
    PC = paste0("PC", seq_len(npc)),
    proportion = variance_proportion[component_index],
    percent = 100 * variance_proportion[component_index]
  )
  list(
    scores = scores,
    variance = variance,
    object = z,
    eigenvalues = eig$values,
    eigenvalue_tolerance = eig$tolerance,
    eigensystem_source = eigensystem_source,
    raw_nonfinite_eigenvalues = raw_nonfinite_eigenvalues,
    requested_components = requested_components,
    loadings = loadings
  )
}

plot_pca_loading_manhattan <- function(loadings, cfg, dirs, profile) {
  layout <- manhattan_layout(loadings$chromosome, loadings$position)
  bp_breaks <- manhattan_bp_breaks(loadings$chromosome, loadings$position, layout$offset)
  loadings <- data.table::copy(loadings)
  loadings[, x := layout$x]
  loadings[, chrom_group := factor(match(chromosome, layout$ticks$chromosome) %% 2L)]
  loadings[, axis := factor(axis, levels = natural_sort_levels(axis))]
  colours <- expand_figure_palette(profile, 2L, "colours")
  p <- ggplot2::ggplot(loadings, ggplot2::aes(x = x, y = contribution, colour = chrom_group)) +
    ggplot2::geom_hline(yintercept = 0, colour = "#D9D9D9", linewidth = 0.35) +
    ggplot2::geom_point(size = 1, alpha = .75, show.legend = FALSE) +
    ggplot2::scale_colour_manual(values = colours) +
    ggplot2::scale_x_continuous(breaks = bp_breaks$x, labels = bp_breaks$label) +
    ggplot2::facet_wrap(~axis, ncol = 1, scales = "free_y") +
    ggplot2::labs(
      title = "Principal component analysis SNP loadings",
      x = "Chromosome position", y = "SNP loading (correlation with component)"
    ) + theme_publication(figure_base_size(cfg)) +
    ggplot2::theme(
      panel.spacing = ggplot2::unit(1, "lines"),
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1)
    )
  n_axes <- data.table::uniqueN(loadings$axis)
  save_plot(
    p, "17_PCA_loadings_manhattan", dirs,
    cfg$output$figure_formats, 10, max(4, 2.2 * n_axes), cfg$output$dpi
  )
  invisible(p)
}

plot_pca_loading_ranked <- function(loadings, cfg, dirs, profile) {
  ranked <- data.table::copy(loadings)
  ranked[, axis := factor(axis, levels = natural_sort_levels(axis))]
  data.table::setorder(ranked, axis, -magnitude)
  ranked[, rank := seq_len(.N), by = axis]
  colour <- expand_figure_palette(profile, 1L, "colours")
  p <- ggplot2::ggplot(ranked, ggplot2::aes(x = rank, y = contribution)) +
    ggplot2::geom_hline(yintercept = 0, colour = "#D9D9D9", linewidth = 0.35) +
    ggplot2::geom_point(size = 1, alpha = .75, colour = colour) +
    ggplot2::facet_wrap(~axis, ncol = 1, scales = "free_y") +
    ggplot2::labs(
      title = "Principal component analysis SNP loadings, ranked",
      x = "SNP rank (descending |loading|)", y = "SNP loading (correlation with component)"
    ) + theme_publication(figure_base_size(cfg)) +
    ggplot2::theme(panel.spacing = ggplot2::unit(1, "lines"))
  n_axes <- data.table::uniqueN(ranked$axis)
  save_plot(
    p, "18_PCA_loadings_ranked", dirs,
    cfg$output$figure_formats, 8, max(4, 2.2 * n_axes), cfg$output$dpi
  )
  invisible(p)
}

#' Metadata columns eligible for per-column PCA colouring
#'
#' A column qualifies when, after dropping missing values for samples present
#' in `sample_ids`, it has at least two distinct values, no more than
#' `max_levels` distinct values (keeping legends readable), and every
#' remaining value occurs at least `min_group` times (colouring by a level
#' with one or two samples is not a meaningful group comparison).
#' @keywords internal
pca_metadata_colour_columns <- function(metadata, sample_ids, min_group = 3L, max_levels = 12L) {
  excluded <- c("sample", "vcf_sample", "population", "alias", "latitude", "longitude")
  candidates <- setdiff(names(metadata), excluded)
  metadata <- metadata[match(sample_ids, metadata$sample)]
  keep <- character()
  for (col in candidates) {
    values <- trimws(as.character(metadata[[col]]))
    values <- values[!is.na(values) & nzchar(values)]
    if (!length(values)) next
    counts <- table(values)
    counts <- counts[counts >= min_group]
    if (length(counts) < 2L || length(counts) > max_levels) next
    keep <- c(keep, col)
  }
  keep
}

pca_metadata_display_name <- function(column) {
  words <- strsplit(gsub("_", " ", column, fixed = TRUE), " ", fixed = TRUE)[[1]]
  words <- words[nzchar(words)]
  paste(toupper(substring(words, 1L, 1L)), substring(words, 2L), sep = "", collapse = " ")
}

plot_pca_by_metadata <- function(pca, metadata, column, cfg, dirs, style) {
  fmts <- cfg$output$figure_formats; dpi <- cfg$output$dpi
  min_group <- cfg$analyses$pca_metadata_color_min_group
  scores <- data.table::copy(pca$scores)
  values <- trimws(as.character(metadata[[column]][match(scores$vcf_sample, metadata$sample)]))
  values[!nzchar(values)] <- NA_character_
  counts <- table(values[!is.na(values)])
  scores[["colour_group"]] <- ifelse(values %in% names(counts)[counts >= min_group], values, NA_character_)
  scores <- scores[!is.na(scores$colour_group), ]
  if (data.table::uniqueN(scores$colour_group) < 2L) return(invisible(NULL))
  pal <- population_palette(scores$colour_group, style)
  shapes <- population_shapes(scores$colour_group, style)
  label_name <- pca_metadata_display_name(column)
  x <- "PC1"; y <- "PC2"
  p <- ggplot2::ggplot(
    scores,
    ggplot2::aes(x = .data[[x]], y = .data[[y]], colour = colour_group, shape = colour_group)
  ) +
    ggplot2::geom_hline(yintercept = 0, colour = "#D9D9D9", linewidth = 0.35) +
    ggplot2::geom_vline(xintercept = 0, colour = "#D9D9D9", linewidth = 0.35) +
    ggplot2::geom_point(size = 3, alpha = .9, stroke = 0.55) +
    ggplot2::scale_colour_manual(values = pal) +
    ggplot2::scale_shape_manual(values = shapes) +
    ggplot2::labs(
      title = sprintf("Principal component analysis, coloured by %s", tolower(label_name)),
      x = sprintf("%s (%.2f%%)", x, pca$variance$percent[1]),
      y = sprintf("%s (%.2f%%)", y, pca$variance$percent[2]),
      colour = label_name, shape = label_name
    ) + theme_publication(figure_base_size(cfg))
  save_plot(p, sprintf("07b_PCA_PC1_PC2_by_%s", column), dirs, fmts, 8, 6, dpi)
  invisible(p)
}

plot_pca <- function(pca, cfg, dirs, metadata = NULL) {
  fmts <- cfg$output$figure_formats; dpi <- cfg$output$dpi
  label <- cfg$output$label_samples
  do_label <- identical(label, "all") || (identical(label, "auto") && nrow(pca$scores) <= 60L)
  has_population <- "population" %in% names(pca$scores) && any(!is.na(pca$scores$population))
  style <- figure_style_name(cfg)
  profile <- figure_style_profile(style)
  pal <- if (has_population) {
    population_palette(pca$scores$population, style)
  } else NULL
  shapes <- if (has_population) {
    population_shapes(pca$scores$population, style)
  } else NULL
  for (pair in list(c(1, 2), c(1, 3), c(2, 3))) {
    if (max(pair) > nrow(pca$variance)) next
    x <- paste0("PC", pair[1]); y <- paste0("PC", pair[2])
    mapping <- if (has_population) {
      ggplot2::aes(
        x = .data[[x]], y = .data[[y]],
        colour = population, shape = population
      )
    } else {
      ggplot2::aes(x = .data[[x]], y = .data[[y]])
    }
    p <- ggplot2::ggplot(pca$scores, mapping) +
      ggplot2::geom_hline(
        yintercept = 0, colour = "#D9D9D9", linewidth = 0.35
      ) +
      ggplot2::geom_vline(
        xintercept = 0, colour = "#D9D9D9", linewidth = 0.35
      ) +
      ggplot2::geom_point(size = 3, alpha = .9, stroke = 0.55) +
      ggplot2::labs(
        title = "Principal component analysis",
        x = sprintf("%s (%.2f%%)", x, pca$variance$percent[pair[1]]),
        y = sprintf("%s (%.2f%%)", y, pca$variance$percent[pair[2]]),
        colour = "Population", shape = "Population"
      ) + theme_publication(figure_base_size(cfg))
    if (has_population) {
      p <- p +
        ggplot2::scale_colour_manual(values = pal) +
        ggplot2::scale_shape_manual(values = shapes)
    }
    if (do_label) {
      p <- p + ggrepel::geom_text_repel(
        ggplot2::aes(label = sample), size = 2.5,
        max.overlaps = 30, show.legend = FALSE
      )
    }
    save_plot(p, sprintf("07_PCA_PC%d_PC%d", pair[1], pair[2]), dirs, fmts, 8, 6, dpi)
  }
  if (!is.null(pca$loadings) && nrow(pca$loadings)) {
    plot_pca_loading_manhattan(pca$loadings, cfg, dirs, profile)
    plot_pca_loading_ranked(pca$loadings, cfg, dirs, profile)
  }
  if (isTRUE(cfg$analyses$pca_metadata_color) && !is.null(metadata) &&
      "vcf_sample" %in% names(pca$scores) && nrow(pca$variance) >= 2L) {
    colour_columns <- pca_metadata_colour_columns(
      metadata, pca$scores$vcf_sample,
      cfg$analyses$pca_metadata_color_min_group, cfg$analyses$pca_metadata_color_max_levels
    )
    for (column in colour_columns) {
      plot_pca_by_metadata(pca, metadata, column, cfg, dirs, style)
    }
  }
}

run_ibs <- function(gds, sample_ids, snp_ids, metadata, threads) {
  z <- SNPRelate::snpgdsIBS(
    gds,
    sample.id = sample_ids,
    snp.id = snp_ids,
    autosome.only = FALSE,
    remove.monosnp = TRUE,
    maf = NaN,
    missing.rate = NaN,
    num.thread = threads,
    verbose = FALSE
  )
  sim <- as.matrix(z$ibs)
  original_ids <- as.character(z$sample.id)
  public_ids <- public_sample_ids(metadata, original_ids)
  rownames(sim) <- colnames(sim) <- public_ids
  dist <- 1 - sim
  m <- stats::cmdscale(stats::as.dist(dist), k = min(2L, nrow(dist) - 1L), eig = TRUE)
  points <- data.table::data.table(
    sample = rownames(m$points),
    vcf_sample = original_ids[match(rownames(m$points), public_ids)],
    MDS1 = m$points[, 1],
    MDS2 = if (ncol(m$points) > 1L) m$points[, 2] else 0
  )
  if ("population" %in% names(metadata)) {
    data.table::set(points, j = "population",
                    value = metadata$population[match(points$vcf_sample, metadata$sample)])
  }
  list(similarity = sim, distance = dist, mds = points, eig = m$eig)
}

hclust_dendrogram_segments <- function(tree) {
  n <- length(tree$order)
  leaf_y <- integer(n)
  leaf_y[tree$order] <- seq_len(n)
  node_y <- numeric(n - 1L)
  segments <- vector("list", 3L * (n - 1L))
  segment_index <- 0L

  child_coordinates <- function(code) {
    if (code < 0L) {
      list(height = 0, y = leaf_y[-code])
    } else {
      list(height = tree$height[code], y = node_y[code])
    }
  }

  for (i in seq_len(n - 1L)) {
    left <- child_coordinates(tree$merge[i, 1L])
    right <- child_coordinates(tree$merge[i, 2L])
    parent_height <- tree$height[i]
    node_y[i] <- mean(c(left$y, right$y))
    pieces <- list(
      c(left$height, parent_height, left$y, left$y),
      c(right$height, parent_height, right$y, right$y),
      c(parent_height, parent_height, left$y, right$y)
    )
    for (piece in pieces) {
      segment_index <- segment_index + 1L
      segments[[segment_index]] <- piece
    }
  }

  out <- data.table::as.data.table(do.call(rbind, segments))
  data.table::setnames(out, c("height", "height_end", "y", "yend"))
  out
}

ibs_heatmap_plot <- function(
    distance, base_size = 11, style = "accessibility-first") {
  distance <- as.matrix(distance)
  n <- nrow(distance)
  if (n < 2L || ncol(distance) != n) {
    stop("IBS heatmap requires a square distance matrix with at least two samples", call. = FALSE)
  }
  sample_ids <- rownames(distance)
  if (is.null(sample_ids)) sample_ids <- sprintf("sample_%d", seq_len(n))
  sample_ids <- as.character(sample_ids)
  if (anyNA(sample_ids) || any(!nzchar(sample_ids)) || anyDuplicated(sample_ids)) {
    stop("IBS heatmap sample names must be unique and non-empty", call. = FALSE)
  }
  if (!is.null(colnames(distance)) && !identical(colnames(distance), sample_ids)) {
    stop("IBS heatmap row and column sample names must be identical", call. = FALSE)
  }

  tree <- stats::hclust(stats::as.dist(distance), method = "average")
  ordered_ids <- sample_ids[tree$order]
  ordered <- distance[tree$order, tree$order, drop = FALSE]
  long <- data.table::as.data.table(as.table(ordered))
  data.table::setnames(long, c("sample_y", "sample_x", "distance"))
  data.table::set(
    long, j = "x", value = match(as.character(long$sample_x), ordered_ids)
  )
  data.table::set(
    long, j = "y", value = match(as.character(long$sample_y), ordered_ids)
  )

  dendrogram <- hclust_dendrogram_segments(tree)
  maximum_height <- max(tree$height)
  dendrogram_width <- max(1.5, n * 0.20)
  if (is.finite(maximum_height) && maximum_height > 0) {
    data.table::set(
      dendrogram, j = "x",
      value = -dendrogram$height / maximum_height * dendrogram_width
    )
    data.table::set(
      dendrogram, j = "xend",
      value = -dendrogram$height_end / maximum_height * dendrogram_width
    )
  } else {
    data.table::set(dendrogram, j = "x", value = 0)
    data.table::set(dendrogram, j = "xend", value = 0)
  }

  fill_scale <- if (identical(style, "grayscale-safe")) {
    ggplot2::scale_fill_gradient(
      low = "white", high = "#1A1A1A", na.value = "#F2F2F2"
    )
  } else {
    ggplot2::scale_fill_viridis_c(
      option = "C", begin = 0.05, end = 0.95, na.value = "#F2F2F2"
    )
  }

  ggplot2::ggplot(
    long,
    ggplot2::aes(x = .data$x, y = .data$y, fill = .data$distance)
  ) +
    ggplot2::geom_tile(width = 1, height = 1) +
    ggplot2::geom_segment(
      data = dendrogram,
      ggplot2::aes(
        x = .data$x, xend = .data$xend,
        y = .data$y, yend = .data$yend
      ),
      inherit.aes = FALSE, colour = "grey20", linewidth = 0.3,
      lineend = "square"
    ) +
    fill_scale +
    ggplot2::scale_x_continuous(
      breaks = seq_len(n), labels = ordered_ids, expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq_len(n), labels = ordered_ids, expand = c(0, 0),
      position = "right"
    ) +
    ggplot2::coord_fixed(
      xlim = c(-dendrogram_width - 0.5, n + 0.5),
      ylim = c(0.5, n + 0.5), clip = "off"
    ) +
    ggplot2::labs(
      title = "Pairwise identity-by-state distance",
      subtitle = "Average-linkage dendrogram aligned to heatmap rows",
      x = NULL, y = NULL, fill = "1 - IBS"
    ) +
    theme_publication(base_size) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 90, hjust = 1, vjust = 0.5,
        size = if (n <= 60L) 7 else 5
      ),
      axis.text.y = ggplot2::element_text(size = if (n <= 60L) 7 else 5),
      axis.ticks = ggplot2::element_line(colour = "grey40"),
      panel.border = ggplot2::element_rect(colour = "grey50", fill = NA)
    )
}

plot_ibs <- function(ibs, cfg, dirs) {
  fmts <- cfg$output$figure_formats; dpi <- cfg$output$dpi
  has_population <- "population" %in% names(ibs$mds) && any(!is.na(ibs$mds$population))
  style <- figure_style_name(cfg)
  mapping <- if (has_population) {
    ggplot2::aes(MDS1, MDS2, colour = population, shape = population)
  } else {
    ggplot2::aes(MDS1, MDS2)
  }
  p <- ggplot2::ggplot(ibs$mds, mapping) +
    ggplot2::geom_hline(
      yintercept = 0, colour = "#D9D9D9", linewidth = 0.35
    ) +
    ggplot2::geom_vline(
      xintercept = 0, colour = "#D9D9D9", linewidth = 0.35
    ) +
    ggplot2::geom_point(size = 3, alpha = .9, stroke = 0.55) +
    ggplot2::labs(
      title = "Multidimensional scaling of identity-by-state distance",
      x = "MDS 1", y = "MDS 2",
      colour = "Population", shape = "Population"
    ) +
    theme_publication(figure_base_size(cfg))
  if (has_population) {
    p <- p +
      ggplot2::scale_colour_manual(
        values = population_palette(ibs$mds$population, style)
      ) +
      ggplot2::scale_shape_manual(
        values = population_shapes(ibs$mds$population, style)
      )
  }
  save_plot(p, "08_IBS_MDS", dirs, fmts, 8, 6, dpi)
  n <- nrow(ibs$distance)
  if (n <= 300L) {
    p2 <- ibs_heatmap_plot(
      ibs$distance, figure_base_size(cfg), style
    )
    heatmap_size <- max(8, n * 0.12)
    save_plot(p2, "09_IBS_heatmap", dirs, fmts,
              heatmap_size * 1.2, heatmap_size, dpi)
  }
}

build_nj_tree <- function(ibs, metadata, cfg, dirs) {
  tree <- ape::nj(stats::as.dist(ibs$distance))
  ape::write.tree(tree, file.path(dirs$trees, "IBS_neighbor_joining.nwk"))
  tree
}
