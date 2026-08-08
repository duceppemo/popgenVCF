# Standard KING-robust relationship-degree thresholds (Manichaikul et al.
# 2010, Table 1), not invented. Real chr22 1000 Genomes data (2,504 samples)
# showed off-diagonal kinship well below -0.5 once LD-pruned biallelic SNPs
# spanning highly differentiated continental populations were used -- KING-
# robust is not bounded at -0.5 for arbitrarily divergent pairs, only the
# upper bound (0.5, self/duplicate) is a hard theoretical ceiling.
kinship_relationship_degree <- function(kinship) {
  data.table::fcase(
    !is.finite(kinship), NA_character_,
    kinship > 0.354, "duplicate/MZ twin",
    kinship > 0.177, "1st-degree",
    kinship > 0.0884, "2nd-degree",
    kinship > 0.0442, "3rd-degree",
    default = "unrelated"
  )
}

run_kinship <- function(gds, sample_ids, snp_ids, metadata, threads) {
  king <- SNPRelate::snpgdsIBDKING(
    gds,
    sample.id = sample_ids,
    snp.id = snp_ids,
    type = "KING-robust",
    autosome.only = FALSE,
    remove.monosnp = TRUE,
    maf = NaN,
    missing.rate = NaN,
    num.thread = threads,
    verbose = FALSE
  )
  original_ids <- as.character(king$sample.id)
  public_ids <- public_sample_ids(metadata, original_ids)
  kinship <- as.matrix(king$kinship)
  ibs0 <- as.matrix(king$IBS0)
  dimnames(kinship) <- dimnames(ibs0) <- list(public_ids, public_ids)

  pairs <- data.table::setDT(SNPRelate::snpgdsIBDSelection(king))
  pairs[, sample_1 := public_ids[match(ID1, original_ids)]]
  pairs[, sample_2 := public_ids[match(ID2, original_ids)]]
  pairs[, relationship_degree := kinship_relationship_degree(kinship)]
  if ("population" %in% names(metadata)) {
    pairs[, population_1 := metadata$population[match(ID1, metadata$sample)]]
    pairs[, population_2 := metadata$population[match(ID2, metadata$sample)]]
    pairs <- pairs[, .(sample_1, sample_2, population_1, population_2, IBS0, kinship, relationship_degree)]
  } else {
    pairs <- pairs[, .(sample_1, sample_2, IBS0, kinship, relationship_degree)]
  }
  data.table::setorder(pairs, -kinship)

  list(kinship = kinship, ibs0 = ibs0, pairs = pairs)
}

kinship_heatmap_plot <- function(kinship, base_size = 11, style = "accessibility-first") {
  kinship <- as.matrix(kinship)
  n <- nrow(kinship)
  if (n < 2L || ncol(kinship) != n) {
    stop("kinship heatmap requires a square kinship matrix with at least two samples", call. = FALSE)
  }
  sample_ids <- rownames(kinship)
  if (is.null(sample_ids)) sample_ids <- sprintf("sample_%d", seq_len(n))
  sample_ids <- as.character(sample_ids)
  if (anyNA(sample_ids) || any(!nzchar(sample_ids)) || anyDuplicated(sample_ids)) {
    stop("kinship heatmap sample names must be unique and non-empty", call. = FALSE)
  }
  if (!is.null(colnames(kinship)) && !identical(colnames(kinship), sample_ids)) {
    stop("kinship heatmap row and column sample names must be identical", call. = FALSE)
  }

  # Cluster on a kinship-derived pseudo-distance so related samples group
  # together, but display the real kinship coefficient as fill -- users read
  # actual kinship values off the heatmap, not the clustering distance.
  pseudo_distance <- max(kinship, na.rm = TRUE) - kinship
  # KING-robust can legitimately return NaN for a pair when the estimator's
  # denominator is zero (e.g. too few markers for a sample to have any
  # heterozygous call among the tested set) -- confirmed empirically against
  # the small bundled CI validation fixture. hclust()'s underlying Fortran
  # routine hard-crashes on non-finite distances, so undefined pairs are
  # substituted with the largest *observed* pseudo-distance for clustering
  # purposes only; the tile fill below still shows the real (NaN) kinship
  # value via the grayed-out na.value, so nothing is misrepresented as close.
  if (any(!is.finite(pseudo_distance))) {
    n_undefined <- sum(!is.finite(pseudo_distance[upper.tri(pseudo_distance)]))
    log_msg(
      n_undefined, " sample pair(s) had non-finite KING-robust kinship ",
      "(likely insufficient marker density); treated as maximally ",
      "dissimilar for heatmap clustering only",
      level = "WARNING"
    )
    fallback <- suppressWarnings(max(pseudo_distance[is.finite(pseudo_distance)]))
    if (!is.finite(fallback)) fallback <- 0
    pseudo_distance[!is.finite(pseudo_distance)] <- fallback
  }
  tree <- stats::hclust(stats::as.dist(pseudo_distance), method = "average")
  ordered_ids <- sample_ids[tree$order]
  ordered <- kinship[tree$order, tree$order, drop = FALSE]
  long <- data.table::as.data.table(as.table(ordered))
  data.table::setnames(long, c("sample_y", "sample_x", "kinship"))
  data.table::set(long, j = "x", value = match(as.character(long$sample_x), ordered_ids))
  data.table::set(long, j = "y", value = match(as.character(long$sample_y), ordered_ids))

  dendrogram <- hclust_dendrogram_segments(tree)
  maximum_height <- max(tree$height)
  dendrogram_width <- max(1.5, n * 0.20)
  if (is.finite(maximum_height) && maximum_height > 0) {
    data.table::set(dendrogram, j = "x", value = -dendrogram$height / maximum_height * dendrogram_width)
    data.table::set(dendrogram, j = "xend", value = -dendrogram$height_end / maximum_height * dendrogram_width)
  } else {
    data.table::set(dendrogram, j = "x", value = 0)
    data.table::set(dendrogram, j = "xend", value = 0)
  }

  fill_scale <- if (identical(style, "grayscale-safe")) {
    ggplot2::scale_fill_gradient2(
      low = "white", mid = "#BDBDBD", high = "#1A1A1A",
      midpoint = 0, na.value = "#F2F2F2", name = "Kinship"
    )
  } else {
    ggplot2::scale_fill_gradient2(
      low = "#3B4CC0", mid = "#F7F7F7", high = "#B40426",
      midpoint = 0, na.value = "#F2F2F2", name = "Kinship"
    )
  }

  ggplot2::ggplot(
    long,
    ggplot2::aes(x = .data$x, y = .data$y, fill = .data$kinship)
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
      title = "Pairwise kinship (KING-robust)",
      subtitle = "Average-linkage dendrogram on a kinship-derived pseudo-distance; tile colour is the kinship coefficient",
      x = NULL, y = NULL
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

kinship_relationship_degree_levels <- function() {
  c("duplicate/MZ twin", "1st-degree", "2nd-degree", "3rd-degree", "unrelated")
}

kinship_relationship_degree_palette <- function(profile) {
  levels <- kinship_relationship_degree_levels()
  stats::setNames(expand_figure_palette(profile, length(levels), "colours"), levels)
}

plot_kinship <- function(result, cfg, dirs) {
  fmts <- cfg$output$figure_formats; dpi <- cfg$output$dpi
  style <- figure_style_name(cfg)
  profile <- figure_style_profile(style)
  n <- nrow(result$kinship)
  if (n <= 300L) {
    p1 <- kinship_heatmap_plot(result$kinship, figure_base_size(cfg), style)
    heatmap_size <- max(8, n * 0.12)
    save_plot(p1, "21_kinship_heatmap", dirs, fmts, heatmap_size * 1.2, heatmap_size, dpi)
  }
  pairs <- result$pairs
  if (nrow(pairs)) {
    levels <- kinship_relationship_degree_levels()
    pairs <- data.table::copy(pairs)
    pairs[, relationship_degree := factor(relationship_degree, levels = levels)]
    palette <- kinship_relationship_degree_palette(profile)
    p2 <- ggplot2::ggplot(pairs, ggplot2::aes(IBS0, kinship, colour = relationship_degree)) +
      ggplot2::geom_hline(
        yintercept = c(0.0442, 0.0884, 0.177, 0.354),
        colour = "#D9D9D9", linewidth = 0.3, linetype = "dashed"
      ) +
      ggplot2::geom_point(size = 1.4, alpha = .65, na.rm = TRUE) +
      ggplot2::scale_colour_manual(values = palette, drop = FALSE) +
      ggplot2::labs(
        title = "KING-robust relatedness diagnostic",
        subtitle = "Dashed lines: Manichaikul et al. 2010 relationship-degree thresholds",
        x = "IBS0 (proportion of loci with zero shared alleles)",
        y = "Kinship coefficient", colour = "Relationship"
      ) + theme_publication(figure_base_size(cfg))
    save_plot(p2, "22_kinship_IBS0_vs_kinship", dirs, fmts, 8, 6, dpi)
  }
}
