normalize_pca_eigenvalues <- function(eigenvalues,
                                      relative_tolerance = sqrt(.Machine$double.eps)) {
  eigenvalues <- as.numeric(eigenvalues)
  if (!length(eigenvalues)) {
    stop("SNPRelate PCA returned no eigenvalues", call. = FALSE)
  }
  if (any(!is.finite(eigenvalues))) {
    stop(
      sprintf(
        "SNPRelate PCA returned %d non-finite eigenvalue(s)",
        sum(!is.finite(eigenvalues))
      ),
      call. = FALSE
    )
  }

  relative_tolerance <- as.numeric(relative_tolerance)[1L]
  if (!is.finite(relative_tolerance) || relative_tolerance < 0) {
    stop("relative_tolerance must be one finite nonnegative value", call. = FALSE)
  }
  scale <- max(abs(eigenvalues))
  tolerance <- max(
    .Machine$double.eps * max(1, scale),
    relative_tolerance * scale
  )
  materially_negative <- eigenvalues < -tolerance
  if (any(materially_negative)) {
    stop(
      sprintf(
        paste0(
          "SNPRelate PCA returned materially negative eigenvalues ",
          "(minimum=%.6g; tolerance=%.6g)"
        ),
        min(eigenvalues), tolerance
      ),
      call. = FALSE
    )
  }

  adjusted <- eigenvalues
  adjusted[abs(adjusted) <= tolerance] <- 0
  if (sum(adjusted) <= 0) {
    stop("SNPRelate PCA retained no positive genetic variance", call. = FALSE)
  }

  list(
    values = adjusted,
    adjusted_negative = sum(eigenvalues < 0),
    tolerance = tolerance
  )
}

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

run_pca <- function(gds, sample_ids, snp_ids, metadata, n_pcs, threads) {
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
  variance_proportion <- eig$values / sum(eig$values)

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
    requested_components = requested_components
  )
}

plot_pca <- function(pca, cfg, dirs) {
  fmts <- cfg$output$figure_formats; dpi <- cfg$output$dpi
  label <- cfg$output$label_samples
  do_label <- identical(label, "all") || (identical(label, "auto") && nrow(pca$scores) <= 60L)
  has_population <- "population" %in% names(pca$scores) && any(!is.na(pca$scores$population))
  pal <- if (has_population) population_palette(pca$scores$population) else NULL
  for (pair in list(c(1, 2), c(1, 3), c(2, 3))) {
    if (max(pair) > nrow(pca$variance)) next
    x <- paste0("PC", pair[1]); y <- paste0("PC", pair[2])
    mapping <- if (has_population) {
      ggplot2::aes(x = .data[[x]], y = .data[[y]], colour = population)
    } else {
      ggplot2::aes(x = .data[[x]], y = .data[[y]])
    }
    p <- ggplot2::ggplot(pca$scores, mapping) +
      ggplot2::geom_point(size = 2.7, alpha = .85) +
      ggplot2::labs(
        title = "Principal component analysis",
        x = sprintf("%s (%.2f%%)", x, pca$variance$percent[pair[1]]),
        y = sprintf("%s (%.2f%%)", y, pca$variance$percent[pair[2]]),
        colour = "Population"
      ) + theme_publication()
    if (has_population) p <- p + ggplot2::scale_colour_manual(values = pal)
    if (do_label) {
      p <- p + ggrepel::geom_text_repel(
        ggplot2::aes(label = sample), size = 2.5,
        max.overlaps = 30, show.legend = FALSE
      )
    }
    save_plot(p, sprintf("07_PCA_PC%d_PC%d", pair[1], pair[2]), dirs, fmts, 8, 6, dpi)
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

plot_ibs <- function(ibs, cfg, dirs) {
  fmts <- cfg$output$figure_formats; dpi <- cfg$output$dpi
  has_population <- "population" %in% names(ibs$mds) && any(!is.na(ibs$mds$population))
  mapping <- if (has_population) {
    ggplot2::aes(MDS1, MDS2, colour = population)
  } else {
    ggplot2::aes(MDS1, MDS2)
  }
  p <- ggplot2::ggplot(ibs$mds, mapping) +
    ggplot2::geom_point(size = 2.7) +
    ggplot2::labs(title = "MDS of IBS distance", colour = "Population") +
    theme_publication()
  if (has_population) {
    p <- p + ggplot2::scale_colour_manual(values = population_palette(ibs$mds$population))
  }
  save_plot(p, "08_IBS_MDS", dirs, fmts, 8, 6, dpi)
  n <- nrow(ibs$distance)
  if (n <= 300L) {
    ord <- stats::hclust(stats::as.dist(ibs$distance), method = "average")$order
    long <- data.table::as.data.table(as.table(ibs$distance[ord, ord, drop = FALSE]))
    data.table::setnames(long, c("sample_y", "sample_x", "distance"))
    p2 <- ggplot2::ggplot(long, ggplot2::aes(sample_x, sample_y, fill = distance)) +
      ggplot2::geom_raster() + ggplot2::scale_fill_viridis_c() +
      ggplot2::labs(title = "Pairwise IBS distance", x = NULL, y = NULL, fill = "1 - IBS") +
      theme_publication() +
      ggplot2::theme(axis.text = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank())
    save_plot(p2, "09_IBS_heatmap", dirs, fmts, 8, 8, dpi)
  }
}

build_nj_tree <- function(ibs, metadata, cfg, dirs) {
  tree <- ape::nj(stats::as.dist(ibs$distance))
  ape::write.tree(tree, file.path(dirs$trees, "IBS_neighbor_joining.nwk"))
  tree
}
