pca_component_count <- function(n_pcs, sample_ids, snp_ids) {
  n_samples <- length(sample_ids)
  n_snps <- length(snp_ids)
  available <- min(n_samples - 1L, n_snps)
  if (identical(n_pcs, "auto")) {
    # The Tracy-Widom test (pca_significant_component_count() below) needs
    # the full eigenvalue spectrum to test against, not just a handful --
    # request the same generous cap run_dapc_analysis() already uses for
    # its own shared PCA (100, or fewer if the data can't support that
    # many), then run_pca() trims down to the significant count once the
    # eigenvalues are in hand.
    requested <- min(available, 100L)
  } else {
    n_pcs <- as.integer(n_pcs)[1L]
    if (is.na(n_pcs) || n_pcs < 2L) {
      stop("n_pcs must be \"auto\" or request at least two PCA components", call. = FALSE)
    }
    requested <- min(n_pcs, available)
  }
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

# How many leading PCA eigenvalues represent real structure rather than
# noise, via the sequential Tracy-Widom test from Patterson, Price & Reich
# (2006) "Population Structure and Eigenanalysis" -- the same test
# EIGENSOFT's smartpca uses for its own automatic PC-count selection.
# Reimplementing Patterson et al.'s effective-marker-count-adjusted
# normalization from scratch would risk a scientifically wrong result with
# no easy way to catch it; instead this calls the Bioconductor LEA
# package's own compiled Tracy-Widom routine directly (LEA is already an
# optional dependency here, for the LEA/sNMF ancestry backend), bypassing
# its pcaProject file-based wrapper (tied to LEA's own PCA workflow, not
# ours) -- confirmed directly against LEA's own tutorial dataset that this
# produces identical statistics to its own pca()+tracy.widom() workflow.
pca_tracy_widom_table <- function(eigenvalues) {
  if (!requireNamespace("LEA", quietly = TRUE)) {
    stop(
      "analyses.n_pcs = \"auto\" requires the LEA package (for its Tracy-Widom ",
      "eigenvalue significance test, Patterson, Price & Reich 2006); install it ",
      "or set analyses.n_pcs to a fixed integer instead",
      call. = FALSE
    )
  }
  eigenvalues <- as.numeric(eigenvalues)
  eigenvalues <- eigenvalues[is.finite(eigenvalues) & eigenvalues > 0]
  if (length(eigenvalues) < 2L) {
    stop("Tracy-Widom auto-selection needs at least two positive eigenvalues", call. = FALSE)
  }
  in_file <- tempfile(fileext = ".eigenvalues")
  out_file <- sub("[.]eigenvalues$", ".tracywidom", in_file)
  on.exit(unlink(c(in_file, out_file)), add = TRUE)
  writeLines(format(eigenvalues, scientific = FALSE, trim = TRUE), in_file)
  # LEA's own C routine writes a "summary of the options" banner straight to
  # stdout (not through an R connection) -- sink() still redirects it since
  # it shares the process's real stdout file descriptor, keeping pipeline
  # logs free of a third-party tool's own console banner.
  sink_con <- file(tempfile(), open = "wt")
  sink(sink_con, type = "output")
  status <- tryCatch({
    .C("R_tracyWidom", as.character(in_file), as.character(out_file), PACKAGE = "LEA")
    TRUE
  }, error = function(e) e)
  sink(type = "output")
  close(sink_con)
  if (!isTRUE(status)) {
    stop(sprintf("LEA's Tracy-Widom routine failed: %s", conditionMessage(status)), call. = FALSE)
  }
  if (!file.exists(out_file)) {
    stop("LEA's Tracy-Widom routine did not produce an output file", call. = FALSE)
  }
  # Columns: N (1-based component index), eigenvalues, twstats, pvalues,
  # effectn, percentage (fraction of total variance, 0-1, not 0-100).
  data.table::as.data.table(utils::read.table(out_file, header = TRUE))
}

# Sequential test: stop at the first eigenvalue whose p-value is no longer
# significant, matching smartpca's own "number of significant PCs"
# convention -- eigenvalue significance in this test is only meaningful
# from the top down (later p-values can look significant again by chance
# once the leading signal has already been exhausted; that is not a
# second, independent block of structure).
pca_tracy_widom_significant_count <- function(tw, alpha = 0.05) {
  first_nonsignificant <- which(tw$pvalues >= alpha)[1L]
  n_significant <- if (is.na(first_nonsignificant)) nrow(tw) else first_nonsignificant - 1L
  max(2L, n_significant)
}

pca_significant_component_count <- function(eigenvalues, alpha = 0.05) {
  tw <- pca_tracy_widom_table(eigenvalues)
  pca_tracy_widom_significant_count(tw, alpha)
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

run_pca <- function(gds, sample_ids, snp_ids, metadata, n_pcs, threads, ids = NULL,
                    always_tracy_widom = FALSE) {
  auto_pcs <- identical(n_pcs, "auto")
  requested_components <- pca_component_count(n_pcs, sample_ids, snp_ids)
  compute_tracy_widom <- auto_pcs || isTRUE(always_tracy_widom)
  # A fixed n_pcs only asks SNPRelate to compute exactly that many
  # eigenvalues -- fine for the retained scores/loadings, but too narrow a
  # spectrum to meaningfully judge where the real-structure/noise boundary
  # falls. When a Tracy-Widom comparison was requested (auto selection, or
  # a fixed n_pcs wanting the "how close is my choice to the data-driven
  # one" comparison figure), widen the eigendecomposition to the same
  # generous cap `pca_component_count()`'s own "auto" branch uses, without
  # changing how many components are actually retained for a fixed n_pcs.
  available <- min(length(sample_ids) - 1L, length(snp_ids))
  eigen_cnt <- if (compute_tracy_widom) max(requested_components, min(available, 100L)) else requested_components
  run_snprelate <- function(need_genmat = FALSE) {
    SNPRelate::snpgdsPCA(
      gds,
      sample.id = sample_ids,
      snp.id = snp_ids,
      autosome.only = FALSE,
      remove.monosnp = TRUE,
      maf = NaN,
      missing.rate = NaN,
      eigen.cnt = eigen_cnt,
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
    z <- recover_pca_eigensystem(run_snprelate(TRUE), eigen_cnt)
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
  tracy_widom <- NULL
  tracy_widom_significant <- NULL
  tracy_widom_alpha <- 0.05
  retain <- requested_components
  if (auto_pcs) {
    # Auto-selection cannot proceed without this -- let a missing/failed
    # Tracy-Widom test raise its own clear error rather than silently
    # falling back to some other component count.
    tracy_widom <- pca_tracy_widom_table(eig$values[available_components])
    tracy_widom_significant <- pca_tracy_widom_significant_count(tracy_widom, tracy_widom_alpha)
    log_msg(
      "PCA auto component selection (Tracy-Widom, Patterson/Price/Reich 2006): ",
      tracy_widom_significant, " of ", length(available_components),
      " computed component(s) significant",
      level = "INFO"
    )
    retain <- tracy_widom_significant
  } else if (isTRUE(always_tracy_widom)) {
    # Purely a comparison for the user's own benefit here -- a fixed n_pcs
    # is retained exactly as requested either way, so a missing LEA
    # installation or any other failure degrades to simply not drawing the
    # comparison figure, not to an error.
    tracy_widom <- tryCatch(
      pca_tracy_widom_table(eig$values[available_components]),
      error = function(e) {
        log_msg(
          "Skipping the Tracy-Widom significance comparison figure: ", conditionMessage(e),
          level = "WARNING"
        )
        NULL
      }
    )
    if (!is.null(tracy_widom)) {
      tracy_widom_significant <- pca_tracy_widom_significant_count(tracy_widom, tracy_widom_alpha)
      log_msg(
        "PCA component significance (Tracy-Widom, Patterson/Price/Reich 2006): ",
        tracy_widom_significant, " of ", length(available_components),
        " computed component(s) significant (", requested_components, " requested)",
        level = "INFO"
      )
    }
  }
  npc <- min(retain, length(available_components))
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
    retained_components = npc,
    loadings = loadings,
    tracy_widom = tracy_widom,
    tracy_widom_significant = tracy_widom_significant,
    tracy_widom_alpha = tracy_widom_alpha
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
  base_size <- figure_base_size(cfg)
  p <- ggplot2::ggplot(loadings, ggplot2::aes(x = x, y = contribution, colour = chrom_group)) +
    ggplot2::geom_hline(yintercept = 0, colour = "#D9D9D9", linewidth = 0.35) +
    ggplot2::geom_point(size = 1, alpha = .75, show.legend = FALSE) +
    ggplot2::scale_colour_manual(values = colours) +
    ggplot2::scale_x_continuous(breaks = bp_breaks$x, labels = bp_breaks$label) +
    ggplot2::facet_wrap(~axis, ncol = 1, scales = "free_y") +
    ggplot2::labs(
      title = "Principal component analysis SNP loadings",
      x = "Chromosome position", y = "SNP loading (correlation with component)"
    ) + theme_publication(base_size) +
    ggplot2::theme(panel.spacing = ggplot2::unit(1, "lines"))
  last_axis <- levels(loadings$axis)[length(levels(loadings$axis))]
  n_axes <- data.table::uniqueN(loadings$axis)
  # geom_hline(yintercept = 0) above silently expands this facet's own
  # rendered y-scale to include 0 whenever the real data doesn't naturally
  # span it (e.g. a low-variance PC whose loadings cluster tightly well
  # away from zero) -- ggplot2 trains scale limits on every layer's data,
  # not just the primary geom's. manhattan_chromosome_row()'s pad/margin
  # math is computed from this y_range directly, so it must match the
  # panel's true rendered range or the label ends up positioned inside the
  # now much taller panel instead of below it, truncated by the panel's
  # own clipping rather than bleeding into the margin as intended --
  # confirmed directly against a real production figure.
  last_axis_range <- range(
    c(loadings$contribution[loadings$axis == last_axis], 0), na.rm = TRUE
  )
  p <- manhattan_chromosome_row(
    p, layout$ticks, last_axis_range,
    base_size, facet_var = "axis", facet_last_level = last_axis, facet_levels = levels(loadings$axis),
    plot_width_in = 10
  )
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

# Scree-style Tracy-Widom significance plot -- percent of total variance vs.
# component index, styled after LEA's own tracy.widom() example plot, with
# every Tracy-Widom-significant component highlighted against the discarded
# tail. Produced whenever `pca$tracy_widom` is populated: always for
# `analyses.n_pcs = "auto"`, and best-effort for a fixed n_pcs too (see
# run_pca()'s `always_tracy_widom` argument) -- for a fixed n_pcs, `retained`
# (what was actually kept) and `significant` (Tracy-Widom's own
# recommendation) are independent numbers, so a second reference line marks
# `retained` whenever it differs, letting the user directly compare their
# chosen component count against the data-driven one.
plot_pca_tracy_widom <- function(tw, significant, alpha, retained, cfg, dirs, profile) {
  if (is.null(tw) || !nrow(tw)) return(invisible(NULL))
  df <- data.frame(
    index = tw$N,
    percent = 100 * tw$percentage,
    retained_significant = tw$N <= significant
  )
  highlight <- unname(expand_figure_palette(profile, 1L, "colours"))
  muted <- "#B3B3B3"
  show_retained_line <- !is.null(retained) && !identical(as.integer(retained), as.integer(significant))
  subtitle <- sprintf(
    "%d of %d computed component(s) significant at alpha = %.2f (Patterson, Price & Reich 2006)",
    significant, nrow(tw), alpha
  )
  subtitle <- if (show_retained_line) {
    paste0(subtitle, sprintf("; %d retained (dotted line)", retained))
  } else if (!is.null(retained)) {
    paste0(subtitle, sprintf("; %d retained, matching the significant count", retained))
  } else {
    subtitle
  }
  p <- ggplot2::ggplot(df, ggplot2::aes(x = index, y = percent)) +
    ggplot2::geom_line(colour = muted, linewidth = 0.4) +
    ggplot2::geom_vline(
      xintercept = significant + 0.5, linetype = "dashed",
      colour = "#666666", linewidth = 0.4
    )
  if (show_retained_line) {
    p <- p + ggplot2::geom_vline(
      xintercept = retained + 0.5, linetype = "dotted",
      colour = "#CC5500", linewidth = 0.5
    )
  }
  p <- p +
    ggplot2::geom_point(
      ggplot2::aes(colour = retained_significant), size = 2.4, show.legend = FALSE
    ) +
    ggplot2::scale_colour_manual(values = c(`TRUE` = highlight, `FALSE` = muted)) +
    ggplot2::labs(
      title = "PCA component significance (Tracy-Widom test)",
      subtitle = subtitle,
      x = "Principal component", y = "Percent of total variance explained (%)"
    ) + theme_publication(figure_base_size(cfg))
  save_plot(p, "06b_PCA_Tracy_Widom_test", dirs, cfg$output$figure_formats, 8, 5, cfg$output$dpi)
  invisible(p)
}

plot_pca <- function(pca, cfg, dirs, metadata = NULL) {
  fmts <- cfg$output$figure_formats; dpi <- cfg$output$dpi
  label <- cfg$output$label_samples
  do_label <- identical(label, "all") || (identical(label, "auto") && nrow(pca$scores) <= 60L)
  has_population <- "population" %in% names(pca$scores) && any(!is.na(pca$scores$population))
  style <- figure_style_name(cfg)
  profile <- figure_style_profile(style)
  if (!is.null(pca$tracy_widom)) {
    plot_pca_tracy_widom(
      pca$tracy_widom, pca$tracy_widom_significant, pca$tracy_widom_alpha,
      pca$retained_components, cfg, dirs, profile
    )
  }
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

# Mean-imputes missing genotypes per resampled locus column before computing
# the Manhattan distance -- an approximation specific to bootstrap replicates
# (which resample loci with replacement and so cannot use
# SNPRelate::snpgdsIBS()'s own pairwise-complete C computation: it rejects
# duplicate SNP ids outright, "Some of snp.id do not exist!", confirmed
# directly). The main, displayed tree is unaffected and continues to use
# SNPRelate::snpgdsIBS()'s exact result. On complete data (no missing
# genotypes) this reduces to SNPRelate's own documented IBS formula exactly
# -- verified directly: max absolute difference ~5e-17 (floating-point
# noise) against snpgdsIBS()'s `1 - ibs` on a real quickstart subset.
ibs_bootstrap_distance <- function(geno_subset) {
  if (anyNA(geno_subset)) {
    locus_means <- colMeans(geno_subset, na.rm = TRUE)
    locus_means[is.nan(locus_means)] <- 0
    missing <- which(is.na(geno_subset), arr.ind = TRUE)
    geno_subset[missing] <- locus_means[missing[, 2L]]
  }
  stats::dist(geno_subset, method = "manhattan") / (2 * ncol(geno_subset))
}

bootstrap_nj_ibs_tree <- function(reference_tree, gds, sample_ids, snp_ids, metadata,
                                  replicates, workers, seed) {
  if (replicates <= 0L || length(snp_ids) < 2L) return(NULL)
  geno <- SNPRelate::snpgdsGetGeno(gds, sample.id = sample_ids, snp.id = snp_ids, verbose = FALSE)
  rownames(geno) <- public_sample_ids(metadata, sample_ids)
  n_snps <- ncol(geno)
  seeds <- tree_bootstrap_replicate_seeds(seed, replicates)
  build_one <- function(i) {
    set.seed(seeds[[i]])
    idx <- sample.int(n_snps, n_snps, replace = TRUE)
    tryCatch(ape::nj(ibs_bootstrap_distance(geno[, idx, drop = FALSE])), error = function(e) e)
  }
  trees <- run_tree_bootstrap_replicates(replicates, workers, build_one)
  support <- bootstrap_tree_support(reference_tree, trees)
  if (is.null(support)) return(NULL)
  list(support = support, replicates = length(trees))
}

build_nj_tree <- function(ibs, metadata, cfg, dirs, gds = NULL, sample_ids = NULL, snp_ids = NULL) {
  tree <- ape::nj(stats::as.dist(ibs$distance))
  bootstrap <- NULL
  if (isTRUE(cfg$analyses$tree_bootstrap$enabled) && !is.null(gds)) {
    bootstrap <- bootstrap_nj_ibs_tree(
      tree, gds, sample_ids, snp_ids, metadata,
      cfg$analyses$tree_bootstrap$replicates, cfg$compute$threads, cfg$compute$seed
    )
    if (!is.null(bootstrap)) tree$node.label <- as.character(bootstrap$support)
  }
  ape::write.tree(tree, file.path(dirs$trees, "IBS_neighbor_joining.nwk"))
  attr(tree, "bootstrap_replicates") <- if (is.null(bootstrap)) 0L else bootstrap$replicates
  tree
}
