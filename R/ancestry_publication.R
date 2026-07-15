#' Write publication-ready ancestry artifacts
#'
#' Creates backend-neutral ancestry figures, source tables, manuscript text,
#' captions, and validation records from canonical ancestry objects. Population
#' metadata is optional; when absent, samples are ordered by dominant ancestry.
#'
#' @param ancestry A `PopgenVCFAncestryReplicate`,
#'   `PopgenVCFAncestryConsensus`, or `PopgenVCFAncestryResult`.
#' @param output_dir Output directory.
#' @param metadata Optional data frame with `sample_id` and optionally
#'   `population`.
#' @param k_selection Optional `PopgenVCFKSelection` object.
#' @param backend Optional backend used when `ancestry` is a result collection.
#' @param k Optional K used when `ancestry` is a result collection.
#' @param replicate Optional replicate used when `ancestry` is a result
#'   collection.
#' @param palette Optional cluster colour vector.
#' @param module Artifact module namespace.
#' @return A validated `PopgenVCFArtifactManifest`.
#' @export
write_ancestry_publication_artifacts <- function(
    ancestry, output_dir, metadata = NULL, k_selection = NULL,
    backend = NULL, k = NULL, replicate = NULL, palette = NULL,
    module = "ancestry") {
  selected <- select_publication_ancestry(ancestry, backend, k, replicate)
  consensus <- if (inherits(selected, "PopgenVCFAncestryConsensus")) selected else NULL
  q <- if (is.null(consensus)) selected$q else consensus$mean_q
  sample_ids <- if (is.null(consensus)) selected$sample_ids else consensus$sample_ids
  backend_name <- selected$backend
  k_value <- selected$k

  if (!is.null(consensus)) validate_ancestry_consensus(consensus) else validate_ancestry_replicate(selected)
  if (!is.null(k_selection)) validate_ancestry_k_selection(k_selection)
  if (!is.character(output_dir) || length(output_dir) != 1L || !nzchar(output_dir)) {
    stop("output_dir must be one non-empty path", call. = FALSE)
  }

  dirs <- list(
    tables = file.path(output_dir, "tables"),
    figures = file.path(output_dir, "figures"),
    methods = file.path(output_dir, "methods"),
    captions = file.path(output_dir, "captions"),
    validation = file.path(output_dir, "validation"),
    data = file.path(output_dir, "source_data")
  )
  invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

  clusters <- paste0("cluster_", seq_len(k_value))
  colnames(q) <- clusters
  source <- data.table::data.table(sample_id = sample_ids)
  source <- cbind(source, data.table::as.data.table(q))
  if (!is.null(consensus)) {
    for (j in seq_len(k_value)) {
      source[[paste0(clusters[j], "_sd")]] <- consensus$sd_q[, j]
      source[[paste0(clusters[j], "_lower")]] <- consensus$lower_q[, j]
      source[[paste0(clusters[j], "_upper")]] <- consensus$upper_q[, j]
    }
  }

  if (!is.null(metadata)) {
    md <- data.table::as.data.table(metadata)
    if (!"sample_id" %in% names(md)) stop("metadata must contain sample_id", call. = FALSE)
    if (anyDuplicated(md$sample_id)) stop("metadata sample_id values must be unique", call. = FALSE)
    keep <- intersect(c("sample_id", "population"), names(md))
    source <- merge(source, md[, ..keep], by = "sample_id", all.x = TRUE, sort = FALSE)
    source <- source[match(sample_ids, sample_id)]
  }

  source[, dominant_cluster := max.col(as.matrix(.SD), ties.method = "first"), .SDcols = clusters]
  order_cols <- if ("population" %in% names(source)) c("population", "dominant_cluster") else "dominant_cluster"
  data.table::setorderv(source, order_cols, rep(1L, length(order_cols)), na.last = TRUE)
  source[, sample_order := seq_len(.N)]

  if (is.null(palette)) palette <- grDevices::hcl.colors(k_value, "Dark 3")
  if (length(palette) < k_value) stop("palette must contain at least K colours", call. = FALSE)
  palette <- unname(palette[seq_len(k_value)])

  prefix <- sprintf("ancestry_%s_K%d", backend_name, k_value)
  paths <- list(
    q = file.path(dirs$tables, paste0(prefix, "_Q.tsv")),
    source = file.path(dirs$data, paste0(prefix, "_figure_source.tsv")),
    methods = file.path(dirs$methods, paste0(prefix, "_methods.md")),
    results = file.path(dirs$methods, paste0(prefix, "_results.md")),
    caption = file.path(dirs$captions, paste0(prefix, "_caption.md")),
    validation = file.path(dirs$validation, paste0(prefix, "_validation.tsv"))
  )
  data.table::fwrite(source, paths$q, sep = "\t", quote = FALSE, na = "NA")
  data.table::fwrite(source, paths$source, sep = "\t", quote = FALSE, na = "NA")

  n_rep <- if (is.null(consensus)) 1L else length(consensus$replicate_ids)
  writeLines(sprintf(
    "%s ancestry coefficients were analysed at K=%d. Cluster labels were aligned across %d replicate%s before publication output generation; consensus coefficients are reported when multiple replicates were available.",
    backend_name, k_value, n_rep, if (n_rep == 1L) "" else "s"
  ), paths$methods, useBytes = TRUE)
  result_text <- if (is.null(consensus)) {
    sprintf("Ancestry coefficients for %d samples were summarized at K=%d using %s.", length(sample_ids), k_value, backend_name)
  } else {
    sprintf("The K=%d %s consensus included %d replicates and had global stability %.3f.", k_value, backend_name, n_rep, consensus$global_stability)
  }
  writeLines(result_text, paths$results, useBytes = TRUE)
  writeLines(sprintf(
    "Ancestry coefficient estimates for %d samples at K=%d using %s. Samples are ordered by population when available and then by dominant ancestry cluster.",
    length(sample_ids), k_value, backend_name
  ), paths$caption, useBytes = TRUE)

  artifacts <- list(
    new_analysis_artifact(module, "q_table", "table", paths$q, "tsv", "Canonical ancestry coefficients"),
    new_analysis_artifact(module, "figure_source", "data", paths$source, "tsv", "Ancestry figure source data"),
    new_analysis_artifact(module, "methods", "methods", paths$methods, "md", "Manuscript-ready ancestry methods"),
    new_analysis_artifact(module, "results", "methods", paths$results, "md", "Manuscript-ready ancestry results"),
    new_analysis_artifact(module, "caption", "caption", paths$caption, "md", "Ancestry figure caption")
  )

  bar_paths <- ancestry_figure_paths(dirs$figures, paste0(prefix, "_barplot"))
  draw_bar <- function() plot_ancestry_barplot(source, clusters, palette, backend_name, k_value)
  ancestry_write_devices(bar_paths, draw_bar, width = max(8, nrow(source) * 0.06), height = 5.5)
  artifacts <- c(artifacts, ancestry_figure_artifacts(module, "barplot", bar_paths, "Ancestry coefficient barplot"))

  if (!is.null(consensus)) {
    uncertainty_path <- file.path(dirs$data, paste0(prefix, "_uncertainty.tsv"))
    stability_path <- file.path(dirs$data, paste0(prefix, "_stability.tsv"))
    data.table::fwrite(consensus$sample_uncertainty, uncertainty_path, sep = "\t", quote = FALSE)
    data.table::fwrite(consensus$cluster_stability, stability_path, sep = "\t", quote = FALSE)
    artifacts <- c(artifacts,
      list(new_analysis_artifact(module, "uncertainty_source", "data", uncertainty_path, "tsv", "Per-sample ancestry uncertainty")),
      list(new_analysis_artifact(module, "stability_source", "data", stability_path, "tsv", "Per-cluster ancestry stability")))

    uncertainty_paths <- ancestry_figure_paths(dirs$figures, paste0(prefix, "_uncertainty"))
    draw_uncertainty <- function() plot_ancestry_uncertainty(consensus)
    ancestry_write_devices(uncertainty_paths, draw_uncertainty, 7, 5)
    artifacts <- c(artifacts, ancestry_figure_artifacts(module, "uncertainty", uncertainty_paths, "Ancestry uncertainty plot"))

    stability_paths <- ancestry_figure_paths(dirs$figures, paste0(prefix, "_stability"))
    draw_stability <- function() plot_ancestry_stability(consensus)
    ancestry_write_devices(stability_paths, draw_stability, 7, 5)
    artifacts <- c(artifacts, ancestry_figure_artifacts(module, "stability", stability_paths, "Replicate stability plot"))
  }

  if (!is.null(k_selection)) {
    k_source <- file.path(dirs$data, "ancestry_K_selection.tsv")
    data.table::fwrite(k_selection$summary, k_source, sep = "\t", quote = FALSE, na = "NA")
    artifacts <- c(artifacts, list(new_analysis_artifact(module, "k_selection_source", "data", k_source, "tsv", "K-selection source data")))
    k_paths <- ancestry_figure_paths(dirs$figures, "ancestry_K_selection")
    draw_k <- function() plot_ancestry_k_selection(k_selection)
    ancestry_write_devices(k_paths, draw_k, 7, 5)
    artifacts <- c(artifacts, ancestry_figure_artifacts(module, "k_selection", k_paths, "Ancestry K-selection curve"))
  }

  validation <- data.table::data.table(
    check = c("finite_q", "q_rows_sum_to_one", "sample_ids_unique", "all_artifacts_exist"),
    passed = c(
      all(is.finite(q)),
      all(abs(rowSums(q) - 1) < 1e-6),
      !anyDuplicated(sample_ids),
      all(vapply(artifacts, function(z) file.exists(z$path), logical(1L)))
    )
  )
  data.table::fwrite(validation, paths$validation, sep = "\t", quote = FALSE)
  artifacts <- c(artifacts, list(new_analysis_artifact(module, "validation", "validation", paths$validation, "tsv", "Ancestry artifact validation checks")))
  if (!all(validation$passed)) stop("ancestry publication validation failed", call. = FALSE)

  manifest <- new_artifact_manifest(artifacts)
  validate_artifact_manifest(manifest, must_exist = TRUE)
  manifest
}

select_publication_ancestry <- function(x, backend = NULL, k = NULL, replicate = NULL) {
  if (inherits(x, "PopgenVCFAncestryConsensus") || inherits(x, "PopgenVCFAncestryReplicate")) return(x)
  validate_ancestry_result(x)
  reps <- x$replicates
  if (!is.null(backend)) reps <- reps[vapply(reps, function(z) z$backend == tolower(backend), logical(1L))]
  if (!is.null(k)) reps <- reps[vapply(reps, function(z) z$k == as.integer(k)[1L], logical(1L))]
  if (!is.null(replicate)) reps <- reps[vapply(reps, function(z) z$replicate == as.integer(replicate)[1L], logical(1L))]
  if (!length(reps)) stop("no ancestry replicate matches backend, K, and replicate filters", call. = FALSE)
  ord <- order(vapply(reps, `[[`, character(1L), "backend"), vapply(reps, `[[`, integer(1L), "k"), vapply(reps, `[[`, integer(1L), "replicate"))
  reps[[ord[1L]]]
}

ancestry_figure_paths <- function(dir, stem) {
  list(pdf = file.path(dir, paste0(stem, ".pdf")), svg = file.path(dir, paste0(stem, ".svg")), png = file.path(dir, paste0(stem, ".png")))
}

ancestry_write_devices <- function(paths, draw, width, height) {
  grDevices::pdf(paths$pdf, width = width, height = height, useDingbats = FALSE); draw(); grDevices::dev.off()
  grDevices::svg(paths$svg, width = width, height = height); draw(); grDevices::dev.off()
  grDevices::png(paths$png, width = width * 300, height = height * 300, res = 300); draw(); grDevices::dev.off()
  invisible(paths)
}

ancestry_figure_artifacts <- function(module, stem, paths, description) {
  list(
    new_analysis_artifact(module, paste0(stem, "_pdf"), "figure", paths$pdf, "pdf", paste("Vector", description)),
    new_analysis_artifact(module, paste0(stem, "_svg"), "figure", paths$svg, "svg", paste("Editable vector", description)),
    new_analysis_artifact(module, paste0(stem, "_png"), "figure", paths$png, "png", paste("High-resolution", description))
  )
}

plot_ancestry_barplot <- function(source, clusters, palette, backend, k) {
  mat <- t(as.matrix(source[, ..clusters]))
  graphics::barplot(mat, col = palette, border = NA, space = 0, axes = FALSE,
    xlab = "Samples", ylab = "Ancestry coefficient", main = sprintf("%s ancestry (K = %d)", backend, k))
  graphics::axis(2, las = 1)
  graphics::box()
  graphics::legend("topright", legend = clusters, fill = palette, border = NA, bty = "n", cex = 0.8)
}

plot_ancestry_uncertainty <- function(consensus) {
  u <- consensus$sample_uncertainty
  ord <- order(u$uncertainty, decreasing = TRUE)
  graphics::plot(seq_along(ord), u$uncertainty[ord], pch = 16,
    xlab = "Samples ordered by uncertainty", ylab = "Mean ancestry SD",
    main = sprintf("Ancestry uncertainty: %s K=%d", consensus$backend, consensus$k))
  graphics::abline(h = mean(u$uncertainty), lty = 2)
}

plot_ancestry_stability <- function(consensus) {
  s <- consensus$cluster_stability
  graphics::barplot(s$stability, names.arg = s$cluster, ylim = c(0, 1), las = 2,
    ylab = "Stability (1 - mean SD)", main = sprintf("Replicate stability: %s K=%d", consensus$backend, consensus$k))
  graphics::abline(h = consensus$global_stability, lty = 2)
}

plot_ancestry_k_selection <- function(x) {
  tab <- data.table::copy(x$summary)
  backends <- unique(tab$backend)
  yr <- range(c(tab$mean, tab$lower, tab$upper), finite = TRUE)
  graphics::plot(range(tab$k), yr, type = "n", xlab = "K", ylab = "Fit metric",
    main = sprintf("Ancestry model selection; recommended K=%d", x$overall_k))
  cols <- grDevices::hcl.colors(length(backends), "Dark 3")
  for (i in seq_along(backends)) {
    z <- tab[backend == backends[i]][order(k)]
    graphics::segments(z$k, z$lower, z$k, z$upper, col = cols[i])
    graphics::lines(z$k, z$mean, type = "b", pch = 16, col = cols[i])
  }
  graphics::abline(v = x$overall_k, lty = 2)
  graphics::legend("topright", legend = backends, col = cols, lty = 1, pch = 16, bty = "n")
}
