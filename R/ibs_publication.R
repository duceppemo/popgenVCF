#' Compute deterministic classical MDS coordinates from an IBS distance matrix
#'
#' @param distance A symmetric sample-by-sample distance matrix with zero diagonal.
#' @param k Number of MDS axes to retain.
#' @param sample_ids Optional sample identifiers. Defaults to matrix row names.
#' @return A list containing `coordinates`, `eigenvalues`, and `goodness_of_fit`.
#' @export
compute_ibs_mds <- function(distance, k = 2L, sample_ids = NULL) {
  distance <- as.matrix(distance)
  storage.mode(distance) <- "double"
  if (!is.numeric(distance) || nrow(distance) != ncol(distance) || nrow(distance) < 2L) {
    stop("distance must be a numeric square matrix with at least two samples", call. = FALSE)
  }
  if (any(!is.finite(distance))) stop("distance contains non-finite values", call. = FALSE)
  if (any(distance < -1e-12)) stop("distance contains negative values", call. = FALSE)
  if (!isTRUE(all.equal(distance, t(distance), tolerance = 1e-10, check.attributes = FALSE))) {
    stop("distance must be symmetric", call. = FALSE)
  }
  if (any(abs(diag(distance)) > 1e-10)) stop("distance diagonal must be zero", call. = FALSE)

  n <- nrow(distance)
  k <- as.integer(k)
  if (length(k) != 1L || is.na(k) || k < 1L || k >= n) {
    stop("k must be an integer between 1 and n - 1", call. = FALSE)
  }
  if (is.null(sample_ids)) sample_ids <- rownames(distance)
  if (is.null(sample_ids)) sample_ids <- sprintf("sample_%d", seq_len(n))
  sample_ids <- as.character(sample_ids)
  if (length(sample_ids) != n || anyNA(sample_ids) || any(!nzchar(sample_ids)) || anyDuplicated(sample_ids)) {
    stop("sample_ids must contain one unique non-empty identifier per sample", call. = FALSE)
  }

  fit <- stats::cmdscale(stats::as.dist(distance), k = k, eig = TRUE, add = FALSE)
  points <- as.matrix(fit$points)
  if (is.null(dim(points))) points <- matrix(points, ncol = 1L)
  colnames(points) <- paste0("MDS", seq_len(ncol(points)))

  # Eigenvectors are sign-indeterminate. Orient every axis deterministically by
  # making the largest absolute coordinate positive.
  for (axis in seq_len(ncol(points))) {
    anchor <- which.max(abs(points[, axis]))
    if (points[anchor, axis] < 0) points[, axis] <- -points[, axis]
  }

  coordinates <- data.frame(sample_id = sample_ids, points, check.names = FALSE,
                            stringsAsFactors = FALSE)
  positive <- fit$eig[fit$eig > 0]
  goodness <- if (length(positive)) {
    cumsum(positive) / sum(positive)
  } else numeric()

  list(
    coordinates = coordinates,
    eigenvalues = as.numeric(fit$eig),
    goodness_of_fit = goodness,
    additive_constant = unname(fit$ac %||% 0)
  )
}

#' Write publication-ready IBS and MDS artifacts
#'
#' @param similarity Symmetric IBS similarity matrix.
#' @param distance Symmetric IBS distance matrix.
#' @param metadata Optional data frame containing `sample_id` (or `sample`) and `population`.
#' @param output_dir Analysis output directory.
#' @param k Number of MDS axes to retain.
#' @param palette Optional named population colour vector.
#' @param module Artifact module name.
#' @return A `PopgenVCFArtifactManifest`.
#' @export
write_ibs_publication_artifacts <- function(similarity, distance, metadata = NULL,
                                            output_dir, k = 2L, palette = NULL,
                                            module = "ibs") {
  similarity <- as.matrix(similarity)
  distance <- as.matrix(distance)
  if (!identical(dim(similarity), dim(distance))) {
    stop("similarity and distance matrices must have identical dimensions", call. = FALSE)
  }
  if (nrow(similarity) != ncol(similarity) || nrow(similarity) < 2L) {
    stop("similarity and distance must be square matrices", call. = FALSE)
  }
  if (any(!is.finite(similarity)) || any(similarity < -1e-10 | similarity > 1 + 1e-10)) {
    stop("similarity values must be finite and within [0,1]", call. = FALSE)
  }
  if (!isTRUE(all.equal(similarity, t(similarity), tolerance = 1e-10, check.attributes = FALSE))) {
    stop("similarity must be symmetric", call. = FALSE)
  }
  if (any(abs(diag(similarity) - 1) > 1e-10)) stop("similarity diagonal must equal one", call. = FALSE)

  ids <- rownames(distance) %||% rownames(similarity)
  mds <- compute_ibs_mds(distance, k = k, sample_ids = ids)
  coords <- data.table::as.data.table(mds$coordinates)
  if (!is.null(metadata)) {
    metadata <- data.table::as.data.table(metadata)
    id_col <- if ("sample_id" %in% names(metadata)) "sample_id" else if ("sample" %in% names(metadata)) "sample" else NA_character_
    if (is.na(id_col) || !"population" %in% names(metadata)) {
      stop("metadata must contain sample_id (or sample) and population", call. = FALSE)
    }
    meta <- metadata[, .(sample_id = as.character(get(id_col)), population = as.character(population))]
    if (anyDuplicated(meta$sample_id)) stop("metadata sample identifiers must be unique", call. = FALSE)
    coords <- merge(coords, meta, by = "sample_id", all.x = TRUE, sort = FALSE)
    coords <- coords[match(mds$coordinates$sample_id, sample_id)]
  }

  dirs <- list(
    tables = file.path(output_dir, "tables"), figures = file.path(output_dir, "figures"),
    methods = file.path(output_dir, "methods"), captions = file.path(output_dir, "captions"),
    validation = file.path(output_dir, "validation"), data = file.path(output_dir, "source_data")
  )
  invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))
  paths <- list(
    similarity = file.path(dirs$tables, "IBS_similarity.tsv"),
    distance = file.path(dirs$tables, "IBS_distance.tsv"),
    coordinates = file.path(dirs$tables, "IBS_MDS_coordinates.tsv"),
    eigenvalues = file.path(dirs$tables, "IBS_MDS_eigenvalues.tsv"),
    source = file.path(dirs$data, "IBS_MDS_figure_source.tsv"),
    methods = file.path(dirs$methods, "IBS_MDS_methods.md"),
    caption = file.path(dirs$captions, "IBS_MDS_caption.md"),
    validation = file.path(dirs$validation, "IBS_MDS_validation.tsv"),
    pdf = file.path(dirs$figures, "IBS_MDS1_MDS2.pdf"),
    svg = file.path(dirs$figures, "IBS_MDS1_MDS2.svg"),
    png = file.path(dirs$figures, "IBS_MDS1_MDS2.png")
  )

  matrix_table <- function(x) data.table::data.table(sample_id = rownames(x) %||% mds$coordinates$sample_id, as.data.frame(x, check.names = FALSE))
  data.table::fwrite(matrix_table(similarity), paths$similarity, sep = "\t", quote = FALSE, na = "NA")
  data.table::fwrite(matrix_table(distance), paths$distance, sep = "\t", quote = FALSE, na = "NA")
  data.table::fwrite(coords, paths$coordinates, sep = "\t", quote = FALSE, na = "NA")
  data.table::fwrite(coords, paths$source, sep = "\t", quote = FALSE, na = "NA")
  eig <- data.table::data.table(axis = seq_along(mds$eigenvalues), eigenvalue = mds$eigenvalues,
                                positive = mds$eigenvalues > 0)
  eig[, positive_variance_percent := if (sum(pmax(eigenvalue, 0)) > 0) 100 * pmax(eigenvalue, 0) / sum(pmax(eigenvalue, 0)) else 0]
  data.table::fwrite(eig, paths$eigenvalues, sep = "\t", quote = FALSE)

  axis_percent <- eig$positive_variance_percent[seq_len(min(2L, nrow(eig)))]
  writeLines(paste0(
    "Pairwise identity-by-state similarity was converted to genetic distance and summarized by classical multidimensional scaling. ",
    "The first two MDS axes represented ", sprintf("%.2f", axis_percent[1]), "% and ",
    sprintf("%.2f", axis_percent[2]), "% of the positive-eigenvalue variation, respectively."
  ), paths$methods, useBytes = TRUE)
  writeLines(paste0(
    "Identity-by-state multidimensional scaling of ", nrow(coords), " samples. Points represent samples; proximity indicates greater genome-wide IBS similarity."
  ), paths$caption, useBytes = TRUE)

  validation <- data.table::data.table(
    check = c("similarity_symmetric", "similarity_diagonal_one", "distance_symmetric", "distance_diagonal_zero", "finite_mds_coordinates", "sample_ids_unique"),
    passed = c(
      isTRUE(all.equal(similarity, t(similarity), tolerance = 1e-10, check.attributes = FALSE)),
      all(abs(diag(similarity) - 1) <= 1e-10),
      isTRUE(all.equal(distance, t(distance), tolerance = 1e-10, check.attributes = FALSE)),
      all(abs(diag(distance)) <= 1e-10),
      all(vapply(coords[, grep("^MDS[0-9]+$", names(coords), value = TRUE), with = FALSE], function(x) all(is.finite(x)), logical(1))),
      !anyDuplicated(coords$sample_id)
    )
  )
  data.table::fwrite(validation, paths$validation, sep = "\t", quote = FALSE)
  if (!all(validation$passed)) stop("IBS/MDS publication validation failed", call. = FALSE)

  plot_one <- function(path, device) {
    if (device == "pdf") grDevices::pdf(path, width = 7, height = 6, useDingbats = FALSE)
    if (device == "svg") grDevices::svg(path, width = 7, height = 6)
    if (device == "png") grDevices::png(path, width = 2100, height = 1800, res = 300)
    on.exit(grDevices::dev.off(), add = TRUE)
    point_col <- rep("grey40", nrow(coords))
    if ("population" %in% names(coords)) {
      pops <- as.character(coords$population)
      lev <- sort(unique(pops[!is.na(pops)]))
      if (is.null(palette)) palette <- stats::setNames(grDevices::hcl.colors(length(lev), "Dark 3"), lev)
      point_col <- unname(palette[pops]); point_col[is.na(point_col)] <- "grey40"
    }
    graphics::plot(coords$MDS1, coords$MDS2, pch = 21, bg = point_col, col = "black", cex = 1.1,
      xlab = sprintf("MDS1 (%.2f%%)", axis_percent[1]), ylab = sprintf("MDS2 (%.2f%%)", axis_percent[2]),
      main = "IBS multidimensional scaling")
    graphics::abline(h = 0, v = 0, lty = 3, col = "grey75")
    if ("population" %in% names(coords)) {
      lev <- sort(unique(as.character(coords$population[!is.na(coords$population)])))
      graphics::legend("topright", legend = lev, pt.bg = unname(palette[lev]), pch = 21, bty = "n", cex = 0.8)
    }
  }
  plot_one(paths$pdf, "pdf"); plot_one(paths$svg, "svg"); plot_one(paths$png, "png")

  manifest <- new_artifact_manifest(list(
    new_analysis_artifact(module, "similarity", "table", paths$similarity, "tsv", "Pairwise IBS similarity matrix"),
    new_analysis_artifact(module, "distance", "table", paths$distance, "tsv", "Pairwise IBS distance matrix"),
    new_analysis_artifact(module, "mds_coordinates", "table", paths$coordinates, "tsv", "IBS MDS sample coordinates"),
    new_analysis_artifact(module, "mds_eigenvalues", "table", paths$eigenvalues, "tsv", "IBS MDS eigenvalues"),
    new_analysis_artifact(module, "mds_pdf", "figure", paths$pdf, "pdf", "Vector IBS MDS scatterplot"),
    new_analysis_artifact(module, "mds_svg", "figure", paths$svg, "svg", "Editable IBS MDS scatterplot"),
    new_analysis_artifact(module, "mds_png", "figure", paths$png, "png", "High-resolution IBS MDS scatterplot"),
    new_analysis_artifact(module, "methods", "methods", paths$methods, "md", "Manuscript-ready IBS/MDS methods text"),
    new_analysis_artifact(module, "caption", "caption", paths$caption, "md", "IBS/MDS figure caption"),
    new_analysis_artifact(module, "validation", "validation", paths$validation, "tsv", "IBS/MDS artifact validation checks"),
    new_analysis_artifact(module, "figure_source", "data", paths$source, "tsv", "IBS/MDS figure source data")
  ))
  validate_artifact_manifest(manifest, must_exist = TRUE)
  manifest
}
