#' Write publication-ready PCA artifacts
#'
#' @param coordinates Data frame containing `sample_id`, `PC1`, and `PC2`.
#' @param eigenvalues Numeric PCA eigenvalues.
#' @param metadata Optional data frame containing `sample_id` and `population`.
#' @param output_dir Analysis output directory.
#' @param palette Optional named population colour vector.
#' @param module Artifact module name.
#' @return A `PopgenVCFArtifactManifest`.
#' @export
write_pca_publication_artifacts <- function(coordinates, eigenvalues, metadata = NULL,
                                            output_dir, palette = NULL,
                                            module = "pca") {
  if (!is.data.frame(coordinates) || !"sample_id" %in% names(coordinates)) {
    stop("coordinates must be a data frame containing sample_id", call. = FALSE)
  }
  pc_cols <- grep("^PC[0-9]+$", names(coordinates), value = TRUE)
  if (!all(c("PC1", "PC2") %in% pc_cols)) stop("coordinates must contain PC1 and PC2", call. = FALSE)
  eigenvalues <- as.numeric(eigenvalues)
  if (!length(eigenvalues) || any(!is.finite(eigenvalues)) || any(eigenvalues < 0) || sum(eigenvalues) <= 0) {
    stop("eigenvalues must be finite nonnegative values with a positive sum", call. = FALSE)
  }

  dirs <- list(
    tables = file.path(output_dir, "tables"), figures = file.path(output_dir, "figures"),
    methods = file.path(output_dir, "methods"), captions = file.path(output_dir, "captions"),
    validation = file.path(output_dir, "validation"), data = file.path(output_dir, "source_data")
  )
  invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

  coords <- data.table::as.data.table(coordinates)
  if (anyDuplicated(coords$sample_id)) stop("coordinate sample_id values must be unique", call. = FALSE)
  if (!is.null(metadata)) {
    metadata <- data.table::as.data.table(metadata)
    if (!all(c("sample_id", "population") %in% names(metadata))) stop("metadata must contain sample_id and population", call. = FALSE)
    if (anyDuplicated(metadata$sample_id)) stop("metadata sample_id values must be unique", call. = FALSE)
    coords <- merge(coords, metadata[, .(sample_id, population)], by = "sample_id", all.x = TRUE, sort = FALSE)
  }

  variance <- data.table::data.table(
    component = paste0("PC", seq_along(eigenvalues)), eigenvalue = eigenvalues,
    variance_percent = 100 * eigenvalues / sum(eigenvalues),
    cumulative_percent = 100 * cumsum(eigenvalues) / sum(eigenvalues)
  )
  paths <- list(
    coordinates = file.path(dirs$tables, "PCA_coordinates.tsv"),
    variance = file.path(dirs$tables, "PCA_variance.tsv"),
    source = file.path(dirs$data, "PCA_figure_source.tsv"),
    methods = file.path(dirs$methods, "PCA_methods.md"),
    caption = file.path(dirs$captions, "PCA_caption.md"),
    validation = file.path(dirs$validation, "PCA_validation.tsv"),
    pdf = file.path(dirs$figures, "PCA_PC1_PC2.pdf"),
    svg = file.path(dirs$figures, "PCA_PC1_PC2.svg"),
    png = file.path(dirs$figures, "PCA_PC1_PC2.png")
  )
  data.table::fwrite(coords, paths$coordinates, sep = "\t", quote = FALSE, na = "NA")
  data.table::fwrite(variance, paths$variance, sep = "\t", quote = FALSE, na = "NA")
  data.table::fwrite(coords, paths$source, sep = "\t", quote = FALSE, na = "NA")

  writeLines(paste0(
    "Principal component analysis was performed on the quality-controlled, linkage-disequilibrium-pruned genotype matrix. ",
    "PC1 and PC2 explained ", sprintf("%.2f", variance$variance_percent[1]), "% and ",
    sprintf("%.2f", variance$variance_percent[2]), "% of the retained genetic variance, respectively."
  ), paths$methods, useBytes = TRUE)
  writeLines(paste0(
    "Principal component analysis of ", nrow(coords), " samples. PC1 and PC2 explain ",
    sprintf("%.2f", variance$variance_percent[1]), "% and ", sprintf("%.2f", variance$variance_percent[2]),
    "% of the retained genetic variance, respectively."
  ), paths$caption, useBytes = TRUE)

  validation <- data.table::data.table(
    check = c("finite_coordinates", "finite_eigenvalues", "variance_sums_to_100", "sample_ids_unique"),
    passed = c(
      all(vapply(coords[, ..pc_cols], function(x) all(is.finite(x)), logical(1))),
      all(is.finite(eigenvalues)), abs(sum(variance$variance_percent) - 100) < 1e-8,
      !anyDuplicated(coords$sample_id)
    )
  )
  data.table::fwrite(validation, paths$validation, sep = "\t", quote = FALSE)
  if (!all(validation$passed)) stop("PCA publication validation failed", call. = FALSE)

  plot_one <- function(path, device) {
    if (device == "pdf") grDevices::pdf(path, width = 7, height = 6, useDingbats = FALSE)
    if (device == "svg") grDevices::svg(path, width = 7, height = 6)
    if (device == "png") grDevices::png(path, width = 2100, height = 1800, res = 300)
    on.exit(grDevices::dev.off(), add = TRUE)
    point_col <- rep("black", nrow(coords))
    if ("population" %in% names(coords)) {
      pops <- as.character(coords$population)
      if (is.null(palette)) {
        lev <- sort(unique(pops[!is.na(pops)]))
        palette <- stats::setNames(grDevices::hcl.colors(length(lev), "Dark 3"), lev)
      }
      point_col <- unname(palette[pops]); point_col[is.na(point_col)] <- "grey40"
    }
    graphics::plot(coords$PC1, coords$PC2, pch = 21, bg = point_col, col = "black", cex = 1.1,
      xlab = sprintf("PC1 (%.2f%%)", variance$variance_percent[1]),
      ylab = sprintf("PC2 (%.2f%%)", variance$variance_percent[2]), main = "Principal component analysis")
    graphics::abline(h = 0, v = 0, lty = 3, col = "grey75")
    if ("population" %in% names(coords)) {
      lev <- sort(unique(as.character(coords$population[!is.na(coords$population)])))
      graphics::legend("topright", legend = lev, pt.bg = unname(palette[lev]), pch = 21, bty = "n", cex = 0.8)
    }
  }
  plot_one(paths$pdf, "pdf"); plot_one(paths$svg, "svg"); plot_one(paths$png, "png")

  manifest <- new_artifact_manifest(list(
    new_analysis_artifact(module, "coordinates", "table", paths$coordinates, "tsv", "PCA sample coordinates"),
    new_analysis_artifact(module, "variance", "table", paths$variance, "tsv", "PCA eigenvalues and explained variance"),
    new_analysis_artifact(module, "pc1_pc2_pdf", "figure", paths$pdf, "pdf", "Vector PCA scatterplot"),
    new_analysis_artifact(module, "pc1_pc2_svg", "figure", paths$svg, "svg", "Editable vector PCA scatterplot"),
    new_analysis_artifact(module, "pc1_pc2_png", "figure", paths$png, "png", "High-resolution PCA scatterplot"),
    new_analysis_artifact(module, "methods", "methods", paths$methods, "md", "Manuscript-ready PCA methods text"),
    new_analysis_artifact(module, "caption", "caption", paths$caption, "md", "PCA figure caption"),
    new_analysis_artifact(module, "validation", "validation", paths$validation, "tsv", "PCA artifact validation checks"),
    new_analysis_artifact(module, "figure_source", "data", paths$source, "tsv", "PCA figure source data")
  ))
  validate_artifact_manifest(manifest, must_exist = TRUE)
  manifest
}
