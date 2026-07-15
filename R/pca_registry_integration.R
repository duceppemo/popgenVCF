run_module_pca <- function(analysis, context) {
  cfg <- context$cfg; dirs <- context$dirs
  pca <- run_pca(
    context$gds, context$sample_ids, context$final_snps,
    context$metadata, cfg$analyses$n_pcs, cfg$compute$threads
  )
  context$pca <- pca
  analysis <- set_analysis_result(analysis, "pca", pca[c("scores", "variance")])
  write_tsv(pca$scores, file.path(dirs$tables, "12_PCA_scores.tsv"))
  write_tsv(pca$variance, file.path(dirs$tables, "13_PCA_variance.tsv"))
  plot_pca(pca, cfg, dirs)

  coordinates <- data.table::copy(pca$scores)
  data.table::setnames(coordinates, "sample", "sample_id")
  publication_metadata <- NULL
  if ("population" %in% names(coordinates)) {
    publication_metadata <- coordinates[, .(sample_id, population)]
    coordinates[, population := NULL]
  }
  artifacts <- write_pca_publication_artifacts(
    coordinates = coordinates,
    eigenvalues = pca$object$eigenval,
    metadata = publication_metadata,
    output_dir = dirs$root
  )
  list(analysis = analysis, context = context, artifacts = artifacts)
}

#' Construct the built-in analysis registry
#' @return A populated `PopgenVCFRegistry`.
#' @export
default_analysis_registry <- function() {
  modules <- list(
    diversity_module_spec(),
    pca_module_spec(),
    ibs_module_spec(),
    tree_module_spec(),
    fst_module_spec(),
    dapc_module_spec(),
    amova_module_spec(),
    ibd_module_spec(),
    admixture_module_spec(),
    faststructure_module_spec(),
    snmf_module_spec(),
    chromosome_module_spec()
  )

  registry <- new_analysis_registry()
  for (module in modules) {
    registry <- register_analysis_module(registry, module)
  }
  registry
}
