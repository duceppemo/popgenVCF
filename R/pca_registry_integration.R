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
  r <- new_analysis_registry()
  r <- register_analysis(r, "diversity", run_module_diversity,
    description = "Sample, population, and locus diversity",
    validate = validate_diversity_result,
    outputs = c("diversity", "diversity_ci"),
    references = "Nei 1987", resource_class = "heavy")
  r <- register_analysis_module(r, pca_module_spec())
  r <- register_analysis_module(r, ibs_module_spec())
  r <- register_analysis(r, "tree", run_module_tree, requires = "ibs",
    description = "Neighbour-joining tree from IBS distance",
    validate = validate_tree_result,
    outputs = "tree", references = "Saitou and Nei 1987")
  r <- register_analysis_module(r, fst_module_spec())
  r <- register_analysis_module(r, dapc_module_spec())
  r <- register_analysis_module(r, amova_module_spec())
  r <- register_analysis_module(r, ibd_module_spec())
  r <- register_analysis(r, "admixture", run_module_admixture,
    enabled = function(cfg) isTRUE(cfg$analyses$admixture$enabled),
    description = "External ADMIXTURE cross-validation",
    validate = validate_admixture_result,
    outputs = "admixture_cv", references = "Alexander et al. 2009",
    resource_class = "external")
  r <- register_analysis(r, "faststructure", run_module_faststructure,
    enabled = function(cfg) isTRUE(cfg$analyses$faststructure$enabled),
    description = "External fastStructure ancestry inference",
    validate = validate_population_structure_result,
    outputs = "faststructure", references = "Raj et al. 2014",
    resource_class = "external")
  r <- register_analysis(r, "snmf", run_module_snmf,
    enabled = function(cfg) isTRUE(cfg$analyses$snmf$enabled),
    description = "LEA sNMF ancestry inference",
    validate = validate_population_structure_result,
    outputs = "snmf", references = "Frichot et al. 2014",
    resource_class = "external")
  r <- register_analysis(r, "chromosome", run_module_chromosome,
    enabled = function(cfg) isTRUE(cfg$analyses$chromosome_specific),
    description = "Chromosome-specific PCA and FST",
    validate = validate_chromosome_result,
    outputs = "chromosome_summary", resource_class = "heavy")
  r
}
