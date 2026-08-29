#' Construct the multilocus genotype and clonal diversity analysis module descriptor
#'
#' The descriptor owns the complete clonality registry contract: per-population
#' genotypic diversity indices, samples sharing an identical multilocus
#' genotype, a genotype accumulation curve, and a minimum spanning network of
#' multilocus genotypes, computed by `run_module_clonality()` from
#' `compute_diversity()`'s already-computed genotype matrix
#' (`requires = "diversity"`).
#'
#' @return A `PopgenVCFModuleSpec` object.
#' @export
clonality_module_spec <- function() {
  new_analysis_module_spec(
    name = "clonality",
    run = run_module_clonality,
    requires = "diversity",
    enabled = function(cfg) !identical(cfg$analyses$clonality, FALSE),
    description = "Multilocus genotype (MLG) and clonal diversity analysis",
    validate = validate_clonality_result,
    outputs = "clonality",
    references = "Kamvar, Tabima, and Grunwald 2014",
    resource_class = "heavy",
    contract_version = "1.0"
  )
}
