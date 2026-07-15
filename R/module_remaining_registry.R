#' Construct the diversity analysis module descriptor
#' @return A `PopgenVCFModuleSpec` object.
#' @export
diversity_module_spec <- function() {
  new_analysis_module_spec(
    name = "diversity",
    run = run_module_diversity,
    description = "Sample, population, and locus diversity",
    validate = validate_diversity_result,
    outputs = c("diversity", "diversity_ci"),
    references = "Nei 1987",
    resource_class = "heavy",
    contract_version = "1.0"
  )
}

#' Construct the neighbour-joining tree module descriptor
#' @return A `PopgenVCFModuleSpec` object.
#' @export
tree_module_spec <- function() {
  new_analysis_module_spec(
    name = "tree",
    run = run_module_tree,
    requires = "ibs",
    description = "Neighbour-joining tree from IBS distance",
    validate = validate_tree_result,
    outputs = "tree",
    references = "Saitou and Nei 1987",
    contract_version = "1.0"
  )
}

#' Construct the ADMIXTURE module descriptor
#' @return A `PopgenVCFModuleSpec` object.
#' @export
admixture_module_spec <- function() {
  new_analysis_module_spec(
    name = "admixture",
    run = run_module_admixture,
    enabled = function(cfg) isTRUE(cfg$analyses$admixture$enabled),
    description = "External ADMIXTURE cross-validation",
    validate = validate_admixture_result,
    outputs = "admixture_cv",
    references = "Alexander et al. 2009",
    resource_class = "external",
    contract_version = "1.0"
  )
}

#' Construct the fastStructure module descriptor
#' @return A `PopgenVCFModuleSpec` object.
#' @export
faststructure_module_spec <- function() {
  new_analysis_module_spec(
    name = "faststructure",
    run = run_module_faststructure,
    enabled = function(cfg) isTRUE(cfg$analyses$faststructure$enabled),
    description = "External fastStructure ancestry inference",
    validate = validate_population_structure_result,
    outputs = "faststructure",
    references = "Raj et al. 2014",
    resource_class = "external",
    contract_version = "1.0"
  )
}

#' Construct the sNMF module descriptor
#' @return A `PopgenVCFModuleSpec` object.
#' @export
snmf_module_spec <- function() {
  new_analysis_module_spec(
    name = "snmf",
    run = run_module_snmf,
    enabled = function(cfg) isTRUE(cfg$analyses$snmf$enabled),
    description = "LEA sNMF ancestry inference",
    validate = validate_population_structure_result,
    outputs = "snmf",
    references = "Frichot et al. 2014",
    resource_class = "external",
    contract_version = "1.0"
  )
}

#' Construct the chromosome-specific analysis module descriptor
#' @return A `PopgenVCFModuleSpec` object.
#' @export
chromosome_module_spec <- function() {
  new_analysis_module_spec(
    name = "chromosome",
    run = run_module_chromosome,
    enabled = function(cfg) isTRUE(cfg$analyses$chromosome_specific),
    description = "Chromosome-specific PCA and FST",
    validate = validate_chromosome_result,
    outputs = "chromosome_summary",
    resource_class = "heavy",
    contract_version = "1.0"
  )
}
