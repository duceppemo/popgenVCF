#' Construct the population-level genetic distance and tree analysis module descriptor
#'
#' The descriptor owns the complete population-tree registry contract:
#' Nei's (1972) standard genetic distance between populations
#' (`adegenet::dist.genpop()`) and a neighbour-joining tree of populations
#' built from it -- distinct from the existing individual-level `tree`
#' module, which builds an NJ tree from IBS distance between samples, not
#' from population allele frequencies.
#'
#' @return A `PopgenVCFModuleSpec` object.
#' @export
population_tree_module_spec <- function() {
  new_analysis_module_spec(
    name = "population_tree",
    run = run_module_population_tree,
    requires = "diversity",
    enabled = function(cfg) !identical(cfg$analyses$population_tree, FALSE),
    description = "Nei's standard genetic distance and NJ tree between populations",
    validate = validate_population_tree_result,
    outputs = "population_tree",
    references = "Nei 1972",
    resource_class = "standard",
    contract_version = "1.0"
  )
}
