#' Construct the population assignment test analysis module descriptor
#'
#' The descriptor owns the complete population-assignment registry contract:
#' a frequency-based self-assignment test (Paetkau et al. 1995, 2004) that
#' scores each sample's leave-one-out genotype likelihood (Rannala and
#' Mountain 1997) against every population's allele frequencies and flags
#' samples that assign to a population other than their recorded metadata
#' label -- candidate migrants or metadata errors.
#'
#' @return A `PopgenVCFModuleSpec` object.
#' @export
population_assignment_module_spec <- function() {
  new_analysis_module_spec(
    name = "population_assignment",
    run = run_module_population_assignment,
    enabled = function(cfg) !identical(cfg$analyses$population_assignment, FALSE),
    description = "Frequency-based population self-assignment test",
    validate = validate_population_assignment_result,
    outputs = "population_assignment",
    references = c("Paetkau et al. 1995", "Paetkau et al. 2004", "Rannala and Mountain 1997"),
    resource_class = "standard",
    contract_version = "1.0"
  )
}
