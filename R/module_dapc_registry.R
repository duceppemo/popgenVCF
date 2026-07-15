#' Construct the DAPC analysis module descriptor
#'
#' The descriptor owns the complete DAPC registry contract while preserving
#' its existing diversity dependency, configuration enablement, validation,
#' population-metadata requirements, and output schema.
#'
#' @return A `PopgenVCFModuleSpec` object.
#' @export
dapc_module_spec <- function() {
  new_analysis_module_spec(
    name = "dapc",
    run = run_module_dapc,
    requires = "diversity",
    enabled = function(cfg) isTRUE(cfg$analyses$dapc),
    description = "Discriminant analysis of principal components",
    validate = validate_dapc_result,
    outputs = "dapc",
    references = "Jombart et al. 2010",
    resource_class = "heavy",
    contract_version = "1.0"
  )
}
