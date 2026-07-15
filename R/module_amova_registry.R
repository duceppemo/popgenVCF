#' Construct the AMOVA analysis module descriptor
#'
#' The descriptor owns AMOVA execution, its dependency on diversity, the
#' configuration enablement predicate, validation, output name, scientific
#' reference, resource classification, and contract version.
#'
#' @return A `PopgenVCFModuleSpec` object.
#' @export
amova_module_spec <- function() {
  new_analysis_module_spec(
    name = "amova",
    run = run_module_amova,
    requires = "diversity",
    enabled = function(cfg) isTRUE(cfg$analyses$amova),
    description = "Analysis of molecular variance",
    validate = validate_amova_result,
    outputs = "amova",
    references = "Excoffier et al. 1992",
    resource_class = "heavy",
    contract_version = "1.0"
  )
}
