#' Construct the IBS/MDS analysis module descriptor
#'
#' The descriptor owns the complete IBS/MDS registry contract while preserving
#' the existing runner, result schema, VCF-only behavior, and downstream tree
#' and isolation-by-distance dependencies.
#'
#' @return A `PopgenVCFModuleSpec` object.
#' @export
ibs_module_spec <- function() {
  new_analysis_module_spec(
    name = "ibs",
    run = run_module_ibs,
    description = "IBS matrices and multidimensional scaling",
    validate = validate_ibs_result,
    outputs = "ibs",
    references = "Zheng et al. 2012",
    resource_class = "heavy",
    contract_version = "1.0"
  )
}
