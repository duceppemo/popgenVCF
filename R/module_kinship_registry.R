#' Construct the kinship/relatedness analysis module descriptor
#'
#' The descriptor owns the complete pairwise-kinship registry contract:
#' KING-robust kinship estimation (Manichaikul et al. 2010), the kinship and
#' IBS0 matrices, the full pairs table, and a close-relative summary table
#' while preserving the existing runner, result schema, and VCF-only
#' behavior.
#'
#' @return A `PopgenVCFModuleSpec` object.
#' @export
kinship_module_spec <- function() {
  new_analysis_module_spec(
    name = "kinship",
    run = run_module_kinship,
    enabled = function(cfg) !identical(cfg$analyses$kinship, FALSE),
    description = "Pairwise sample kinship (KING-robust) and close-relative detection",
    validate = validate_kinship_result,
    outputs = "kinship",
    references = "Manichaikul et al. 2010",
    resource_class = "heavy",
    contract_version = "1.0"
  )
}
