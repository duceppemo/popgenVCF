#' Construct the runs-of-homozygosity (ROH) analysis module descriptor
#'
#' The descriptor owns the complete ROH registry contract: HMM-based
#' autozygosity detection (`bcftools roh`, Narasimhan et al. 2016), the full
#' per-run table, and a per-sample FROH summary table while preserving the
#' existing runner, result schema, and VCF-only behavior.
#'
#' @return A `PopgenVCFModuleSpec` object.
#' @export
roh_module_spec <- function() {
  new_analysis_module_spec(
    name = "roh",
    run = run_module_roh,
    enabled = function(cfg) !identical(cfg$analyses$roh, FALSE),
    description = "Runs of homozygosity and per-sample FROH",
    validate = validate_roh_result,
    outputs = "roh",
    references = "Narasimhan et al. 2016",
    resource_class = "heavy",
    contract_version = "1.0"
  )
}
