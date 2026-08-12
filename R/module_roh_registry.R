#' Construct the runs-of-homozygosity (ROH) analysis module descriptor
#'
#' The descriptor owns the complete ROH registry contract: HMM-based
#' autozygosity detection (`bcftools roh`, Narasimhan et al. 2016), the full
#' per-run table, a per-sample FROH summary table, and a run-length-class
#' breakdown of FROH (short/intermediate/long, Ceballos et al. 2018) while
#' preserving the existing runner, result schema, and VCF-only behavior.
#'
#' @return A `PopgenVCFModuleSpec` object.
#' @export
roh_module_spec <- function() {
  new_analysis_module_spec(
    name = "roh",
    run = run_module_roh,
    enabled = function(cfg) !identical(cfg$analyses$roh, FALSE),
    description = "Runs of homozygosity, per-sample FROH, and length-class breakdown",
    validate = validate_roh_result,
    outputs = "roh",
    references = c("Narasimhan et al. 2016", "Ceballos et al. 2018"),
    resource_class = "heavy",
    contract_version = "1.0"
  )
}
