#' Construct the linkage-disequilibrium decay analysis module descriptor
#'
#' The descriptor owns the complete LD-decay registry contract: pairwise
#' genotypic correlation (`SNPRelate::snpgdsLDMat()`, already a dependency)
#' between nearby SNPs on the QC-passed, unpruned marker set, binned by
#' physical distance into a mean-r-squared decay curve, while preserving the
#' existing runner, result schema, and VCF-only behavior.
#'
#' @return A `PopgenVCFModuleSpec` object.
#' @export
ld_decay_module_spec <- function() {
  new_analysis_module_spec(
    name = "ld_decay",
    run = run_module_ld_decay,
    enabled = function(cfg) !identical(cfg$analyses$ld_decay, FALSE),
    description = "Linkage disequilibrium decay by physical distance",
    validate = validate_ld_decay_result,
    outputs = "ld_decay",
    resource_class = "standard",
    contract_version = "1.0"
  )
}
