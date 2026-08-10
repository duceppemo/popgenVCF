#' Construct the LD-based effective population size analysis module descriptor
#'
#' The descriptor owns the complete Ne(LD) registry contract: per-population
#' contemporary effective population size from linkage disequilibrium between
#' unlinked (cross-chromosome) marker pairs (Waples 2006; Waples & Do 2008),
#' while preserving the existing runner, result schema, and per-population
#' requirement.
#'
#' @return A `PopgenVCFModuleSpec` object.
#' @export
ne_ld_module_spec <- function() {
  new_analysis_module_spec(
    name = "ne_ld",
    run = run_module_ne_ld,
    enabled = function(cfg) !identical(cfg$analyses$ne_ld, FALSE),
    description = "LD-based contemporary effective population size per population",
    validate = validate_ne_ld_result,
    outputs = "ne_ld",
    references = "Waples 2006; Waples and Do 2008",
    resource_class = "standard",
    contract_version = "1.0"
  )
}
