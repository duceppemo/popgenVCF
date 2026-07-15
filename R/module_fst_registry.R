#' Construct the FST analysis module descriptor
#'
#' The descriptor owns the complete FST registry contract while preserving the
#' current Weir-Cockerham estimates, confidence intervals, output schema, and
#' population-metadata requirements.
#'
#' @return A `PopgenVCFModuleSpec` object.
#' @export
fst_module_spec <- function() {
  new_analysis_module_spec(
    name = "fst",
    run = run_module_fst,
    description = "Global and pairwise Weir-Cockerham FST",
    validate = validate_fst_result,
    outputs = c("fst", "fst_ci"),
    references = "Weir and Cockerham 1984",
    resource_class = "heavy",
    contract_version = "1.0"
  )
}
