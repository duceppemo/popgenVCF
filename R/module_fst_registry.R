#' Construct the FST analysis module descriptor
#'
#' The descriptor owns the complete FST registry contract while preserving the
#' current Weir-Cockerham estimates, confidence intervals, Wright's
#' island-model Nm (gene flow) estimates, output schema, and
#' population-metadata requirements.
#'
#' @return A `PopgenVCFModuleSpec` object.
#' @export
fst_module_spec <- function() {
  new_analysis_module_spec(
    name = "fst",
    run = run_module_fst,
    enabled = function(cfg) !identical(cfg$analyses$fst, FALSE),
    description = "Global and pairwise Weir-Cockerham FST, and Nm (gene flow)",
    validate = validate_fst_result,
    outputs = c("fst", "fst_ci"),
    references = c("Weir and Cockerham 1984", "Wright 1931"),
    resource_class = "heavy",
    contract_version = "1.0"
  )
}
