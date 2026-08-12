#' Construct the FST analysis module descriptor
#'
#' The descriptor owns the complete FST registry contract while preserving the
#' current Weir-Cockerham estimates, confidence intervals, Wright's
#' island-model Nm (gene flow) estimates, Jost's (2008) D differentiation
#' measure, Weir and Goudet's (2017) population-specific FST (beta), output
#' schema, and population-metadata requirements.
#'
#' @return A `PopgenVCFModuleSpec` object.
#' @export
fst_module_spec <- function() {
  new_analysis_module_spec(
    name = "fst",
    run = run_module_fst,
    requires = "diversity",
    enabled = function(cfg) !identical(cfg$analyses$fst, FALSE),
    description = "Global and pairwise Weir-Cockerham FST, Nm, Jost's D, and population-specific FST",
    validate = validate_fst_result,
    outputs = c("fst", "fst_ci"),
    references = c("Weir and Cockerham 1984", "Wright 1931", "Jost 2008", "Weir and Goudet 2017"),
    resource_class = "heavy",
    contract_version = "1.0"
  )
}
