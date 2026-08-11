#' Construct the site frequency spectrum and bottleneck-screen module descriptor
#'
#' The descriptor owns the complete bottleneck registry contract: the folded
#' site frequency spectrum per population and the mode-shift bottleneck test
#' (Luikart and Cornuet 1998), computed from `compute_diversity()`'s
#' already-computed per-locus allele frequencies.
#'
#' @return A `PopgenVCFModuleSpec` object.
#' @export
bottleneck_module_spec <- function() {
  new_analysis_module_spec(
    name = "bottleneck",
    run = run_module_bottleneck,
    requires = "diversity",
    enabled = function(cfg) !identical(cfg$analyses$bottleneck, FALSE),
    description = "Site frequency spectrum and mode-shift bottleneck screen",
    validate = validate_bottleneck_result,
    outputs = "bottleneck",
    references = "Luikart and Cornuet 1998",
    resource_class = "light",
    contract_version = "1.0"
  )
}
