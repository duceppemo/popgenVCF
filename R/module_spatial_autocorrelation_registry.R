#' Construct the spatial autocorrelation analysis module descriptor
#'
#' The descriptor owns the complete spatial-autocorrelation registry
#' contract: a distance-class correlogram (Smouse and Peakall 1999) with a
#' permutation-based null envelope and per-class p-value, complementing the
#' single overall Mantel r the `ibd` module already computes.
#'
#' @return A `PopgenVCFModuleSpec` object.
#' @export
spatial_autocorrelation_module_spec <- function() {
  new_analysis_module_spec(
    name = "spatial_autocorrelation",
    run = run_module_spatial_autocorrelation,
    enabled = function(cfg) !identical(cfg$analyses$spatial_autocorrelation, FALSE),
    description = "Spatial autocorrelation correlogram (Smouse and Peakall 1999)",
    validate = validate_spatial_autocorrelation_result,
    outputs = "spatial_autocorrelation",
    references = "Smouse and Peakall 1999",
    resource_class = "standard",
    contract_version = "1.0"
  )
}
