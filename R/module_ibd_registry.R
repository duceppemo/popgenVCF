#' Construct the isolation-by-distance analysis module descriptor
#'
#' The descriptor owns Mantel, partial Mantel (controlling for population
#' identity), and isolation-by-distance execution, its dependency on IBS,
#' configuration enablement, validation, output name, scientific references,
#' resource classification, and contract version.
#'
#' @return A `PopgenVCFModuleSpec` object.
#' @export
ibd_module_spec <- function() {
  new_analysis_module_spec(
    name = "ibd",
    run = run_module_ibd,
    requires = "ibs",
    enabled = function(cfg) {
      isTRUE(cfg$analyses$mantel) ||
        isTRUE(cfg$analyses$isolation_by_distance)
    },
    description = "Mantel test, partial Mantel, and isolation by distance",
    validate = validate_ibd_result,
    outputs = "ibd",
    references = c("Mantel 1967", "Rousset 1997", "Smouse, Long, and Sokal 1986"),
    resource_class = "standard",
    contract_version = "1.0"
  )
}
