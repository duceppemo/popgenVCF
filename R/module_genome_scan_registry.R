#' Construct the sliding-window genome scan analysis module descriptor
#'
#' The descriptor owns the complete genome-scan registry contract: windowed
#' Weir-Cockerham global FST, windowed diversity, and windowed Tajima's D,
#' tracked along physical position rather than only as a single genome-wide
#' summary, while preserving the existing runner, result schema, and
#' population-metadata requirements.
#'
#' @return A `PopgenVCFModuleSpec` object.
#' @export
genome_scan_module_spec <- function() {
  new_analysis_module_spec(
    name = "genome_scan",
    run = run_module_genome_scan,
    requires = "diversity",
    enabled = function(cfg) !identical(cfg$analyses$genome_scan, FALSE),
    description = "Sliding-window FST, diversity, and Tajima's D scans",
    validate = validate_genome_scan_result,
    outputs = "genome_scan",
    references = c("Weir and Cockerham 1984", "Tajima 1989"),
    resource_class = "heavy",
    contract_version = "1.0"
  )
}
