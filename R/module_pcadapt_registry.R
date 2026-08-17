#' Construct the pcadapt outlier scan module descriptor
#'
#' The descriptor owns the complete pcadapt registry contract: a genome-wide,
#' PCA-based statistical outlier scan for local adaptation/selection (Luu et
#' al. 2016; Prive et al. 2020), computed by `run_module_pcadapt()` directly
#' from the retained genotype matrix. Deliberately declares no `requires`:
#' unlike most modules in this registry, no population metadata is needed --
#' pcadapt is an unsupervised, PCA-based method -- so it must not depend on
#' the population-gated `diversity` module, which would otherwise be forced
#' to run (and fail) even when population metadata is unavailable.
#'
#' @return A `PopgenVCFModuleSpec` object.
#' @export
pcadapt_module_spec <- function() {
  new_analysis_module_spec(
    name = "pcadapt",
    run = run_module_pcadapt,
    enabled = function(cfg) !identical(cfg$analyses$pcadapt, FALSE),
    description = "Genome-wide PCA-based outlier scan for local adaptation/selection",
    validate = validate_pcadapt_result,
    outputs = "pcadapt",
    references = c("Luu et al. 2016", "Prive et al. 2020"),
    resource_class = "standard",
    contract_version = "1.0"
  )
}
