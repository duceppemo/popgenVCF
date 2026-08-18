#' Construct the sex-biased dispersal test module descriptor
#'
#' The descriptor owns the complete sexbias registry contract: a
#' population-level test of whether one recorded sex disperses more than the
#' other (Goudet, Perrin, and Waser 2002), computed by `run_module_sexbias()`
#' from `compute_diversity()`'s already-computed genotype matrix
#' (`requires = "diversity"`, matching `clonality`'s reuse of the same
#' genotype matrix -- both are population-gated, so unlike `pcadapt` this
#' dependency cannot force `diversity` to run when population metadata is
#' unavailable). Skips gracefully, returning `NULL`, when the optional
#' `hierfstat` package is not installed or the metadata's `sex` column is
#' absent or does not resolve to at least two samples per recorded sex.
#'
#' @return A `PopgenVCFModuleSpec` object.
#' @export
sexbias_module_spec <- function() {
  new_analysis_module_spec(
    name = "sexbias",
    run = run_module_sexbias,
    requires = "diversity",
    enabled = function(cfg) !identical(cfg$analyses$sexbias, FALSE),
    description = "Sex-biased dispersal test from population-assignment indices",
    validate = validate_sexbias_result,
    outputs = "sexbias",
    references = "Goudet, Perrin, and Waser 2002",
    resource_class = "standard",
    contract_version = "1.0"
  )
}
