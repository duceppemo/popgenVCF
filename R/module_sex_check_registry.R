#' Construct the sex-check analysis module descriptor
#'
#' Genetic sex inference from X-chromosome heterozygosity (Visscher et al.
#' 2010's F-statistic, PLINK's `--check-sex` convention) and mismatch
#' detection against a supplied `sex` metadata column. VCF-only: runs from
#' genotypes alone and requires no population or geographic metadata.
#' Skips transparently (no table, no figure) when the analyzed VCF has too
#' few QC-passing X-chromosome SNPs, e.g. single-autosome inputs.
#'
#' @return A `PopgenVCFModuleSpec` object.
#' @export
sex_check_module_spec <- function() {
  new_analysis_module_spec(
    name = "sex_check",
    run = run_module_sex_check,
    enabled = function(cfg) !identical(cfg$analyses$sex_check, FALSE),
    description = "Genetic sex inference from X-chromosome heterozygosity and reported-sex mismatch detection",
    validate = validate_sex_check_result,
    outputs = "sex_check",
    references = "Visscher et al. 2010; Purcell et al. 2007 (PLINK --check-sex)",
    resource_class = "standard",
    contract_version = "1.0"
  )
}
