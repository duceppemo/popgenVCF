#' Construct the sex-check analysis module descriptor
#'
#' Genetic sex inference from X-chromosome heterozygosity (Visscher et al.
#' 2010's F-statistic) and Y-chromosome genotype call rate (males have a Y
#' to call genotypes from, females do not), PLINK's `--check-sex`
#' convention extended to use both complementary signals, plus mismatch
#' detection against a supplied `sex` metadata column. When both signals are
#' available and confidently disagree, the combined call is `discordant` --
#' informative evidence of a data problem, not averaged away. VCF-only:
#' runs from genotypes alone and requires no population or geographic
#' metadata. Skips transparently (no table, no figure) only when the
#' analyzed VCF has too few QC-passing SNPs on *both* chromosomes; either
#' signal alone is used when only one chromosome is present.
#'
#' @return A `PopgenVCFModuleSpec` object.
#' @export
sex_check_module_spec <- function() {
  new_analysis_module_spec(
    name = "sex_check",
    run = run_module_sex_check,
    enabled = function(cfg) !identical(cfg$analyses$sex_check, FALSE),
    description = "Genetic sex inference from X-chromosome heterozygosity and Y-chromosome call rate, and reported-sex mismatch detection",
    validate = validate_sex_check_result,
    outputs = "sex_check",
    references = "Visscher et al. 2010; Purcell et al. 2007 (PLINK --check-sex)",
    resource_class = "standard",
    contract_version = "1.0"
  )
}
