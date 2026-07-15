pca_publication_artifact_names <- function() {
  c(
    "coordinates", "variance", "pc1_pc2_pdf", "pc1_pc2_svg",
    "pc1_pc2_png", "methods", "caption", "validation", "figure_source"
  )
}

#' Construct the PCA analysis module descriptor
#'
#' The descriptor owns PCA execution, validation, scientific references,
#' resource classification, result names, and publication artifact contract.
#'
#' @return A `PopgenVCFModuleSpec` object.
#' @export
pca_module_spec <- function() {
  new_analysis_module_spec(
    name = "pca",
    run = run_module_pca,
    description = "Principal component analysis",
    validate = validate_pca_result,
    outputs = "pca",
    references = "Patterson et al. 2006",
    resource_class = "heavy",
    contract_version = "1.0",
    artifacts = pca_publication_artifact_names(),
    artifacts_must_exist = TRUE
  )
}
