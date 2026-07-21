# Dynamic namespace synchronization for the Phase 0.9 publication-report API.
# These exports are registered at namespace load so the implementation remains
# usable before the next generated NAMESPACE synchronization pass.
.onLoad <- function(libname, pkgname) {
  ns <- asNamespace(pkgname)
  namespaceExport(ns, c(
    "new_publication_report_spec",
    "validate_publication_report_spec",
    "new_publication_report_plan",
    "validate_publication_report_plan",
    "new_publication_report_output_manifest",
    "validate_publication_report_output_manifest",
    "publication_report_plan_report",
    "new_publication_report_renderer",
    "validate_publication_report_renderer",
    "quarto_publication_report_renderer",
    "execute_publication_report_plan",
    "validate_publication_report_execution",
    "publication_report_execution_report"
  ))
}
