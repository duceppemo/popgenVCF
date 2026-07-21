# Phase 0.9.1 - deterministic publication report rendering contracts

.publication_report_formats <- c("docx", "html", "pdf")

.publication_report_scalar <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop(sprintf("%s must be one non-empty character value.", name), call. = FALSE)
  }
  invisible(TRUE)
}

.publication_report_fingerprint <- function(x) {
  candidate <- x
  candidate$fingerprint <- NULL
  digest::digest(candidate, algo = "sha256", serialize = TRUE)
}

#' Create a deterministic publication report specification
#'
#' @param formats Output formats selected from `html`, `pdf`, and `docx`.
#' @param style Stable rendering style identifier.
#' @param numbered_sections Whether sections are numbered.
#' @param numbered_figures Whether figures and tables are numbered.
#' @param bibliography Whether bibliography rendering is required.
#' @param supplementary_index Whether a supplementary-material index is required.
#' @return A fingerprinted `PopgenVCFPublicationReportSpec`.
#' @export
new_publication_report_spec <- function(
    formats = "html",
    style = "generic",
    numbered_sections = TRUE,
    numbered_figures = TRUE,
    bibliography = TRUE,
    supplementary_index = TRUE) {
  formats <- sort(unique(tolower(as.character(formats))))
  if (!length(formats) || any(!formats %in% .publication_report_formats)) {
    stop("formats must contain only html, pdf, or docx.", call. = FALSE)
  }
  .publication_report_scalar(style, "style")
  flags <- list(numbered_sections, numbered_figures, bibliography, supplementary_index)
  if (any(!vapply(flags, function(x) is.logical(x) && length(x) == 1L && !is.na(x), logical(1L)))) {
    stop("publication report options must be TRUE or FALSE.", call. = FALSE)
  }
  spec <- list(
    record_type = "popgenvcf_publication_report_spec",
    schema_version = "1.0.0",
    formats = formats,
    style = style,
    numbered_sections = numbered_sections,
    numbered_figures = numbered_figures,
    bibliography = bibliography,
    supplementary_index = supplementary_index
  )
  spec$fingerprint <- .publication_report_fingerprint(spec)
  class(spec) <- c("PopgenVCFPublicationReportSpec", "list")
  spec
}

#' Validate a publication report specification
#' @param spec A publication report specification.
#' @return `TRUE`, invisibly.
#' @export
validate_publication_report_spec <- function(spec) {
  if (!inherits(spec, "PopgenVCFPublicationReportSpec")) {
    stop("spec must be a publication report specification.", call. = FALSE)
  }
  if (!identical(spec$schema_version, "1.0.0") ||
      !identical(spec$formats, sort(unique(spec$formats))) ||
      !length(spec$formats) || any(!spec$formats %in% .publication_report_formats)) {
    stop("Malformed publication report specification.", call. = FALSE)
  }
  .publication_report_scalar(spec$style, "style")
  if (!identical(spec$fingerprint, .publication_report_fingerprint(spec))) {
    stop("Publication report specification fingerprint mismatch.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Build a deterministic publication report rendering plan
#'
#' @param manuscript A validated `PopgenVCFManuscript`.
#' @param spec A publication report specification.
#' @param renderer_id Stable renderer or backend identity.
#' @param renderer_version Stable renderer version.
#' @return A fingerprinted `PopgenVCFPublicationReportPlan`.
#' @export
new_publication_report_plan <- function(
    manuscript,
    spec = new_publication_report_spec(),
    renderer_id = "quarto",
    renderer_version = "unspecified") {
  validate_manuscript(manuscript)
  validate_publication_report_spec(spec)
  .publication_report_scalar(renderer_id, "renderer_id")
  .publication_report_scalar(renderer_version, "renderer_version")
  source <- render_manuscript_markdown(manuscript)
  manuscript_digest <- digest::digest(manuscript, algo = "sha256", serialize = TRUE)
  source_digest <- digest::digest(paste(source, collapse = "\n"), algo = "sha256", serialize = FALSE)
  outputs <- data.frame(
    format = spec$formats,
    path = paste0("manuscript.", spec$formats),
    media_type = unname(c(
      docx = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
      html = "text/html",
      pdf = "application/pdf"
    )[spec$formats]),
    stringsAsFactors = FALSE
  )
  plan <- list(
    record_type = "popgenvcf_publication_report_plan",
    schema_version = "1.0.0",
    project_id = manuscript$project_id,
    manuscript_digest = manuscript_digest,
    publication_digest = manuscript$publication_digest,
    source_digest = source_digest,
    specification_fingerprint = spec$fingerprint,
    renderer = list(id = renderer_id, version = renderer_version),
    outputs = outputs,
    author_editable_sections = c("abstract", "introduction", "results", "discussion", "declarations"),
    generated_sections = c("methods", "captions", "artifacts", "software", "parameters", "bibliography")
  )
  plan$fingerprint <- .publication_report_fingerprint(plan)
  class(plan) <- c("PopgenVCFPublicationReportPlan", "list")
  validate_publication_report_plan(plan, manuscript, spec)
  plan
}

#' Validate a publication report rendering plan
#' @param plan A publication report rendering plan.
#' @param manuscript The originating manuscript.
#' @param spec The originating rendering specification.
#' @return `TRUE`, invisibly.
#' @export
validate_publication_report_plan <- function(plan, manuscript, spec) {
  if (!inherits(plan, "PopgenVCFPublicationReportPlan")) {
    stop("plan must be a publication report rendering plan.", call. = FALSE)
  }
  validate_manuscript(manuscript)
  validate_publication_report_spec(spec)
  if (!identical(plan$manuscript_digest, digest::digest(manuscript, algo = "sha256", serialize = TRUE)) ||
      !identical(plan$publication_digest, manuscript$publication_digest)) {
    stop("Publication report plan is not bound to the supplied manuscript.", call. = FALSE)
  }
  if (!identical(plan$specification_fingerprint, spec$fingerprint) ||
      !identical(plan$outputs$format, spec$formats) || anyDuplicated(plan$outputs$path)) {
    stop("Publication report plan is not bound to the supplied specification.", call. = FALSE)
  }
  source <- render_manuscript_markdown(manuscript)
  expected_source <- digest::digest(paste(source, collapse = "\n"), algo = "sha256", serialize = FALSE)
  if (!identical(plan$source_digest, expected_source)) {
    stop("Publication report source fingerprint mismatch.", call. = FALSE)
  }
  if (!identical(plan$fingerprint, .publication_report_fingerprint(plan))) {
    stop("Publication report plan fingerprint mismatch.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Create a deterministic rendered-output manifest
#'
#' @param plan A validated publication report plan.
#' @param output_dir Directory containing rendered outputs.
#' @param warnings Stable renderer warning messages.
#' @return A fingerprinted output manifest.
#' @export
new_publication_report_output_manifest <- function(plan, output_dir, warnings = character()) {
  if (!inherits(plan, "PopgenVCFPublicationReportPlan")) {
    stop("plan must be a publication report rendering plan.", call. = FALSE)
  }
  .publication_report_scalar(output_dir, "output_dir")
  paths <- file.path(output_dir, plan$outputs$path)
  missing <- plan$outputs$path[!file.exists(paths)]
  if (length(missing)) {
    stop(sprintf("Missing rendered publication output(s): %s", paste(missing, collapse = ", ")), call. = FALSE)
  }
  files <- plan$outputs
  files$sha256 <- vapply(paths, digest::digest, character(1L), algo = "sha256", file = TRUE)
  files$bytes <- unname(file.info(paths)$size)
  warnings <- sort(unique(as.character(warnings[nzchar(as.character(warnings))])))
  manifest <- list(
    record_type = "popgenvcf_publication_report_output_manifest",
    schema_version = "1.0.0",
    plan_fingerprint = plan$fingerprint,
    renderer = plan$renderer,
    files = files,
    warnings = warnings,
    succeeded = TRUE
  )
  manifest$fingerprint <- .publication_report_fingerprint(manifest)
  class(manifest) <- c("PopgenVCFPublicationReportOutputManifest", "list")
  manifest
}

#' Validate rendered publication outputs
#' @param manifest A rendered-output manifest.
#' @param plan The originating publication report plan.
#' @param output_dir Directory containing rendered outputs.
#' @return `TRUE`, invisibly.
#' @export
validate_publication_report_output_manifest <- function(manifest, plan, output_dir) {
  if (!inherits(manifest, "PopgenVCFPublicationReportOutputManifest")) {
    stop("manifest must be a publication report output manifest.", call. = FALSE)
  }
  if (!inherits(plan, "PopgenVCFPublicationReportPlan") ||
      !identical(manifest$plan_fingerprint, plan$fingerprint) ||
      !identical(manifest$files$format, plan$outputs$format) ||
      !identical(manifest$files$path, plan$outputs$path)) {
    stop("Publication report output manifest is not bound to the supplied plan.", call. = FALSE)
  }
  if (!identical(manifest$fingerprint, .publication_report_fingerprint(manifest))) {
    stop("Publication report output manifest fingerprint mismatch.", call. = FALSE)
  }
  paths <- file.path(output_dir, manifest$files$path)
  if (any(!file.exists(paths))) stop("Rendered publication output is missing.", call. = FALSE)
  actual <- vapply(paths, digest::digest, character(1L), algo = "sha256", file = TRUE)
  if (!identical(unname(manifest$files$sha256), unname(actual))) {
    stop("Rendered publication output checksum mismatch.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Render a deterministic publication report plan summary
#' @param plan A publication report rendering plan.
#' @return Character vector containing Markdown report lines.
#' @export
publication_report_plan_report <- function(plan) {
  if (!inherits(plan, "PopgenVCFPublicationReportPlan") ||
      !identical(plan$fingerprint, .publication_report_fingerprint(plan))) {
    stop("Invalid publication report rendering plan.", call. = FALSE)
  }
  c(
    "# Publication report rendering plan",
    "",
    sprintf("- Project: `%s`", plan$project_id),
    sprintf("- Formats: `%s`", paste(plan$outputs$format, collapse = ", ")),
    sprintf("- Renderer: `%s` (`%s`)", plan$renderer$id, plan$renderer$version),
    sprintf("- Author-editable sections: `%d`", length(plan$author_editable_sections)),
    sprintf("- Generated sections: `%d`", length(plan$generated_sections)),
    sprintf("- Plan fingerprint: `%s`", plan$fingerprint)
  )
}
