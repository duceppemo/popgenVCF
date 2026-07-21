# Phase 0.9.2 - journal presets and deterministic publication layouts

.publication_layout_formats <- c("docx", "html", "pdf")
.publication_layout_profile_ids <- c("g3", "general", "molecular-ecology", "nature-style", "plos")

.publication_layout_fingerprint <- function(x) {
  candidate <- unclass(x)
  candidate$fingerprint <- NULL
  digest::digest(candidate, algo = "sha256", serialize = TRUE)
}

.publication_layout_scalar <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    stop(sprintf("%s must be one non-empty character value.", name), call. = FALSE)
  }
  trimws(x)
}

.publication_layout_named_list <- function(x, name) {
  if (!is.list(x) || (length(x) && (is.null(names(x)) || any(!nzchar(names(x)))))) {
    stop(sprintf("%s must be a named list.", name), call. = FALSE)
  }
  if (!length(x)) return(list())
  x[order(names(x))]
}

#' Create a deterministic publication layout profile
#'
#' @param id Stable layout-profile identifier.
#' @param journal_profile Existing validated journal submission profile.
#' @param formats Supported report formats.
#' @param geometry Page and margin geometry.
#' @param typography Typography requirements.
#' @param structure Heading, numbering, caption, and placement requirements.
#' @param bibliography Bibliography and citation requirements.
#' @param submission Submission-specific layout requirements.
#' @param renderer_parameters Backend-independent normalized renderer parameters.
#' @param version Layout-profile version.
#' @return A fingerprinted `PopgenVCFPublicationLayoutProfile`.
#' @export
new_publication_layout_profile <- function(
    id,
    journal_profile = generic_journal_profile(),
    formats = .publication_layout_formats,
    geometry = list(paper = "letter", margin_mm = 25.4, columns = 1L),
    typography = list(font_family = "serif", font_size_pt = 11, line_spacing = 1.5),
    structure = list(heading_depth = 3L, numbered_sections = TRUE,
                     numbered_figures = TRUE, captions = "below",
                     table_captions = "above", figure_placement = "near-reference"),
    bibliography = list(style = "author-date", doi_as_url = TRUE),
    submission = list(line_numbering = FALSE, separate_title_page = FALSE,
                      supplementary_index = TRUE),
    renderer_parameters = list(),
    version = "1.0.0") {
  id <- .publication_layout_scalar(id, "id")
  version <- .publication_layout_scalar(version, "version")
  validate_journal_profile(journal_profile)
  formats <- sort(unique(tolower(as.character(formats))))
  if (!length(formats) || any(!formats %in% .publication_layout_formats)) {
    stop("formats must contain only html, pdf, or docx.", call. = FALSE)
  }
  geometry <- .publication_layout_named_list(geometry, "geometry")
  typography <- .publication_layout_named_list(typography, "typography")
  structure <- .publication_layout_named_list(structure, "structure")
  bibliography <- .publication_layout_named_list(bibliography, "bibliography")
  submission <- .publication_layout_named_list(submission, "submission")
  renderer_parameters <- .publication_layout_named_list(renderer_parameters, "renderer_parameters")
  profile <- list(
    record_type = "popgenvcf_publication_layout_profile",
    schema_version = "1.0.0",
    id = id,
    version = version,
    journal_profile_id = journal_profile$id,
    journal_profile_digest = journal_profile$digest,
    formats = formats,
    geometry = geometry,
    typography = typography,
    structure = structure,
    bibliography = bibliography,
    submission = submission,
    renderer_parameters = renderer_parameters
  )
  profile$fingerprint <- .publication_layout_fingerprint(profile)
  class(profile) <- c("PopgenVCFPublicationLayoutProfile", "list")
  validate_publication_layout_profile(profile, journal_profile)
  profile
}

#' Validate a publication layout profile
#' @param profile A publication layout profile.
#' @param journal_profile Optional originating journal profile.
#' @return `TRUE`, invisibly.
#' @export
validate_publication_layout_profile <- function(profile, journal_profile = NULL) {
  if (!inherits(profile, "PopgenVCFPublicationLayoutProfile")) {
    stop("profile must be a publication layout profile.", call. = FALSE)
  }
  if (!identical(profile$schema_version, "1.0.0") ||
      !identical(profile$formats, sort(unique(profile$formats))) ||
      !length(profile$formats) || any(!profile$formats %in% .publication_layout_formats)) {
    stop("Malformed publication layout profile.", call. = FALSE)
  }
  .publication_layout_scalar(profile$id, "profile id")
  .publication_layout_scalar(profile$version, "profile version")
  for (field in c("geometry", "typography", "structure", "bibliography", "submission", "renderer_parameters")) {
    .publication_layout_named_list(profile[[field]], field)
  }
  if (!is.null(journal_profile)) {
    validate_journal_profile(journal_profile)
    if (!identical(profile$journal_profile_id, journal_profile$id) ||
        !identical(profile$journal_profile_digest, journal_profile$digest)) {
      stop("Publication layout profile is not bound to the supplied journal profile.", call. = FALSE)
    }
  }
  if (!identical(profile$fingerprint, .publication_layout_fingerprint(profile))) {
    stop("Publication layout profile fingerprint mismatch.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Return a built-in publication layout profile
#'
#' @param name One of `general`, `nature-style`, `g3`, `molecular-ecology`, or `plos`.
#' @return A deterministic publication layout profile.
#' @export
publication_layout_profile <- function(name = c("g3", "general", "molecular-ecology", "nature-style", "plos")) {
  name <- match.arg(name)
  journal <- generic_journal_profile()
  common_structure <- list(
    captions = "below", figure_placement = "near-reference",
    heading_depth = 3L, numbered_figures = TRUE,
    numbered_sections = TRUE, table_captions = "above"
  )
  if (name == "general") {
    return(new_publication_layout_profile("general", journal))
  }
  if (name == "nature-style") {
    return(new_publication_layout_profile(
      name, journal, geometry = list(columns = 1L, margin_mm = 25.4, paper = "a4"),
      typography = list(font_family = "sans", font_size_pt = 10, line_spacing = 1.5),
      structure = modifyList(common_structure, list(numbered_sections = FALSE, heading_depth = 2L)),
      bibliography = list(doi_as_url = TRUE, style = "numeric"),
      submission = list(line_numbering = TRUE, separate_title_page = TRUE, supplementary_index = TRUE),
      renderer_parameters = list(reference_location = "document-end")
    ))
  }
  if (name == "g3") {
    return(new_publication_layout_profile(
      name, journal, typography = list(font_family = "serif", font_size_pt = 12, line_spacing = 2),
      structure = common_structure,
      bibliography = list(doi_as_url = TRUE, style = "author-date"),
      submission = list(line_numbering = TRUE, separate_title_page = FALSE, supplementary_index = TRUE),
      renderer_parameters = list(keywords_heading = "Keywords")
    ))
  }
  if (name == "molecular-ecology") {
    return(new_publication_layout_profile(
      name, journal, geometry = list(columns = 1L, margin_mm = 25, paper = "a4"),
      typography = list(font_family = "serif", font_size_pt = 12, line_spacing = 2),
      structure = common_structure,
      bibliography = list(doi_as_url = TRUE, style = "author-date"),
      submission = list(line_numbering = TRUE, separate_title_page = TRUE, supplementary_index = TRUE),
      renderer_parameters = list(running_title = TRUE)
    ))
  }
  new_publication_layout_profile(
    name, journal, geometry = list(columns = 1L, margin_mm = 25.4, paper = "letter"),
    typography = list(font_family = "serif", font_size_pt = 12, line_spacing = 2),
    structure = modifyList(common_structure, list(numbered_sections = FALSE)),
    bibliography = list(doi_as_url = TRUE, style = "numeric"),
    submission = list(line_numbering = TRUE, separate_title_page = TRUE, supplementary_index = TRUE),
    renderer_parameters = list(data_availability_heading = "Data Availability")
  )
}

#' Bind a layout profile to a publication report specification
#'
#' @param spec A publication report specification.
#' @param profile A publication layout profile.
#' @param overrides Named deterministic layout overrides.
#' @return A fingerprinted layout binding.
#' @export
bind_publication_layout <- function(spec, profile, overrides = list()) {
  validate_publication_report_spec(spec)
  validate_publication_layout_profile(profile)
  unsupported <- setdiff(spec$formats, profile$formats)
  if (length(unsupported)) {
    stop(sprintf("Layout profile does not support format(s): %s", paste(unsupported, collapse = ", ")), call. = FALSE)
  }
  overrides <- .publication_layout_named_list(overrides, "overrides")
  allowed <- c("geometry", "typography", "structure", "bibliography", "submission", "renderer_parameters")
  if (length(setdiff(names(overrides), allowed))) stop("Layout overrides contain unknown fields.", call. = FALSE)
  resolved <- list(
    geometry = profile$geometry, typography = profile$typography,
    structure = profile$structure, bibliography = profile$bibliography,
    submission = profile$submission, renderer_parameters = profile$renderer_parameters
  )
  for (field in names(overrides)) {
    resolved[[field]] <- modifyList(resolved[[field]], .publication_layout_named_list(overrides[[field]], field))
    resolved[[field]] <- resolved[[field]][order(names(resolved[[field]]))]
  }
  binding <- list(
    record_type = "popgenvcf_publication_layout_binding",
    schema_version = "1.0.0",
    specification_fingerprint = spec$fingerprint,
    profile_id = profile$id,
    profile_version = profile$version,
    profile_fingerprint = profile$fingerprint,
    formats = spec$formats,
    resolved = resolved
  )
  binding$fingerprint <- .publication_layout_fingerprint(binding)
  class(binding) <- c("PopgenVCFPublicationLayoutBinding", "list")
  binding
}

#' Validate a publication layout binding
#' @param binding A layout binding.
#' @param spec Originating report specification.
#' @param profile Originating layout profile.
#' @return `TRUE`, invisibly.
#' @export
validate_publication_layout_binding <- function(binding, spec, profile) {
  if (!inherits(binding, "PopgenVCFPublicationLayoutBinding")) stop("binding must be a publication layout binding.", call. = FALSE)
  validate_publication_report_spec(spec)
  validate_publication_layout_profile(profile)
  if (!identical(binding$specification_fingerprint, spec$fingerprint) ||
      !identical(binding$profile_fingerprint, profile$fingerprint) ||
      !identical(binding$formats, spec$formats)) {
    stop("Publication layout binding is not bound to its specification and profile.", call. = FALSE)
  }
  if (!identical(binding$fingerprint, .publication_layout_fingerprint(binding))) {
    stop("Publication layout binding fingerprint mismatch.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Return deterministic renderer parameters for a layout binding
#' @param binding A validated layout binding.
#' @return A normalized named list suitable for renderer execution.
#' @export
publication_layout_parameters <- function(binding) {
  if (!inherits(binding, "PopgenVCFPublicationLayoutBinding") ||
      !identical(binding$fingerprint, .publication_layout_fingerprint(binding))) {
    stop("Invalid publication layout binding.", call. = FALSE)
  }
  out <- unlist(binding$resolved, recursive = TRUE, use.names = TRUE)
  out <- as.list(out[order(names(out))])
  c(list(layout_profile = binding$profile_id,
         layout_profile_version = binding$profile_version,
         layout_fingerprint = binding$profile_fingerprint), out)
}

#' Render a deterministic publication layout summary
#' @param profile A publication layout profile.
#' @return Markdown report lines.
#' @export
publication_layout_report <- function(profile) {
  validate_publication_layout_profile(profile)
  c(
    "# Publication layout profile", "",
    sprintf("- Profile: `%s`", profile$id),
    sprintf("- Version: `%s`", profile$version),
    sprintf("- Formats: `%s`", paste(profile$formats, collapse = ", ")),
    sprintf("- Journal profile: `%s`", profile$journal_profile_id),
    sprintf("- Layout fingerprint: `%s`", profile$fingerprint)
  )
}
