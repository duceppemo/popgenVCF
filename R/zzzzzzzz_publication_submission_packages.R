# Phase 0.9.4 - deterministic submission packages and supplementary indexes

.publication_submission_fingerprint <- function(x) {
  candidate <- unclass(x)
  candidate$fingerprint <- NULL
  digest::digest(candidate, algo = "sha256", serialize = TRUE)
}

.publication_submission_scalar <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    stop(sprintf("%s must be one non-empty character value.", name), call. = FALSE)
  }
  trimws(x)
}

.publication_submission_entries <- function(entries, name = "entries") {
  if (!is.list(entries)) stop(sprintf("%s must be a list.", name), call. = FALSE)
  if (!length(entries)) return(list())
  normalized <- lapply(entries, function(entry) {
    if (!is.list(entry)) stop(sprintf("Every %s entry must be a list.", name), call. = FALSE)
    required <- c("path", "role", "media_type", "size_bytes", "sha256", "source_fingerprint")
    if (!all(required %in% names(entry))) stop(sprintf("Every %s entry must contain required fields.", name), call. = FALSE)
    entry$path <- .publication_submission_scalar(entry$path, "entry path")
    entry$role <- .publication_submission_scalar(entry$role, "entry role")
    entry$media_type <- .publication_submission_scalar(entry$media_type, "entry media type")
    entry$sha256 <- .publication_submission_scalar(entry$sha256, "entry sha256")
    entry$source_fingerprint <- .publication_submission_scalar(entry$source_fingerprint, "entry source fingerprint")
    entry$size_bytes <- as.numeric(entry$size_bytes)
    if (length(entry$size_bytes) != 1L || is.na(entry$size_bytes) || entry$size_bytes < 0) {
      stop("entry size_bytes must be one non-negative number.", call. = FALSE)
    }
    entry[order(names(entry))]
  })
  paths <- vapply(normalized, `[[`, character(1L), "path")
  if (anyDuplicated(paths)) stop(sprintf("%s paths must be unique.", name), call. = FALSE)
  normalized[order(paths)]
}

#' Create a deterministic submission-package specification
#'
#' @param id Stable package identifier.
#' @param journal_profile Validated journal profile.
#' @param layout_profile Validated publication layout profile.
#' @param figure_style Validated publication figure-style profile.
#' @param report_spec Validated publication report specification.
#' @param archive_format Archive format; currently `zip` or `tar.gz`.
#' @param root_directory Stable archive root directory.
#' @param required_roles Required logical file roles.
#' @param version Specification version.
#' @return A fingerprinted `PopgenVCFPublicationSubmissionPackageSpec`.
#' @export
new_publication_submission_package_spec <- function(
    id, journal_profile, layout_profile, figure_style, report_spec,
    archive_format = c("zip", "tar.gz"), root_directory = "submission",
    required_roles = c("manuscript", "metadata", "provenance"), version = "1.0.0") {
  id <- .publication_submission_scalar(id, "id")
  version <- .publication_submission_scalar(version, "version")
  root_directory <- .publication_submission_scalar(root_directory, "root_directory")
  archive_format <- match.arg(archive_format)
  validate_journal_profile(journal_profile)
  validate_publication_layout_profile(layout_profile, journal_profile)
  validate_publication_figure_style_profile(figure_style)
  validate_publication_report_spec(report_spec)
  required_roles <- sort(unique(as.character(required_roles)))
  if (!length(required_roles) || any(!nzchar(required_roles))) stop("required_roles must be non-empty values.", call. = FALSE)
  spec <- list(
    record_type = "popgenvcf_publication_submission_package_spec",
    schema_version = "1.0.0", id = id, version = version,
    journal_profile_id = journal_profile$id,
    journal_profile_digest = journal_profile$digest,
    layout_fingerprint = layout_profile$fingerprint,
    figure_style_fingerprint = figure_style$fingerprint,
    report_specification_fingerprint = report_spec$fingerprint,
    archive_format = archive_format, root_directory = root_directory,
    required_roles = required_roles
  )
  spec$fingerprint <- .publication_submission_fingerprint(spec)
  class(spec) <- c("PopgenVCFPublicationSubmissionPackageSpec", "list")
  validate_publication_submission_package_spec(spec, journal_profile, layout_profile, figure_style, report_spec)
  spec
}

#' Validate a submission-package specification
#' @param spec A submission-package specification.
#' @param journal_profile Originating journal profile.
#' @param layout_profile Originating layout profile.
#' @param figure_style Originating figure-style profile.
#' @param report_spec Originating report specification.
#' @return `TRUE`, invisibly.
#' @export
validate_publication_submission_package_spec <- function(spec, journal_profile, layout_profile, figure_style, report_spec) {
  if (!inherits(spec, "PopgenVCFPublicationSubmissionPackageSpec")) stop("spec must be a submission-package specification.", call. = FALSE)
  validate_journal_profile(journal_profile)
  validate_publication_layout_profile(layout_profile, journal_profile)
  validate_publication_figure_style_profile(figure_style)
  validate_publication_report_spec(report_spec)
  if (!identical(spec$journal_profile_digest, journal_profile$digest) ||
      !identical(spec$layout_fingerprint, layout_profile$fingerprint) ||
      !identical(spec$figure_style_fingerprint, figure_style$fingerprint) ||
      !identical(spec$report_specification_fingerprint, report_spec$fingerprint)) {
    stop("Submission-package specification is not bound to its publication contracts.", call. = FALSE)
  }
  if (!identical(spec$fingerprint, .publication_submission_fingerprint(spec))) stop("Submission-package specification fingerprint mismatch.", call. = FALSE)
  invisible(TRUE)
}

#' Create a deterministic supplementary-material index
#' @param supplements Supplementary entries with package and manuscript metadata.
#' @return A fingerprinted supplementary-material index.
#' @export
new_publication_supplementary_index <- function(supplements = list()) {
  entries <- .publication_submission_entries(supplements, "supplementary")
  if (length(entries)) {
    required <- c("label", "title", "manuscript_reference")
    for (entry in entries) {
      if (!all(required %in% names(entry))) stop("Supplementary entries require label, title, and manuscript_reference.", call. = FALSE)
      for (field in required) .publication_submission_scalar(entry[[field]], paste("supplementary", field))
    }
    labels <- vapply(entries, `[[`, character(1L), "label")
    if (anyDuplicated(labels)) stop("Supplementary labels must be unique.", call. = FALSE)
    entries <- entries[order(labels)]
  }
  index <- list(record_type = "popgenvcf_publication_supplementary_index", schema_version = "1.0.0", entries = entries)
  index$fingerprint <- .publication_submission_fingerprint(index)
  class(index) <- c("PopgenVCFPublicationSupplementaryIndex", "list")
  validate_publication_supplementary_index(index)
  index
}

#' Validate a supplementary-material index
#' @param index A supplementary-material index.
#' @return `TRUE`, invisibly.
#' @export
validate_publication_supplementary_index <- function(index) {
  if (!inherits(index, "PopgenVCFPublicationSupplementaryIndex")) stop("index must be a supplementary-material index.", call. = FALSE)
  .publication_submission_entries(index$entries, "supplementary")
  if (!identical(index$fingerprint, .publication_submission_fingerprint(index))) stop("Supplementary index fingerprint mismatch.", call. = FALSE)
  invisible(TRUE)
}

#' Create a deterministic submission-package manifest
#' @param spec Validated submission-package specification.
#' @param files Package file entries.
#' @param supplementary_index Validated supplementary-material index.
#' @param output_manifest_fingerprint Fingerprint of the originating report output manifest.
#' @param execution_fingerprint Fingerprint of the originating report execution.
#' @return A fingerprinted package manifest.
#' @export
new_publication_submission_package_manifest <- function(
    spec, files, supplementary_index = new_publication_supplementary_index(),
    output_manifest_fingerprint, execution_fingerprint) {
  if (!inherits(spec, "PopgenVCFPublicationSubmissionPackageSpec")) stop("spec must be a submission-package specification.", call. = FALSE)
  validate_publication_supplementary_index(supplementary_index)
  files <- .publication_submission_entries(files, "package")
  roles <- unique(vapply(files, `[[`, character(1L), "role"))
  missing <- setdiff(spec$required_roles, roles)
  if (length(missing)) stop(sprintf("Submission package is missing required role(s): %s", paste(missing, collapse = ", ")), call. = FALSE)
  output_manifest_fingerprint <- .publication_submission_scalar(output_manifest_fingerprint, "output manifest fingerprint")
  execution_fingerprint <- .publication_submission_scalar(execution_fingerprint, "execution fingerprint")
  supplements <- supplementary_index$entries
  if (length(supplements)) {
    missing_paths <- setdiff(vapply(supplements, `[[`, character(1L), "path"), vapply(files, `[[`, character(1L), "path"))
    if (length(missing_paths)) stop("Supplementary index contains files absent from the package manifest.", call. = FALSE)
  }
  manifest <- list(
    record_type = "popgenvcf_publication_submission_package_manifest",
    schema_version = "1.0.0", specification_fingerprint = spec$fingerprint,
    archive_format = spec$archive_format, root_directory = spec$root_directory,
    files = files, supplementary_index_fingerprint = supplementary_index$fingerprint,
    output_manifest_fingerprint = output_manifest_fingerprint,
    execution_fingerprint = execution_fingerprint
  )
  manifest$fingerprint <- .publication_submission_fingerprint(manifest)
  class(manifest) <- c("PopgenVCFPublicationSubmissionPackageManifest", "list")
  manifest
}

#' Validate a submission-package manifest
#' @param manifest A package manifest.
#' @param spec Originating package specification.
#' @param supplementary_index Originating supplementary index.
#' @return `TRUE`, invisibly.
#' @export
validate_publication_submission_package_manifest <- function(manifest, spec, supplementary_index) {
  if (!inherits(manifest, "PopgenVCFPublicationSubmissionPackageManifest")) stop("manifest must be a submission-package manifest.", call. = FALSE)
  validate_publication_supplementary_index(supplementary_index)
  if (!identical(manifest$specification_fingerprint, spec$fingerprint) ||
      !identical(manifest$supplementary_index_fingerprint, supplementary_index$fingerprint)) {
    stop("Submission-package manifest is not bound to its specification and supplementary index.", call. = FALSE)
  }
  .publication_submission_entries(manifest$files, "package")
  if (!identical(manifest$fingerprint, .publication_submission_fingerprint(manifest))) stop("Submission-package manifest fingerprint mismatch.", call. = FALSE)
  invisible(TRUE)
}

#' Render a deterministic submission-package report
#' @param manifest A validated package manifest.
#' @return Markdown report lines.
#' @export
publication_submission_package_report <- function(manifest) {
  if (!inherits(manifest, "PopgenVCFPublicationSubmissionPackageManifest")) stop("manifest must be a submission-package manifest.", call. = FALSE)
  c(
    "# Submission package", "",
    sprintf("- Archive format: `%s`", manifest$archive_format),
    sprintf("- Root directory: `%s`", manifest$root_directory),
    sprintf("- Files: `%d`", length(manifest$files)),
    sprintf("- Supplementary index: `%s`", manifest$supplementary_index_fingerprint),
    sprintf("- Package fingerprint: `%s`", manifest$fingerprint)
  )
}
