software_identity_scalar <- function(x, label) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop(label, " must be one non-empty string", call. = FALSE)
  }
  x[[1L]]
}

popgenvcf_software_identity_path <- function() {
  installed <- system.file("metadata", "software-identity.json", package = "popgenVCF")
  if (nzchar(installed) && file.exists(installed)) return(installed)

  source_root <- Sys.getenv("POPGENVCF_SOURCE_ROOT", unset = "")
  candidates <- unique(c(
    if (nzchar(source_root)) file.path(source_root, "inst", "metadata", "software-identity.json"),
    file.path(getwd(), "inst", "metadata", "software-identity.json"),
    file.path(dirname(getwd()), "inst", "metadata", "software-identity.json")
  ))
  matches <- candidates[file.exists(candidates)]
  if (!length(matches)) stop("canonical software identity metadata is unavailable", call. = FALSE)
  normalizePath(matches[[1L]], winslash = "/", mustWork = TRUE)
}

validate_popgenvcf_software_identity <- function(identity) {
  required <- c(
    "schema_version", "name", "title", "citation_title", "version", "citation_year",
    "release_status", "description", "author", "repository", "documentation",
    "issue_tracker", "release_archive", "license", "keywords", "programming_language",
    "runtime_platform", "application_category", "development_status",
    "system_requirements", "optional_system_requirements"
  )
  missing <- setdiff(required, names(identity))
  if (length(missing)) {
    stop("software identity is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  invisible(lapply(c(
    "schema_version", "name", "title", "citation_title", "version", "release_status",
    "description", "repository", "documentation", "issue_tracker", "release_archive",
    "programming_language", "runtime_platform", "application_category", "development_status"
  ), function(field) software_identity_scalar(identity[[field]], field)))

  if (!grepl("^[0-9]+\\.[0-9]+\\.[0-9]+([.-][A-Za-z0-9]+)*$", identity$version)) {
    stop("software identity version is invalid", call. = FALSE)
  }
  if (!identity$release_status %in% c("development", "released")) {
    stop("software identity release_status is invalid", call. = FALSE)
  }
  if (!is.numeric(identity$citation_year) || length(identity$citation_year) != 1L ||
      is.na(identity$citation_year) || identity$citation_year < 2000L) {
    stop("software identity citation_year is invalid", call. = FALSE)
  }

  author_required <- c("given_name", "family_name", "email", "orcid", "roles")
  author_missing <- setdiff(author_required, names(identity$author))
  if (length(author_missing)) {
    stop("software identity author is missing: ", paste(author_missing, collapse = ", "), call. = FALSE)
  }
  invisible(lapply(c("given_name", "family_name", "email", "orcid"), function(field) {
    software_identity_scalar(identity$author[[field]], paste("author", field))
  }))
  if (!grepl("^[0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9]{3}[0-9X]$", identity$author$orcid)) {
    stop("software identity author ORCID is invalid", call. = FALSE)
  }
  if (!all(c("aut", "cre") %in% identity$author$roles)) {
    stop("software identity author must include aut and cre roles", call. = FALSE)
  }

  if (!identical(identity$license$spdx, "MIT") ||
      !identical(identity$license$url, "https://spdx.org/licenses/MIT.html")) {
    stop("software identity license must be canonical MIT metadata", call. = FALSE)
  }
  if (!length(identity$keywords) || anyNA(identity$keywords) || any(!nzchar(identity$keywords)) ||
      anyDuplicated(identity$keywords)) {
    stop("software identity keywords must be unique non-empty strings", call. = FALSE)
  }

  if (identical(identity$release_status, "development")) {
    if (!is.null(identity$date_released) || !is.null(identity$doi)) {
      stop("development software identity must not claim a release date or DOI", call. = FALSE)
    }
  } else {
    software_identity_scalar(identity$date_released, "date_released")
  }

  invisible(identity)
}

popgenvcf_software_identity <- function() {
  identity <- jsonlite::read_json(popgenvcf_software_identity_path(), simplifyVector = TRUE)
  validate_popgenvcf_software_identity(identity)
  identity
}
