#!/usr/bin/env Rscript

`%||%` <- function(x, y) if (is.null(x)) y else x
fail <- function(...) stop(..., call. = FALSE)
root <- Sys.getenv("R_METADATA_ROOT", unset = "")
if (!nzchar(root)) {
  args <- commandArgs(trailingOnly = FALSE)
  script_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
  root <- if (length(script_arg)) dirname(dirname(normalizePath(script_arg[[1L]], mustWork = TRUE))) else getwd()
}
root <- normalizePath(root, winslash = "/", mustWork = TRUE)
path_at <- function(...) file.path(root, ...)

if (!requireNamespace("jsonlite", quietly = TRUE)) fail("Package 'jsonlite' is required")
if (!requireNamespace("yaml", quietly = TRUE)) fail("Package 'yaml' is required")

files <- c(
  description = path_at("DESCRIPTION"),
  identity = path_at("inst", "metadata", "software-identity.json"),
  package_citation = path_at("inst", "CITATION"),
  citation = path_at("CITATION.cff"),
  codemeta = path_at("codemeta.json"),
  statement = path_at("docs", "reproducibility.md")
)
missing_files <- files[!file.exists(files)]
if (length(missing_files)) fail("Missing required metadata files: ", paste(missing_files, collapse = ", "))

checks <- list()
record_check <- function(id, passed, detail) {
  checks[[length(checks) + 1L]] <<- data.frame(
    id = as.character(id), passed = isTRUE(passed), detail = as.character(detail),
    stringsAsFactors = FALSE
  )
  invisible(isTRUE(passed))
}
chars <- function(x) as.character(unlist(x, use.names = FALSE))
scalar <- function(x) if (length(x)) chars(x)[[1L]] else NA_character_
same_set <- function(x, y) setequal(sort(unique(chars(x))), sort(unique(chars(y))))

identity <- jsonlite::read_json(files[["identity"]], simplifyVector = FALSE)
identity_required <- c(
  "schema_version", "name", "title", "citation_title", "version", "citation_year",
  "release_status", "date_released", "doi", "description", "author", "repository",
  "documentation", "issue_tracker", "release_archive", "license", "keywords",
  "programming_language", "runtime_platform", "application_category",
  "development_status", "system_requirements", "optional_system_requirements"
)
record_check(
  "identity.required_fields",
  all(identity_required %in% names(identity)),
  paste("required fields:", paste(identity_required, collapse = ", "))
)
record_check("identity.schema_version", identical(scalar(identity$schema_version), "1.0"), "schema version must be 1.0")
record_check(
  "identity.version_format",
  grepl("^[0-9]+\\.[0-9]+\\.[0-9]+([.-][A-Za-z0-9]+)*$", scalar(identity$version)),
  "version must use semantic release form"
)
record_check(
  "identity.author_roles",
  all(c("aut", "cre") %in% chars(identity$author$roles)),
  "canonical author must include aut and cre"
)
record_check(
  "identity.author_orcid",
  grepl("^[0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9]{3}[0-9X]$", scalar(identity$author$orcid)),
  "canonical author must include a valid ORCID"
)
record_check(
  "identity.license",
  identical(scalar(identity$license$spdx), "MIT") &&
    identical(scalar(identity$license$url), "https://spdx.org/licenses/MIT.html"),
  "canonical license must be MIT"
)
record_check(
  "identity.development_release_boundary",
  !identical(scalar(identity$release_status), "development") ||
    (is.null(identity$date_released) && is.null(identity$doi)),
  "development metadata must not claim a release date or DOI"
)
record_check(
  "identity.keywords",
  length(chars(identity$keywords)) >= 5L && !anyDuplicated(chars(identity$keywords)) &&
    all(nzchar(chars(identity$keywords))),
  "keywords must be unique and non-empty"
)

description <- read.dcf(files[["description"]])[1L, ]
package_name <- unname(description[["Package"]])
package_title <- unname(description[["Title"]])
package_version <- unname(description[["Version"]])
repository <- scalar(identity$repository)
documentation <- scalar(identity$documentation)
issue_tracker <- scalar(identity$issue_tracker)

record_check("description.package", identical(package_name, scalar(identity$name)), "Package must match software identity")
record_check("description.title", identical(package_title, scalar(identity$title)), "Title must match software identity")
record_check("description.version", identical(package_version, scalar(identity$version)), "Version must match software identity")
record_check(
  "description.author",
  identical(unname(description[["Author"]]), "Marc-Olivier Duceppe [aut, cre]") &&
    grepl("Marc-Olivier Duceppe", unname(description[["Maintainer"]]), fixed = TRUE) &&
    grepl(scalar(identity$author$email), unname(description[["Maintainer"]]), fixed = TRUE),
  "Author and Maintainer must match canonical identity"
)
record_check(
  "description.authors_at_r",
  all(vapply(c(
    scalar(identity$author$given_name), scalar(identity$author$family_name),
    scalar(identity$author$email), scalar(identity$author$orcid),
    '"aut"', '"cre"'
  ), grepl, logical(1L), x = unname(description[["Authors@R"]]), fixed = TRUE)),
  "Authors@R must include canonical names, email, ORCID, and roles"
)
description_urls <- trimws(strsplit(unname(description[["URL"]]), ",", fixed = TRUE)[[1L]])
record_check(
  "description.urls",
  all(c(documentation, repository) %in% description_urls),
  "URL must contain documentation and source repository"
)
record_check("description.bug_reports", identical(unname(description[["BugReports"]]), issue_tracker), "BugReports must match issue tracker")
record_check("description.license", grepl("^MIT", unname(description[["License"]])), "DESCRIPTION license must identify MIT")
record_check("description.runtime", grepl("R \\(>= 4\\.3\\)", unname(description[["Depends"]]), perl = TRUE), "R >= 4.3 must be declared")
system_requirements <- unname(description[["SystemRequirements"]])
record_check(
  "description.system_requirements",
  all(vapply(c(chars(identity$system_requirements), chars(identity$optional_system_requirements)),
             grepl, logical(1L), x = system_requirements, fixed = TRUE)),
  "SystemRequirements must include canonical required and optional tools"
)

cff <- yaml::read_yaml(files[["citation"]])
cff_author <- cff$authors[[1L]] %||% list()
record_check("cff.schema", identical(scalar(cff[["cff-version"]]), "1.2.0"), "CFF schema must be 1.2.0")
record_check("cff.type", identical(scalar(cff$type), "software"), "CFF type must be software")
record_check("cff.title", identical(scalar(cff$title), scalar(identity$citation_title)), "CFF title must match canonical citation title")
record_check("cff.version", identical(scalar(cff$version), package_version), "CFF version must match DESCRIPTION")
record_check("cff.repository", identical(scalar(cff[["repository-code"]]), repository), "CFF repository-code must match identity")
record_check("cff.artifact_repository", identical(scalar(cff[["repository-artifact"]]), scalar(identity$release_archive)), "CFF repository-artifact must match release archive")
record_check("cff.url", identical(scalar(cff$url), documentation), "CFF URL must match documentation site")
record_check("cff.license", identical(scalar(cff$license), scalar(identity$license$spdx)), "CFF license must match SPDX identity")
record_check("cff.keywords", same_set(cff$keywords, identity$keywords), "CFF keywords must match canonical identity")
record_check(
  "cff.author",
  identical(scalar(cff_author[["given-names"]]), scalar(identity$author$given_name)) &&
    identical(scalar(cff_author[["family-names"]]), scalar(identity$author$family_name)) &&
    identical(scalar(cff_author$email), scalar(identity$author$email)) &&
    identical(scalar(cff_author$orcid), paste0("https://orcid.org/", scalar(identity$author$orcid))),
  "CFF author must match canonical identity"
)
record_check(
  "cff.development_release_boundary",
  !identical(scalar(identity$release_status), "development") ||
    (is.null(cff[["date-released"]]) && is.null(cff$identifiers)),
  "development CFF must not claim a release date or DOI"
)

codemeta <- jsonlite::read_json(files[["codemeta"]], simplifyVector = FALSE)
cm_author <- codemeta$author[[1L]] %||% list()
cm_maintainer <- codemeta$maintainer %||% list()
record_check("codemeta.context", identical(scalar(codemeta[["@context"]]), "https://doi.org/10.5063/schema/codemeta-2.0"), "CodeMeta context must be 2.0")
record_check("codemeta.type", identical(scalar(codemeta[["@type"]]), "SoftwareSourceCode"), "CodeMeta type must be SoftwareSourceCode")
for (field in c("name", "version", "description")) {
  expected <- scalar(identity[[field]])
  record_check(paste0("codemeta.", field), identical(scalar(codemeta[[field]]), expected), paste(field, "must match identity"))
}
record_check("codemeta.identifier", identical(scalar(codemeta$identifier), repository), "CodeMeta identifier must be repository URL")
record_check("codemeta.repository", identical(scalar(codemeta$codeRepository), repository), "CodeMeta codeRepository must match identity")
record_check("codemeta.url", identical(scalar(codemeta$url), documentation), "CodeMeta URL must match documentation site")
record_check("codemeta.issue_tracker", identical(scalar(codemeta$issueTracker), issue_tracker), "CodeMeta issueTracker must match identity")
record_check("codemeta.download", identical(scalar(codemeta$downloadUrl), scalar(identity$release_archive)), "CodeMeta downloadUrl must match release archive")
record_check("codemeta.license", identical(scalar(codemeta$license), scalar(identity$license$url)), "CodeMeta license must match canonical URL")
record_check("codemeta.language", identical(scalar(codemeta$programmingLanguage), scalar(identity$programming_language)), "CodeMeta language must match identity")
record_check("codemeta.runtime", identical(scalar(codemeta$runtimePlatform), scalar(identity$runtime_platform)), "CodeMeta runtime must match identity")
record_check("codemeta.category", identical(scalar(codemeta$applicationCategory), scalar(identity$application_category)), "CodeMeta category must match identity")
record_check("codemeta.status", identical(scalar(codemeta$developmentStatus), scalar(identity$development_status)), "CodeMeta development status must match identity")
record_check("codemeta.keywords", same_set(codemeta$keywords, identity$keywords), "CodeMeta keywords must match identity")
record_check("codemeta.requirements", same_set(codemeta$softwareRequirements, identity$system_requirements), "CodeMeta requirements must match required tools")
record_check(
  "codemeta.author",
  identical(scalar(cm_author$givenName), scalar(identity$author$given_name)) &&
    identical(scalar(cm_author$familyName), scalar(identity$author$family_name)) &&
    identical(scalar(cm_author$email), scalar(identity$author$email)) &&
    identical(scalar(cm_author[["@id"]]), paste0("https://orcid.org/", scalar(identity$author$orcid))),
  "CodeMeta author must match canonical identity"
)
record_check(
  "codemeta.maintainer",
  identical(scalar(cm_maintainer$givenName), scalar(identity$author$given_name)) &&
    identical(scalar(cm_maintainer$familyName), scalar(identity$author$family_name)) &&
    identical(scalar(cm_maintainer$email), scalar(identity$author$email)) &&
    identical(scalar(cm_maintainer[["@id"]]), paste0("https://orcid.org/", scalar(identity$author$orcid))),
  "CodeMeta maintainer must match canonical identity"
)
record_check(
  "codemeta.development_release_boundary",
  !identical(scalar(identity$release_status), "development") ||
    (is.null(codemeta$datePublished) && is.null(codemeta$doi) && is.null(codemeta$sameAs)),
  "development CodeMeta must not claim a publication date or DOI"
)

package_citation <- paste(readLines(files[["package_citation"]], warn = FALSE), collapse = "\n")
for (required_text in c(
  'packageDescription("popgenVCF")', 'meta$Version', scalar(identity$citation_title),
  scalar(identity$author$given_name), scalar(identity$author$family_name),
  as.character(identity$citation_year), repository
)) {
  record_check(
    paste0("package_citation.", gsub("[^A-Za-z0-9]+", "_", required_text)),
    grepl(required_text, package_citation, fixed = TRUE),
    paste("inst/CITATION must contain", required_text)
  )
}
record_check(
  "package_citation.no_stale_version",
  !grepl("R package version[[:space:]]+[0-9]+\\.[0-9]+\\.[0-9]+", package_citation, perl = TRUE),
  "inst/CITATION must derive the package version dynamically"
)

statement <- paste(readLines(files[["statement"]], warn = FALSE), collapse = "\n")
required_headings <- c(
  "## What the project preserves", "## Minimum reproducibility record",
  "## Determinism and numerical validation", "## Release-evidence boundary",
  "## Input and provenance responsibilities", "## Scientific boundary",
  "## Reporting a reproducibility problem"
)
for (heading in required_headings) {
  record_check(
    paste0("statement.heading.", gsub("[^A-Za-z0-9]+", "_", heading)),
    grepl(heading, statement, fixed = TRUE),
    paste("reproducibility statement must contain", heading)
  )
}
required_concepts <- c(
  "package version or Git commit", "source-package checksum", "container digest",
  "configuration file and checksum", "input-file checksums", "sample identity",
  "random seed", "thread count", "external-tool versions and commands",
  "environment manifest", "validation evidence", "Computational reproducibility",
  "Scientific boundary"
)
for (concept in required_concepts) {
  record_check(
    paste0("statement.concept.", gsub("[^A-Za-z0-9]+", "_", concept)),
    grepl(concept, statement, fixed = TRUE),
    paste("reproducibility statement must contain", concept)
  )
}

check_table <- do.call(rbind, checks)
check_table <- check_table[order(check_table$id), , drop = FALSE]
passed <- all(check_table$passed)
evidence <- list(
  schema_version = "1.0",
  package = package_name,
  version = package_version,
  release_status = scalar(identity$release_status),
  passed = passed,
  checks = lapply(seq_len(nrow(check_table)), function(i) as.list(check_table[i, , drop = FALSE]))
)

evidence_path <- Sys.getenv("POPGENVCF_METADATA_EVIDENCE", unset = "")
if (nzchar(evidence_path)) {
  dir.create(dirname(evidence_path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(evidence, evidence_path, auto_unbox = TRUE, pretty = TRUE, null = "null")
}

if (!passed) {
  failures <- check_table[!check_table$passed, , drop = FALSE]
  fail(
    "Release metadata validation failed:\n",
    paste0("- ", failures$id, ": ", failures$detail, collapse = "\n")
  )
}

cat(
  "Release metadata is valid and synchronized with canonical software identity ",
  package_version, " (", scalar(identity$release_status), ").\n", sep = ""
)
