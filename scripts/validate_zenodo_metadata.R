#!/usr/bin/env Rscript

fail <- function(...) stop(..., call. = FALSE)
root <- Sys.getenv("R_METADATA_ROOT", unset = "")
if (!nzchar(root)) {
  args <- commandArgs(trailingOnly = FALSE)
  script_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
  root <- if (length(script_arg)) {
    dirname(dirname(normalizePath(script_arg[[1L]], mustWork = TRUE)))
  } else {
    getwd()
  }
}
root <- normalizePath(root, winslash = "/", mustWork = TRUE)
path_at <- function(...) file.path(root, ...)

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  fail("Package 'jsonlite' is required")
}

files <- c(
  zenodo = path_at(".zenodo.json"),
  identity = path_at("inst", "metadata", "software-identity.json")
)
missing_files <- files[!file.exists(files)]
if (length(missing_files)) {
  fail("Missing required Zenodo metadata files: ", paste(missing_files, collapse = ", "))
}

identity <- jsonlite::read_json(files[["identity"]], simplifyVector = FALSE)
zenodo <- jsonlite::read_json(files[["zenodo"]], simplifyVector = FALSE)
chars <- function(x) as.character(unlist(x, use.names = FALSE))
scalar <- function(x) if (length(x)) chars(x)[[1L]] else NA_character_
same_set <- function(x, y) setequal(sort(unique(chars(x))), sort(unique(chars(y))))

checks <- list()
record_check <- function(id, passed, detail) {
  checks[[length(checks) + 1L]] <<- data.frame(
    id = as.character(id),
    passed = isTRUE(passed),
    detail = as.character(detail),
    stringsAsFactors = FALSE
  )
}

required <- c(
  "title", "upload_type", "description", "creators", "access_right",
  "license", "version", "keywords"
)
record_check(
  "zenodo.required_fields",
  all(required %in% names(zenodo)),
  paste("required fields:", paste(required, collapse = ", "))
)
record_check(
  "zenodo.title",
  identical(scalar(zenodo$title), scalar(identity$citation_title)),
  "title must match canonical citation title"
)
record_check(
  "zenodo.description",
  identical(scalar(zenodo$description), scalar(identity$description)),
  "description must match canonical software identity"
)
record_check(
  "zenodo.upload_type",
  identical(scalar(zenodo$upload_type), "software"),
  "upload_type must be software"
)
record_check(
  "zenodo.access_right",
  identical(scalar(zenodo$access_right), "open"),
  "access_right must be open"
)
record_check(
  "zenodo.license",
  identical(tolower(scalar(zenodo$license)), tolower(scalar(identity$license$spdx))),
  "license must match the canonical MIT identity"
)
record_check(
  "zenodo.version",
  identical(scalar(zenodo$version), scalar(identity$version)),
  "version must match canonical software identity"
)
record_check(
  "zenodo.keywords",
  same_set(zenodo$keywords, identity$keywords),
  "keywords must match canonical software identity"
)

creators <- zenodo$creators
expected_creator <- paste0(
  scalar(identity$author$family_name), ", ", scalar(identity$author$given_name)
)
record_check(
  "zenodo.creators",
  length(creators) >= 1L && identical(scalar(creators[[1L]]$name), expected_creator),
  "first creator must match canonical author in Family, Given format"
)

prohibited <- c(
  "doi", "conceptdoi", "conceptrecid", "recid", "record_id",
  "publication_date", "date_released"
)
record_check(
  "zenodo.development_release_boundary",
  !identical(scalar(identity$release_status), "development") ||
    !any(prohibited %in% names(zenodo)),
  "development Zenodo metadata must not claim DOI, record, or publication-date fields"
)

check_table <- do.call(rbind, checks)
check_table <- check_table[order(check_table$id), , drop = FALSE]
passed <- all(check_table$passed)
evidence <- list(
  schema_version = "1.0",
  record_type = "popgenvcf_zenodo_metadata_validation",
  package = scalar(identity$name),
  version = scalar(identity$version),
  release_status = scalar(identity$release_status),
  passed = passed,
  checks = lapply(seq_len(nrow(check_table)), function(i) as.list(check_table[i, , drop = FALSE]))
)

evidence_path <- Sys.getenv("POPGENVCF_ZENODO_METADATA_EVIDENCE", unset = "")
if (nzchar(evidence_path)) {
  dir.create(dirname(evidence_path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(
    evidence,
    evidence_path,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )
}

if (!passed) {
  failures <- check_table[!check_table$passed, , drop = FALSE]
  fail(
    "Zenodo metadata validation failed:\n",
    paste0("- ", failures$id, ": ", failures$detail, collapse = "\n")
  )
}

cat(
  "Zenodo metadata is valid and DOI-free for ",
  scalar(identity$name), " ", scalar(identity$version), ".\n",
  sep = ""
)
