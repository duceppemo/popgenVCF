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
  license = path_at("LICENSE"),
  identity = path_at("inst", "metadata", "software-identity.json")
)
missing_files <- files[!file.exists(files)]
if (length(missing_files)) {
  fail("Missing required license metadata files: ", paste(missing_files, collapse = ", "))
}

identity <- jsonlite::read_json(files[["identity"]], simplifyVector = TRUE)
license <- read.dcf(files[["license"]])
if (nrow(license) != 1L) fail("LICENSE must contain exactly one DCF record")

required_fields <- c("YEAR", "COPYRIGHT HOLDER")
checks <- data.frame(
  id = c("license.required_fields", "license.year", "license.copyright_holder"),
  passed = c(
    all(required_fields %in% colnames(license)),
    all(required_fields %in% colnames(license)) &&
      identical(unname(license[1L, "YEAR"]), as.character(identity$citation_year)),
    all(required_fields %in% colnames(license)) &&
      identical(
        unname(license[1L, "COPYRIGHT HOLDER"]),
        paste(identity$author$given_name, identity$author$family_name)
      )
  ),
  detail = c(
    "LICENSE must contain YEAR and COPYRIGHT HOLDER fields",
    "LICENSE year must match canonical citation_year",
    "LICENSE copyright holder must match the canonical author"
  ),
  stringsAsFactors = FALSE
)
checks <- checks[order(checks$id), , drop = FALSE]
passed <- all(checks$passed)

evidence <- list(
  schema_version = "1.0",
  record_type = "popgenvcf_license_metadata_validation",
  package = identity$name,
  version = identity$version,
  passed = passed,
  checks = lapply(seq_len(nrow(checks)), function(i) as.list(checks[i, , drop = FALSE]))
)

evidence_path <- Sys.getenv("POPGENVCF_LICENSE_METADATA_EVIDENCE", unset = "")
if (nzchar(evidence_path)) {
  dir.create(dirname(evidence_path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(evidence, evidence_path, auto_unbox = TRUE, pretty = TRUE)
}

if (!passed) {
  failures <- checks[!checks$passed, , drop = FALSE]
  fail(
    "LICENSE metadata validation failed:\n",
    paste0("- ", failures$id, ": ", failures$detail, collapse = "\n")
  )
}

cat(
  "LICENSE metadata is valid for ", identity$name, " ", identity$version, ".\n",
  sep = ""
)
