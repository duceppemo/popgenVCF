#!/usr/bin/env Rscript

fail <- function(...) stop(..., call. = FALSE)
root <- Sys.getenv("R_METADATA_ROOT", unset = "")
if (!nzchar(root)) {
  args <- commandArgs(trailingOnly = FALSE)
  script_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
  root <- if (length(script_arg)) dirname(dirname(normalizePath(script_arg[[1]], mustWork = TRUE))) else getwd()
}
root <- normalizePath(root, winslash = "/", mustWork = TRUE)
path_at <- function(...) file.path(root, ...)
require_file <- function(path) if (!file.exists(path)) fail("Missing required file: ", path)

files <- c(
  description = path_at("DESCRIPTION"),
  citation = path_at("CITATION.cff"),
  codemeta = path_at("codemeta.json"),
  statement = path_at("docs", "reproducibility.md")
)
invisible(lapply(files, require_file))
if (!requireNamespace("jsonlite", quietly = TRUE)) fail("Package 'jsonlite' is required")

description <- read.dcf(files[["description"]])[1, ]
package_name <- unname(description[["Package"]])
package_version <- unname(description[["Version"]])
repository <- "https://github.com/duceppemo/popgenVCF"

citation <- readLines(files[["citation"]], warn = FALSE)
extract_cff_scalar <- function(key) {
  line <- grep(paste0("^", key, ":[[:space:]]*"), citation, value = TRUE)
  if (length(line) != 1L) fail("CITATION.cff must contain exactly one '", key, "' field")
  value <- sub(paste0("^", key, ":[[:space:]]*"), "", line)
  gsub('^"|"$', "", value)
}

if (extract_cff_scalar("version") != package_version) fail("CITATION.cff version disagrees with DESCRIPTION")
if (extract_cff_scalar("repository-code") != repository) fail("CITATION.cff repository-code is incorrect")
if (extract_cff_scalar("license") != "MIT") fail("CITATION.cff license must be MIT")

codemeta <- jsonlite::read_json(files[["codemeta"]], simplifyVector = TRUE)
required <- c("@context", "@type", "name", "version", "codeRepository", "license", "author")
missing <- required[!required %in% names(codemeta)]
if (length(missing)) fail("codemeta.json is missing: ", paste(missing, collapse = ", "))
if (!identical(codemeta$name, package_name)) fail("codemeta.json name disagrees with DESCRIPTION")
if (!identical(codemeta$version, package_version)) fail("codemeta.json version disagrees with DESCRIPTION")
if (!identical(codemeta$codeRepository, repository)) fail("codemeta.json codeRepository is incorrect")
if (!grepl("MIT", codemeta$license, fixed = TRUE)) fail("codemeta.json license must identify MIT")

statement <- paste(readLines(files[["statement"]], warn = FALSE), collapse = "\n")
for (phrase in c("Computational reproducibility", "Scientific boundary", "container digest", "input-file checksums")) {
  if (!grepl(phrase, statement, fixed = TRUE)) fail("Reproducibility statement is missing required concept: ", phrase)
}

cat("Release metadata is valid and synchronized with DESCRIPTION ", package_version, ".\n", sep = "")
