submission_package_role <- function(path) {
  name <- basename(path)
  if (name == "manuscript.md") return("manuscript_source")
  if (name == "manuscript.rds") return("manuscript_record")
  if (name == "references.bib") return("bibliography")
  if (name == "citation-style.csl") return("citation_style")
  if (grepl("^manuscript\\.(html|docx)$", name)) return("rendered_manuscript")
  if (name == "article.xml") return("jats_article")
  if (grepl("manifest\\.tsv$", name)) return("manifest")
  if (grepl("record\\.json$|profile\\.json$", name)) return("provenance")
  if (grepl("^(figures|tables|supplementary)/", path)) return("asset")
  "supporting_metadata"
}

#' Plan a deterministic manuscript submission package
#'
#' @param manuscript_directory A validated manuscript source directory.
#' @return A stable data table describing included files and destinations.
#' @export
submission_package_plan <- function(manuscript_directory) {
  validate_manuscript(manuscript_directory)
  root <- normalizePath(manuscript_directory, winslash = "/")
  files <- list.files(root, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  files <- files[file.info(files)$isdir %in% FALSE]
  relative <- substring(normalizePath(files, winslash = "/"), nchar(root) + 2L)
  excluded <- grepl("^(submission|submission-package)(/|$)", relative) |
    grepl("(^|/)\\.DS_Store$|(^|/)Thumbs\\.db$", relative)
  files <- files[!excluded]
  relative <- relative[!excluded]
  destination <- file.path("submission", relative)
  plan <- data.table::data.table(
    role = vapply(relative, submission_package_role, character(1L)),
    source = normalizePath(files, winslash = "/"),
    destination = gsub("\\\\", "/", destination),
    size_bytes = unname(file.info(files)$size),
    sha256 = vapply(files, digest::digest, character(1L), algo = "sha256", file = TRUE)
  )
  data.table::setorderv(plan, c("destination", "role"))
  plan
}

submission_copy_file <- function(source, destination) {
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(source, destination, overwrite = TRUE, copy.mode = TRUE)) {
    stop("failed to copy submission package file: ", basename(source), call. = FALSE)
  }
  invisible(destination)
}

#' Write a deterministic manuscript submission package
#'
#' @param manuscript_directory A validated manuscript source directory.
#' @param path Destination `.tar.gz` archive path.
#' @param overwrite Permit replacing an existing archive.
#' @return A `PopgenVCFSubmissionPackage` record, invisibly.
#' @export
write_submission_package <- function(manuscript_directory, path, overwrite = FALSE) {
  validate_manuscript(manuscript_directory)
  if (!grepl("\\.tar\\.gz$", path, ignore.case = TRUE)) path <- paste0(path, ".tar.gz")
  if (file.exists(path) && !isTRUE(overwrite)) stop("submission package already exists", call. = FALSE)
  plan <- submission_package_plan(manuscript_directory)
  stage <- tempfile("popgenvcf-submission-")
  dir.create(stage, recursive = TRUE)
  on.exit(unlink(stage, recursive = TRUE, force = TRUE), add = TRUE)
  for (i in seq_len(nrow(plan))) {
    submission_copy_file(plan$source[[i]], file.path(stage, plan$destination[[i]]))
  }
  copied <- file.path(stage, plan$destination)
  actual <- vapply(copied, digest::digest, character(1L), algo = "sha256", file = TRUE)
  if (!identical(unname(actual), unname(plan$sha256))) stop("submission package copy checksum mismatch", call. = FALSE)
  data.table::fwrite(plan[, .(role, destination, size_bytes, sha256)],
                     file.path(stage, "submission", "submission-manifest.tsv"), sep = "\t")
  manuscript <- readRDS(file.path(manuscript_directory, "manuscript.rds"))
  record <- structure(list(
    schema_version = "1.0",
    profile = "generic-journal-submission",
    project_id = manuscript$project_id,
    project_digest = manuscript$project_digest,
    publication_digest = manuscript$publication_digest,
    file_count = nrow(plan),
    manifest_sha256 = digest::digest(file.path(stage, "submission", "submission-manifest.tsv"),
                                     algo = "sha256", file = TRUE)
  ), class = "PopgenVCFSubmissionPackage")
  jsonlite::write_json(unclass(record), file.path(stage, "submission", "submission-record.json"),
                       pretty = TRUE, auto_unbox = TRUE, null = "null")
  epoch <- as.POSIXct("2000-01-01 00:00:00", tz = "UTC")
  Sys.setFileTime(list.files(stage, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE), epoch)
  target <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (file.exists(target)) unlink(target, force = TRUE)
  old <- setwd(stage); on.exit(setwd(old), add = TRUE)
  utils::tar(target, files = "submission", compression = "gzip", tar = "internal")
  if (!file.exists(target)) stop("submission package archive was not created", call. = FALSE)
  record$archive <- normalizePath(target, winslash = "/")
  record$archive_sha256 <- digest::digest(target, algo = "sha256", file = TRUE)
  verify_submission_package(target)
  invisible(record)
}

#' Verify a manuscript submission package
#'
#' @param path Submission `.tar.gz` archive.
#' @return `TRUE` invisibly, or an error.
#' @export
verify_submission_package <- function(path) {
  if (!file.exists(path)) stop("submission package does not exist", call. = FALSE)
  root <- tempfile("popgenvcf-submission-verify-")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  utils::untar(path, exdir = root)
  package_root <- file.path(root, "submission")
  manifest_path <- file.path(package_root, "submission-manifest.tsv")
  record_path <- file.path(package_root, "submission-record.json")
  if (!file.exists(manifest_path) || !file.exists(record_path)) stop("submission package metadata is missing", call. = FALSE)
  manifest <- data.table::fread(manifest_path)
  required <- c("role", "destination", "size_bytes", "sha256")
  if (!all(required %in% names(manifest))) stop("submission package manifest is invalid", call. = FALSE)
  for (i in seq_len(nrow(manifest))) {
    relative <- sub("^submission/", "", manifest$destination[[i]])
    file <- file.path(package_root, relative)
    if (!file.exists(file)) stop("submission package file is missing: ", relative, call. = FALSE)
    actual <- digest::digest(file, algo = "sha256", file = TRUE)
    if (!identical(actual, manifest$sha256[[i]])) stop("submission package checksum mismatch: ", relative, call. = FALSE)
  }
  invisible(TRUE)
}
