require_release_provenance_packages <- function() {
  required <- c("digest", "jsonlite")
  missing <- required[!vapply(required, requireNamespace, logical(1L), quietly = TRUE)]
  if (length(missing)) {
    stop("Missing required packages: ", paste(missing, collapse = ", "), call. = FALSE)
  }
}

normalize_release_asset_path <- function(path, asset_dir, must_work = TRUE) {
  asset_dir <- normalizePath(asset_dir, winslash = "/", mustWork = TRUE)
  candidate <- if (grepl("^(/|[A-Za-z]:[/\\\\])", path)) path else file.path(asset_dir, path)
  candidate <- normalizePath(candidate, winslash = "/", mustWork = must_work)
  prefix <- paste0(asset_dir, "/")
  if (!startsWith(candidate, prefix)) {
    stop("Release provenance input is outside the asset directory: ", path, call. = FALSE)
  }
  substring(candidate, nchar(prefix) + 1L)
}

release_provenance_file_record <- function(path, asset_dir) {
  require_release_provenance_packages()
  relative <- normalize_release_asset_path(path, asset_dir)
  absolute <- file.path(asset_dir, relative)
  if (!file.exists(absolute) || isTRUE(file.info(absolute)$isdir)) {
    stop("Release provenance input is not a regular file: ", relative, call. = FALSE)
  }
  list(
    path = relative,
    size_bytes = as.numeric(file.info(absolute)$size),
    sha256 = digest::digest(absolute, algo = "sha256", file = TRUE)
  )
}

collect_archival_metadata_records <- function(path, asset_dir) {
  relative_root <- normalize_release_asset_path(path, asset_dir)
  absolute_root <- file.path(asset_dir, relative_root)
  if (!dir.exists(absolute_root)) {
    stop("Archival metadata directory does not exist: ", relative_root, call. = FALSE)
  }
  files <- list.files(
    absolute_root,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  files <- files[file.info(files)$isdir %in% FALSE]
  if (!length(files)) stop("Archival metadata directory is empty", call. = FALSE)
  records <- lapply(files, release_provenance_file_record, asset_dir = asset_dir)
  records[order(vapply(records, `[[`, character(1L), "path"), method = "radix")]
}

build_source_release_provenance <- function(
    asset_dir,
    package_name,
    package_version,
    release_id,
    git_tag,
    git_commit,
    workflow_name,
    workflow_run_id,
    workflow_run_attempt,
    source_archive,
    source_sbom,
    archival_metadata_dir,
    created_at = Sys.getenv("POPGENVCF_RELEASE_CREATED_AT", "1970-01-01T00:00:00Z")) {
  require_release_provenance_packages()
  if (!grepl("^[0-9a-f]{40}$", git_commit)) {
    stop("git_commit must be a lowercase 40-character SHA", call. = FALSE)
  }
  if (!nzchar(package_name) || !nzchar(package_version) || !nzchar(release_id) ||
      !nzchar(git_tag) || !nzchar(workflow_name) || !nzchar(workflow_run_id) ||
      !nzchar(workflow_run_attempt) || !nzchar(created_at)) {
    stop("Release provenance identity fields must be non-empty", call. = FALSE)
  }

  subjects <- list(
    release_provenance_file_record(source_archive, asset_dir),
    release_provenance_file_record(source_sbom, asset_dir)
  )
  subjects <- subjects[order(vapply(subjects, `[[`, character(1L), "path"), method = "radix")]

  list(
    schema_version = "1.0",
    record_type = "popgenvcf_source_release_provenance",
    package = list(name = package_name, version = package_version),
    release = list(id = release_id, git_tag = git_tag, git_commit = git_commit),
    builder = list(
      platform = "GitHub Actions",
      workflow = workflow_name,
      run_id = workflow_run_id,
      run_attempt = workflow_run_attempt
    ),
    created_at = created_at,
    subjects = subjects,
    archival_metadata = collect_archival_metadata_records(archival_metadata_dir, asset_dir),
    control_chain = list(
      manifest = "release-manifest.json",
      checksums = "release-SHA256SUMS.txt",
      binding = paste(
        "The release manifest records this provenance file and every payload checksum;",
        "the terminal checksum file authenticates the manifest without recursively hashing itself."
      )
    ),
    container_evidence = list(
      relationship = "separate_release_artifact",
      description = paste(
        "OCI digest, SBOM, and provenance attestations are produced by the container workflow",
        "for the exact release tag and source commit."
      )
    )
  )
}

write_source_release_provenance <- function(provenance, asset_dir) {
  require_release_provenance_packages()
  path <- file.path(asset_dir, "source-release-provenance.json")
  jsonlite::write_json(
    provenance,
    path,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null",
    digits = NA
  )
  invisible(path)
}

verify_source_release_provenance <- function(path, asset_dir) {
  require_release_provenance_packages()
  if (!file.exists(path)) stop("Source-release provenance file is missing", call. = FALSE)
  provenance <- jsonlite::read_json(path, simplifyVector = FALSE)
  if (!identical(provenance$schema_version, "1.0") ||
      !identical(provenance$record_type, "popgenvcf_source_release_provenance")) {
    stop("Source-release provenance schema is unsupported", call. = FALSE)
  }
  records <- c(provenance$subjects, provenance$archival_metadata)
  expected_paths <- vapply(records, `[[`, character(1L), "path")
  if (anyDuplicated(expected_paths)) {
    stop("Source-release provenance contains duplicate paths", call. = FALSE)
  }
  for (record in records) {
    actual <- release_provenance_file_record(record$path, asset_dir)
    if (!identical(actual$size_bytes, as.numeric(record$size_bytes))) {
      stop("Source-release provenance size mismatch: ", record$path, call. = FALSE)
    }
    if (!identical(actual$sha256, record$sha256)) {
      stop("Source-release provenance checksum mismatch: ", record$path, call. = FALSE)
    }
  }
  invisible(TRUE)
}

main <- function(args = commandArgs(trailingOnly = TRUE)) {
  if (length(args) != 12L) {
    stop(
      "Usage: build_release_provenance.R <asset_dir> <package_name> <package_version> ",
      "<release_id> <git_tag> <git_commit> <workflow_name> <workflow_run_id> ",
      "<workflow_run_attempt> <source_archive> <source_sbom> <archival_metadata_dir>",
      call. = FALSE
    )
  }
  provenance <- build_source_release_provenance(
    asset_dir = args[[1L]],
    package_name = args[[2L]],
    package_version = args[[3L]],
    release_id = args[[4L]],
    git_tag = args[[5L]],
    git_commit = args[[6L]],
    workflow_name = args[[7L]],
    workflow_run_id = args[[8L]],
    workflow_run_attempt = args[[9L]],
    source_archive = args[[10L]],
    source_sbom = args[[11L]],
    archival_metadata_dir = args[[12L]]
  )
  path <- write_source_release_provenance(provenance, args[[1L]])
  verify_source_release_provenance(path, args[[1L]])
  cat("Source-release provenance verified\n")
}
