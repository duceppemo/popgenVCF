archive_scalar_string <- function(x, label) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop(label, " must be one non-empty string", call. = FALSE)
  }
  x
}

archive_named_list <- function(x, label) {
  if (!is.list(x) || (length(x) && (is.null(names(x)) || any(!nzchar(names(x)))))) {
    stop(label, " must be a named list", call. = FALSE)
  }
  x
}

archive_component_table <- function(name, value) {
  if (inherits(value, "PopgenVCFBenchmarkSuite")) {
    tab <- benchmark_suite_table(value)
  } else if (inherits(value, "PopgenVCFPerformanceResult") ||
             inherits(value, "PopgenVCFPerformanceComparison")) {
    tab <- performance_benchmark_table(value)
  } else if (inherits(value, "PopgenVCFExternalReferenceResult") ||
             (is.list(value) && length(value) &&
              all(vapply(value, inherits, logical(1L), "PopgenVCFExternalReferenceResult")))) {
    tab <- external_reference_table(value)
  } else if (is.data.frame(value)) {
    tab <- data.table::as.data.table(value)
  } else {
    tab <- data.table::data.table(
      value_class = paste(class(value), collapse = ","),
      digest = digest::digest(value, algo = "sha256", serialize = TRUE)
    )
  }
  tab <- data.table::as.data.table(tab)
  tab[, component := name]
  data.table::setcolorder(tab, c("component", setdiff(names(tab), "component")))
  tab[]
}

#' Create a release benchmark record
#'
#' @param release Stable release or tag identifier.
#' @param package_version Package version represented by the record.
#' @param git_sha Git commit SHA.
#' @param components Named list of benchmark, validation, performance, or other
#'   scientific result objects.
#' @param provenance,environment,datasets,parameters Named metadata lists.
#' @param container_digest Optional container image digest.
#' @param created_at UTC timestamp.
#' @return A validated `PopgenVCFReleaseBenchmarkRecord`.
#' @export
new_release_benchmark_record <- function(
    release, package_version, git_sha, components,
    provenance = list(), environment = list(), datasets = list(),
    parameters = list(), container_digest = NA_character_,
    created_at = format(Sys.time(), tz = "UTC", usetz = TRUE)) {
  release <- archive_scalar_string(release, "release")
  package_version <- archive_scalar_string(package_version, "package_version")
  git_sha <- archive_scalar_string(git_sha, "git_sha")
  components <- archive_named_list(components, "components")
  if (!length(components)) stop("components must not be empty", call. = FALSE)
  provenance <- archive_named_list(provenance, "provenance")
  environment <- archive_named_list(environment, "environment")
  datasets <- archive_named_list(datasets, "datasets")
  parameters <- archive_named_list(parameters, "parameters")
  component_digests <- vapply(
    components, digest::digest, character(1L), algo = "sha256", serialize = TRUE
  )
  identity <- list(
    release = release, package_version = package_version, git_sha = git_sha,
    container_digest = as.character(container_digest)[1L],
    created_at = as.character(created_at)[1L], component_digests = component_digests,
    provenance = provenance, environment = environment, datasets = datasets,
    parameters = parameters
  )
  x <- structure(list(
    schema_version = "1.0", release = release,
    package_version = package_version, git_sha = git_sha,
    container_digest = as.character(container_digest)[1L],
    created_at = as.character(created_at)[1L], components = components,
    component_digests = component_digests, provenance = provenance,
    environment = environment, datasets = datasets, parameters = parameters,
    record_digest = digest::digest(identity, algo = "sha256", serialize = TRUE)
  ), class = "PopgenVCFReleaseBenchmarkRecord")
  validate_release_benchmark_record(x)
}

#' Validate a release benchmark record
#' @param x A release benchmark record.
#' @return `x`, invisibly.
#' @export
validate_release_benchmark_record <- function(x) {
  if (!inherits(x, "PopgenVCFReleaseBenchmarkRecord")) {
    stop("x must be a PopgenVCFReleaseBenchmarkRecord", call. = FALSE)
  }
  if (!identical(x$schema_version, "1.0")) stop("unsupported release record schema", call. = FALSE)
  archive_scalar_string(x$release, "release")
  archive_scalar_string(x$package_version, "package_version")
  archive_scalar_string(x$git_sha, "git_sha")
  archive_named_list(x$components, "components")
  expected <- vapply(x$components, digest::digest, character(1L), algo = "sha256", serialize = TRUE)
  if (!identical(expected, x$component_digests)) stop("release record component digest mismatch", call. = FALSE)
  invisible(x)
}

#' Create a scientific benchmark archive
#'
#' @param records Optional list of release records.
#' @param metadata Optional archive metadata.
#' @return A `PopgenVCFBenchmarkArchive`.
#' @export
new_benchmark_archive <- function(records = list(), metadata = list()) {
  metadata <- archive_named_list(metadata, "metadata")
  x <- structure(list(schema_version = "1.0", records = list(), metadata = metadata),
                 class = "PopgenVCFBenchmarkArchive")
  for (record in records) x <- register_release_benchmark(x, record)
  x
}

#' Register a release in an archive
#' @param archive A benchmark archive.
#' @param record A release benchmark record.
#' @return Updated archive.
#' @export
register_release_benchmark <- function(archive, record) {
  if (!inherits(archive, "PopgenVCFBenchmarkArchive")) stop("archive is invalid", call. = FALSE)
  validate_release_benchmark_record(record)
  key <- record$release
  if (key %in% names(archive$records)) stop("release already exists in archive: ", key, call. = FALSE)
  archive$records[[key]] <- record
  archive
}

#' Retrieve a release from an archive
#' @param archive A benchmark archive.
#' @param release Release identifier.
#' @return A release benchmark record.
#' @export
get_release_benchmark <- function(archive, release) {
  if (!inherits(archive, "PopgenVCFBenchmarkArchive")) stop("archive is invalid", call. = FALSE)
  release <- archive_scalar_string(release, "release")
  value <- archive$records[[release]]
  if (is.null(value)) stop("release not found in archive: ", release, call. = FALSE)
  value
}

#' Convert archive objects to stable tables
#'
#' @param x A release record or benchmark archive.
#' @return A data table.
#' @export
benchmark_archive_table <- function(x) {
  records <- if (inherits(x, "PopgenVCFReleaseBenchmarkRecord")) list(x) else {
    if (!inherits(x, "PopgenVCFBenchmarkArchive")) stop("x is not an archive object", call. = FALSE)
    x$records
  }
  data.table::rbindlist(lapply(records, function(record) {
    data.table::data.table(
      release = record$release, package_version = record$package_version,
      git_sha = record$git_sha, container_digest = record$container_digest,
      created_at = record$created_at, component_count = length(record$components),
      components = paste(names(record$components), collapse = ","),
      record_digest = record$record_digest
    )
  }), fill = TRUE)
}

release_component_summary <- function(record) {
  data.table::rbindlist(Map(archive_component_table, names(record$components), record$components), fill = TRUE)
}

archive_write_json <- function(x, path) {
  jsonlite::write_json(x, path, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null")
  path
}

#' Write a scientific regression archive
#'
#' @param archive A benchmark archive.
#' @param path Destination directory.
#' @param overwrite Permit replacement of an existing release directory.
#' @return Normalized archive directory, invisibly.
#' @export
write_benchmark_archive <- function(archive, path, overwrite = FALSE) {
  if (!inherits(archive, "PopgenVCFBenchmarkArchive")) stop("archive is invalid", call. = FALSE)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  releases_dir <- file.path(path, "releases")
  dir.create(releases_dir, recursive = TRUE, showWarnings = FALSE)
  manifest_rows <- list()
  for (record in archive$records) {
    validate_release_benchmark_record(record)
    release_dir <- file.path(releases_dir, record$release)
    if (dir.exists(release_dir) && !isTRUE(overwrite)) {
      stop("release directory already exists: ", record$release, call. = FALSE)
    }
    if (dir.exists(release_dir)) unlink(release_dir, recursive = TRUE, force = TRUE)
    dir.create(release_dir, recursive = TRUE)
    files <- c(
      record = file.path(release_dir, "record.rds"),
      summary = file.path(release_dir, "summary.tsv"),
      metadata = file.path(release_dir, "metadata.json")
    )
    saveRDS(record, files[["record"]], version = 3)
    data.table::fwrite(release_component_summary(record), files[["summary"]], sep = "\t")
    archive_write_json(list(
      release = record$release, package_version = record$package_version,
      git_sha = record$git_sha, container_digest = record$container_digest,
      created_at = record$created_at, record_digest = record$record_digest,
      component_digests = as.list(record$component_digests),
      provenance = record$provenance, environment = record$environment,
      datasets = record$datasets, parameters = record$parameters
    ), files[["metadata"]])
    manifest_rows[[record$release]] <- data.table::rbindlist(lapply(names(files), function(kind) {
      file <- files[[kind]]
      data.table::data.table(
        release = record$release, kind = kind,
        path = file.path("releases", record$release, basename(file)),
        size_bytes = file.info(file)$size,
        sha256 = digest::digest(file, algo = "sha256", file = TRUE)
      )
    }))
  }
  manifest <- data.table::rbindlist(manifest_rows, fill = TRUE)
  data.table::fwrite(benchmark_archive_table(archive), file.path(path, "releases.tsv"), sep = "\t")
  data.table::fwrite(manifest, file.path(path, "manifest.tsv"), sep = "\t")
  saveRDS(archive, file.path(path, "archive.rds"), version = 3)
  archive_write_json(archive$metadata, file.path(path, "archive_metadata.json"))
  invisible(normalizePath(path))
}

#' Read and verify a scientific regression archive
#' @param path Archive directory.
#' @param verify Verify manifest checksums.
#' @return A validated benchmark archive.
#' @export
read_benchmark_archive <- function(path, verify = TRUE) {
  archive_file <- file.path(path, "archive.rds")
  if (!file.exists(archive_file)) stop("archive.rds is missing", call. = FALSE)
  archive <- readRDS(archive_file)
  if (!inherits(archive, "PopgenVCFBenchmarkArchive")) stop("invalid archive object", call. = FALSE)
  for (record in archive$records) validate_release_benchmark_record(record)
  if (isTRUE(verify)) verify_benchmark_archive(path)
  archive
}

#' Verify scientific regression archive files
#' @param path Archive directory.
#' @return `TRUE` invisibly when all files match the manifest.
#' @export
verify_benchmark_archive <- function(path) {
  manifest_file <- file.path(path, "manifest.tsv")
  if (!file.exists(manifest_file)) stop("archive manifest is missing", call. = FALSE)
  manifest <- data.table::fread(manifest_file)
  for (i in seq_len(nrow(manifest))) {
    file <- file.path(path, manifest$path[[i]])
    if (!file.exists(file)) stop("archived file is missing: ", manifest$path[[i]], call. = FALSE)
    checksum <- digest::digest(file, algo = "sha256", file = TRUE)
    if (!identical(checksum, manifest$sha256[[i]])) {
      stop("archived file checksum mismatch: ", manifest$path[[i]], call. = FALSE)
    }
  }
  invisible(TRUE)
}
