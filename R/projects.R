project_named_list <- function(x, label) {
  if (!is.list(x) || (length(x) && (is.null(names(x)) || any(!nzchar(names(x)))))) {
    stop(label, " must be a named list", call. = FALSE)
  }
  x
}

project_scalar_string <- function(x, label) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop(label, " must be one non-empty string", call. = FALSE)
  }
  x
}

#' Capture deterministic random-number metadata
#'
#' @param seed Optional integer seed.
#' @param kind RNG kind, normal kind, and sample kind. Defaults to the active R settings.
#' @param streams Optional named list of worker or module seeds.
#' @return A named RNG metadata list.
#' @export
new_project_rng <- function(seed = NA_integer_, kind = RNGkind(), streams = list()) {
  streams <- project_named_list(streams, "streams")
  list(schema_version = "1.0", seed = as.integer(seed)[1L],
       kind = as.character(kind), streams = streams)
}

project_input_record <- function(path, role = "input") {
  path <- project_scalar_string(path, "path")
  exists <- file.exists(path)
  normalized <- if (exists) normalizePath(path, winslash = "/", mustWork = TRUE) else path
  data.table::data.table(
    role = as.character(role)[1L], path = normalized, exists = exists,
    size_bytes = if (exists) unname(file.info(path)$size) else NA_real_,
    sha256 = if (exists) digest::digest(path, algo = "sha256", file = TRUE) else NA_character_
  )
}

project_component_digests <- function(x) {
  if (!length(x)) return(character())
  vapply(x, digest::digest, character(1L), algo = "sha256", serialize = TRUE)
}

# Core project constructor. `new_popgenvcf_project()` (the exported entry
# point) wraps this after normalizing a genuinely empty `inputs` argument.
.project_new_popgenvcf_project <- function(
    name, results = list(), inputs = data.table::data.table(), parameters = list(),
    modules = list(), artifacts = list(), reports = list(), provenance = list(),
    rng = new_project_rng(), project_id = project_uuid(),
    created_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    package_version = tryCatch(as.character(utils::packageVersion("popgenVCF")),
                               error = function(e) NA_character_),
    git_sha = Sys.getenv("GITHUB_SHA", unset = NA_character_)) {
  name <- project_scalar_string(name, "name")
  project_id <- project_scalar_string(project_id, "project_id")
  results <- project_named_list(results, "results")
  parameters <- project_named_list(parameters, "parameters")
  modules <- project_named_list(modules, "modules")
  artifacts <- project_named_list(artifacts, "artifacts")
  reports <- project_named_list(reports, "reports")
  provenance <- project_named_list(provenance, "provenance")
  if (is.character(inputs)) {
    roles <- names(inputs)
    if (is.null(roles)) roles <- rep("input", length(inputs))
    inputs <- data.table::rbindlist(Map(project_input_record, inputs, roles), fill = TRUE)
  }
  inputs <- data.table::as.data.table(inputs)
  required <- c("role", "path", "exists", "size_bytes", "sha256")
  if (!all(required %in% names(inputs))) {
    stop("inputs must contain role, path, exists, size_bytes, and sha256", call. = FALSE)
  }
  if (!is.list(rng) || is.null(rng$schema_version)) stop("rng is invalid", call. = FALSE)
  runtime <- list(r_version = as.character(getRversion()), platform = R.version$platform,
                  os = unname(Sys.info()[["sysname"]]), locale = Sys.getlocale())
  project <- structure(list(
    schema_version = "1.0", project_id = project_id, name = name,
    created_at = as.character(created_at)[1L], package_version = package_version,
    git_sha = git_sha, runtime = runtime, rng = rng, inputs = inputs,
    parameters = parameters, modules = modules, results = results,
    artifacts = artifacts, reports = reports, provenance = provenance,
    component_digests = list(
      parameters = digest::digest(parameters, algo = "sha256", serialize = TRUE),
      modules = digest::digest(modules, algo = "sha256", serialize = TRUE),
      results = project_component_digests(results),
      artifacts = project_component_digests(artifacts),
      reports = project_component_digests(reports)
    )
  ), class = "PopgenVCFProject")
  validate_popgenvcf_project(project)
  project
}

#' Validate a reproducible project
#' @param x A `PopgenVCFProject`.
#' @return `x`, invisibly.
#' @export
validate_popgenvcf_project <- function(x) {
  if (!inherits(x, "PopgenVCFProject")) stop("x must be a PopgenVCFProject", call. = FALSE)
  project_scalar_string(x$project_id, "project_id")
  project_scalar_string(x$name, "name")
  project_named_list(x$results, "results")
  expected <- project_component_digests(x$results)
  if (!identical(expected, x$component_digests$results)) {
    stop("project result digest mismatch", call. = FALSE)
  }
  invisible(x)
}

#' Convert project metadata to stable tables
#' @param x A `PopgenVCFProject` or `PopgenVCFProjectComparison`.
#' @return A data table.
#' @export
project_table <- function(x) {
  if (inherits(x, "PopgenVCFProjectComparison")) return(data.table::copy(x$changes))
  validate_popgenvcf_project(x)
  data.table::data.table(
    project_id = x$project_id, name = x$name, created_at = x$created_at,
    package_version = x$package_version, git_sha = x$git_sha,
    result_count = length(x$results), artifact_count = length(x$artifacts),
    report_count = length(x$reports), input_count = nrow(x$inputs)
  )
}

project_bundle_manifest <- function(root) {
  files <- list.files(root, recursive = TRUE, full.names = TRUE, all.files = TRUE,
                      no.. = TRUE)
  files <- files[basename(files) != "manifest.tsv"]
  relative <- substring(normalizePath(files, winslash = "/"),
                        nchar(normalizePath(root, winslash = "/")) + 2L)
  data.table::data.table(
    path = relative, size_bytes = file.info(files)$size,
    sha256 = vapply(files, digest::digest, character(1L), algo = "sha256", file = TRUE)
  )
}

#' Write a portable `.popgenvcf` project bundle
#'
#' @param project A validated project.
#' @param path Destination bundle path.
#' @param overwrite Permit replacing an existing bundle.
#' @return Normalized bundle path, invisibly.
#' @export
write_popgenvcf_project <- function(project, path, overwrite = FALSE) {
  validate_popgenvcf_project(project)
  if (file.exists(path) && !isTRUE(overwrite)) stop("project bundle already exists", call. = FALSE)
  if (!grepl("\\.popgenvcf$", path, ignore.case = TRUE)) path <- paste0(path, ".popgenvcf")
  root <- tempfile("popgenvcf-project-")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  saveRDS(project, file.path(root, "project.rds"), version = 3)
  data.table::fwrite(project_table(project), file.path(root, "project.tsv"), sep = "\t")
  data.table::fwrite(project$inputs, file.path(root, "inputs.tsv"), sep = "\t")
  result_table <- data.table::data.table(
    id = names(project$results), class = vapply(project$results, function(z) paste(class(z), collapse = ","), character(1L)),
    sha256 = unname(project$component_digests$results)
  )
  data.table::fwrite(result_table, file.path(root, "results.tsv"), sep = "\t")
  jsonlite::write_json(list(
    schema_version = project$schema_version, project_id = project$project_id,
    name = project$name, created_at = project$created_at,
    package_version = project$package_version, git_sha = project$git_sha,
    runtime = project$runtime, rng = project$rng, provenance = project$provenance
  ), file.path(root, "project.json"), auto_unbox = TRUE, pretty = TRUE,
  null = "null", na = "null")
  manifest <- project_bundle_manifest(root)
  data.table::fwrite(manifest, file.path(root, "manifest.tsv"), sep = "\t")
  if (file.exists(path)) unlink(path, force = TRUE)
  old <- setwd(root); on.exit(setwd(old), add = TRUE)
  utils::tar(normalizePath(path, mustWork = FALSE), files = list.files(".", all.files = TRUE,
             no.. = TRUE), compression = "gzip", tar = "internal")
  invisible(normalizePath(path, winslash = "/", mustWork = TRUE))
}

extract_project_bundle <- function(path) {
  if (!file.exists(path)) stop("project bundle does not exist", call. = FALSE)
  root <- tempfile("popgenvcf-project-read-")
  dir.create(root, recursive = TRUE)
  utils::untar(path, exdir = root)
  root
}

#' Verify or reopen a project bundle
#' @param path `.popgenvcf` bundle path.
#' @param verify Verify internal checksums when reading.
#' @return `TRUE` for verification or a validated project for reading.
#' @export
verify_popgenvcf_project <- function(path) {
  root <- extract_project_bundle(path)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  manifest_path <- file.path(root, "manifest.tsv")
  if (!file.exists(manifest_path)) stop("project manifest is missing", call. = FALSE)
  manifest <- data.table::fread(manifest_path)
  for (i in seq_len(nrow(manifest))) {
    file <- file.path(root, manifest$path[[i]])
    if (!file.exists(file)) stop("project file is missing: ", manifest$path[[i]], call. = FALSE)
    actual <- digest::digest(file, algo = "sha256", file = TRUE)
    if (!identical(actual, manifest$sha256[[i]])) {
      stop("project checksum mismatch: ", manifest$path[[i]], call. = FALSE)
    }
  }
  TRUE
}

#' @rdname verify_popgenvcf_project
#' @export
read_popgenvcf_project <- function(path, verify = TRUE) {
  if (isTRUE(verify)) verify_popgenvcf_project(path)
  root <- extract_project_bundle(path)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  project <- readRDS(file.path(root, "project.rds"))
  validate_popgenvcf_project(project)
  project
}

