scientific_release_scalar <- function(x, name, allow_empty = FALSE) {
  x <- trimws(as.character(x))
  if (length(x) != 1L || is.na(x) || (!allow_empty && !nzchar(x))) {
    stop(name, " must be a single non-empty value", call. = FALSE)
  }
  x
}

scientific_release_sha256 <- function(x, name) {
  x <- scientific_release_scalar(x, name)
  x <- sub("^sha256:", "", x, ignore.case = TRUE)
  if (!grepl("^[0-9a-f]{64}$", x)) stop(name, " must be a SHA256 digest", call. = FALSE)
  tolower(x)
}

scientific_release_digest_chain <- function(x) {
  required <- c(
    "analysis_registry", "provenance_dag", "artifact_lineage", "fair_bundle",
    "manuscript", "regeneration_plan", "regeneration_execution",
    "regeneration_verification", "benchmark", "scientific_validation"
  )
  if (is.null(names(x)) || !all(required %in% names(x))) {
    stop("digest_chain must contain: ", paste(required, collapse = ", "), call. = FALSE)
  }
  extra <- setdiff(names(x), required)
  if (length(extra)) stop("digest_chain contains unknown entries: ", paste(extra, collapse = ", "), call. = FALSE)
  out <- vapply(required, function(name) scientific_release_sha256(x[[name]], paste0("digest_chain$", name)), character(1))
  if (anyDuplicated(unname(out))) stop("digest_chain identities must be unique", call. = FALSE)
  out
}

scientific_release_dependencies <- function(x) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  required <- c("package", "version")
  if (!all(required %in% names(x))) stop("dependencies must contain package and version", call. = FALSE)
  x <- x[, required, drop = FALSE]
  x$package <- trimws(as.character(x$package))
  x$version <- trimws(as.character(x$version))
  if (anyNA(x) || any(!nzchar(unlist(x, use.names = FALSE)))) stop("dependency values must be non-empty", call. = FALSE)
  if (anyDuplicated(x$package)) stop("dependencies must contain unique package names", call. = FALSE)
  x <- x[order(x$package), , drop = FALSE]
  rownames(x) <- NULL
  x
}

scientific_release_artifacts <- function(x) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  required <- c("path", "size_bytes", "sha256")
  if (!all(required %in% names(x))) stop("artifacts must contain path, size_bytes, and sha256", call. = FALSE)
  x <- x[, required, drop = FALSE]
  x$path <- gsub("\\\\", "/", trimws(as.character(x$path)))
  x$size_bytes <- suppressWarnings(as.numeric(x$size_bytes))
  x$sha256 <- vapply(seq_len(nrow(x)), function(i) scientific_release_sha256(x$sha256[[i]], paste0("artifacts$sha256[", i, "]")), character(1))
  if (!nrow(x)) stop("artifacts must contain at least one row", call. = FALSE)
  if (any(!nzchar(x$path)) || any(grepl("(^/|(^|/)\\.\\.(/|$))", x$path))) stop("artifact paths must be relative and normalized", call. = FALSE)
  if (anyNA(x$size_bytes) || any(x$size_bytes < 0) || any(x$size_bytes != floor(x$size_bytes))) stop("artifact sizes must be non-negative integers", call. = FALSE)
  if (anyDuplicated(x$path)) stop("artifact paths must be unique", call. = FALSE)
  if (anyDuplicated(x$sha256)) stop("artifact SHA256 identities must be unique", call. = FALSE)
  x <- x[order(x$path), , drop = FALSE]
  rownames(x) <- NULL
  x
}

scientific_release_canonical_evidence <- function(x) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  required <- c("component", "path", "size_bytes", "sha256")
  if (!all(required %in% names(x))) stop("canonical_evidence must contain component, path, size_bytes, and sha256", call. = FALSE)
  x <- x[, required, drop = FALSE]
  x$component <- trimws(as.character(x$component))
  x$path <- gsub("\\\\", "/", trimws(as.character(x$path)))
  x$size_bytes <- suppressWarnings(as.numeric(x$size_bytes))
  x$sha256 <- vapply(seq_len(nrow(x)), function(i) scientific_release_sha256(x$sha256[[i]], paste0("canonical_evidence$sha256[", i, "]")), character(1))
  required_components <- c("validation", "baselines", "drift", "reconciliation", "gate")
  if (!nrow(x)) stop("canonical_evidence must contain evidence rows", call. = FALSE)
  if (!all(required_components %in% x$component)) stop("canonical_evidence must include validation, baselines, drift, reconciliation, and gate evidence", call. = FALSE)
  if (any(!nzchar(x$component)) || any(!nzchar(x$path)) || any(grepl("(^/|(^|/)\\.\\.(/|$))", x$path))) stop("canonical evidence paths must be relative and normalized", call. = FALSE)
  if (anyNA(x$size_bytes) || any(x$size_bytes < 0) || any(x$size_bytes != floor(x$size_bytes))) stop("canonical evidence sizes must be non-negative integers", call. = FALSE)
  if (anyDuplicated(x$path)) stop("canonical evidence paths must be unique", call. = FALSE)
  x <- x[order(match(x$component, required_components), x$path), , drop = FALSE]
  rownames(x) <- NULL
  x
}

scientific_release_certificate <- function(x, release_id, package_version, git_commit) {
  if (!is.list(x) || !identical(x$schema_version, "1.0") || !isTRUE(x$release_ready)) {
    stop("canonical_certificate must be a release-ready schema 1.0 certificate", call. = FALSE)
  }
  if (!identical(scientific_release_scalar(x$release_id, "canonical_certificate$release_id"), release_id)) stop("canonical certificate release identity mismatch", call. = FALSE)
  provenance <- x$provenance
  if (!is.list(provenance) || is.null(names(provenance))) stop("canonical certificate provenance must be named", call. = FALSE)
  required <- c("commit_sha", "package_version", "container_digest", "canonical_dataset_versions", "environment_lockfiles")
  missing <- setdiff(required, names(provenance))
  if (length(missing)) stop("canonical certificate provenance is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  if (!identical(scientific_release_scalar(provenance$commit_sha, "canonical_certificate$provenance$commit_sha"), git_commit)) stop("canonical certificate commit mismatch", call. = FALSE)
  if (!identical(scientific_release_scalar(provenance$package_version, "canonical_certificate$provenance$package_version"), package_version)) stop("canonical certificate package-version mismatch", call. = FALSE)
  scientific_release_sha256(provenance$container_digest, "canonical_certificate$provenance$container_digest")
  if (!length(provenance$canonical_dataset_versions) || !length(provenance$environment_lockfiles)) stop("canonical certificate dataset versions and environment lockfiles must be recorded", call. = FALSE)
  x
}

scientific_release_payload <- function(x) {
  list(
    schema_version = x$schema_version,
    release_id = x$release_id,
    package = x$package,
    git = x$git,
    release_date = x$release_date,
    environment = x$environment,
    dependencies = as.data.frame(x$dependencies, stringsAsFactors = FALSE),
    digest_chain = as.list(x$digest_chain),
    artifacts = as.data.frame(x$artifacts, stringsAsFactors = FALSE),
    canonical_certificate = x$canonical_certificate,
    canonical_evidence = as.data.frame(x$canonical_evidence, stringsAsFactors = FALSE)
  )
}

#' Deterministic scientific release bundles
#'
#' Create, validate, render, and write an immutable root record for a complete
#' reproducible popgenVCF scientific release.
#'
#' @param release_id Stable release identifier.
#' @param package_version Released package version.
#' @param git_commit Full Git commit identity.
#' @param git_tag Release tag.
#' @param release_date Explicit ISO 8601 release date (`YYYY-MM-DD`).
#' @param digest_chain Named SHA256 identities for all required scientific records.
#' @param artifacts Data frame containing release-relative path, size, and SHA256.
#' @param dependencies Data frame containing package and version columns.
#' @param canonical_certificate Release-ready canonical certificate. It defaults
#'   to `NULL` for public-API compatibility but construction fails closed when it
#'   is absent.
#' @param canonical_evidence Data frame of validation, baseline, drift,
#'   reconciliation, and gate evidence files. It defaults to `NULL` for
#'   public-API compatibility but construction fails closed when it is absent.
#' @param git_branch Git branch name.
#' @param git_remote Git remote identity.
#' @param git_dirty Whether the source tree was dirty.
#' @param r_version R version string.
#' @param platform Platform string.
#' @param architecture Architecture string.
#' @param operating_system Operating-system string.
#' @param x A scientific release object or written bundle directory.
#' @param path Output directory.
#' @param overwrite Whether an existing directory may be replaced.
#' @return A validated `PopgenVCFScientificRelease`, canonical table, Markdown
#'   lines, or normalized path, depending on the called function.
#' @name scientific-release-bundle
NULL

#' @rdname scientific-release-bundle
#' @export
new_scientific_release_bundle <- function(
  release_id,
  package_version,
  git_commit,
  git_tag,
  release_date,
  digest_chain,
  artifacts,
  dependencies = data.frame(package = character(), version = character()),
  canonical_certificate = NULL,
  canonical_evidence = NULL,
  git_branch = "",
  git_remote = "",
  git_dirty = FALSE,
  r_version = R.version.string,
  platform = R.version$platform,
  architecture = R.version$arch,
  operating_system = Sys.info()[["sysname"]]
) {
  release_id <- scientific_release_scalar(release_id, "release_id")
  package_version <- scientific_release_scalar(package_version, "package_version")
  git_commit <- scientific_release_scalar(git_commit, "git_commit")
  release_date <- scientific_release_scalar(release_date, "release_date")
  parsed_date <- as.Date(release_date, format = "%Y-%m-%d")
  if (is.na(parsed_date) || format(parsed_date, "%Y-%m-%d") != release_date) stop("release_date must use YYYY-MM-DD", call. = FALSE)
  if (length(git_dirty) != 1L || is.na(git_dirty)) stop("git_dirty must be TRUE or FALSE", call. = FALSE)
  out <- list(
    schema_version = "1.1",
    release_id = release_id,
    package = list(name = "popgenVCF", version = package_version),
    git = list(
      commit = git_commit,
      tag = scientific_release_scalar(git_tag, "git_tag"),
      branch = scientific_release_scalar(git_branch, "git_branch", allow_empty = TRUE),
      remote = scientific_release_scalar(git_remote, "git_remote", allow_empty = TRUE),
      dirty = isTRUE(git_dirty)
    ),
    release_date = release_date,
    environment = list(
      r_version = scientific_release_scalar(r_version, "r_version"),
      platform = scientific_release_scalar(platform, "platform"),
      architecture = scientific_release_scalar(architecture, "architecture", allow_empty = TRUE),
      operating_system = scientific_release_scalar(operating_system, "operating_system")
    ),
    dependencies = data.table::as.data.table(scientific_release_dependencies(dependencies)),
    digest_chain = scientific_release_digest_chain(digest_chain),
    artifacts = data.table::as.data.table(scientific_release_artifacts(artifacts)),
    canonical_certificate = scientific_release_certificate(canonical_certificate, release_id, package_version, git_commit),
    canonical_evidence = data.table::as.data.table(scientific_release_canonical_evidence(canonical_evidence))
  )
  evidence_paths <- out$canonical_evidence$path
  if (!all(evidence_paths %in% out$artifacts$path)) stop("every canonical evidence file must appear in the release artifact manifest", call. = FALSE)
  matched <- match(evidence_paths, out$artifacts$path)
  if (!identical(out$canonical_evidence$sha256, out$artifacts$sha256[matched]) || !identical(out$canonical_evidence$size_bytes, out$artifacts$size_bytes[matched])) stop("canonical evidence checksum or size differs from the release artifact manifest", call. = FALSE)
  out$digest <- digest::digest(scientific_release_payload(out), algo = "sha256", serialize = TRUE)
  out <- structure(out, class = "PopgenVCFScientificRelease")
  validate_scientific_release_bundle(out)
  out
}

#' @rdname scientific-release-bundle
#' @export
scientific_release_bundle_table <- function(x) {
  validate_scientific_release_bundle(x)
  data.table::data.table(
    component = names(x$digest_chain),
    sha256 = unname(x$digest_chain)
  )
}

#' @rdname scientific-release-bundle
#' @export
validate_scientific_release_bundle <- function(x) {
  if (is.character(x) && length(x) == 1L) {
    required <- c("scientific-release.json", "scientific-release.md", "scientific-release.tsv", "scientific-release-manifest.sha256", "canonical-release-certificate.json", "canonical-evidence.tsv")
    missing <- required[!file.exists(file.path(x, required))]
    if (length(missing)) stop("scientific release directory is missing: ", paste(missing, collapse = ", "), call. = FALSE)
    lines <- readLines(file.path(x, "scientific-release-manifest.sha256"), warn = FALSE)
    entries <- strsplit(lines, "  ", fixed = TRUE)
    if (any(lengths(entries) != 2L)) stop("invalid scientific release checksum manifest", call. = FALSE)
    for (entry in entries) {
      actual <- digest::digest(file.path(x, entry[[2L]]), algo = "sha256", file = TRUE)
      if (!identical(actual, entry[[1L]])) stop("scientific release checksum mismatch: ", entry[[2L]], call. = FALSE)
    }
    certificate <- jsonlite::read_json(file.path(x, "canonical-release-certificate.json"), simplifyVector = FALSE)
    if (!isTRUE(certificate$release_ready)) stop("canonical release certificate is not release ready", call. = FALSE)
    return(invisible(TRUE))
  }
  if (!inherits(x, "PopgenVCFScientificRelease")) stop("x must be a PopgenVCFScientificRelease or directory", call. = FALSE)
  if (!identical(x$schema_version, "1.1")) stop("unsupported scientific release schema version", call. = FALSE)
  scientific_release_digest_chain(x$digest_chain)
  scientific_release_dependencies(x$dependencies)
  scientific_release_artifacts(x$artifacts)
  scientific_release_certificate(x$canonical_certificate, x$release_id, x$package$version, x$git$commit)
  scientific_release_canonical_evidence(x$canonical_evidence)
  expected <- digest::digest(scientific_release_payload(x), algo = "sha256", serialize = TRUE)
  if (!identical(expected, x$digest)) stop("scientific release digest mismatch", call. = FALSE)
  invisible(TRUE)
}

#' @rdname scientific-release-bundle
#' @export
render_scientific_release_bundle <- function(x) {
  validate_scientific_release_bundle(x)
  chain <- scientific_release_bundle_table(x)
  rows <- vapply(seq_len(nrow(chain)), function(i) paste0("| ", chain$component[[i]], " | `", chain$sha256[[i]], "` |"), character(1))
  c(
    "# popgenVCF scientific release", "",
    paste0("- Release ID: `", x$release_id, "`"),
    paste0("- Package version: `", x$package$version, "`"),
    paste0("- Git commit: `", x$git$commit, "`"),
    paste0("- Git tag: `", x$git$tag, "`"),
    paste0("- Release date: `", x$release_date, "`"),
    paste0("- Canonical release gate: `", if (isTRUE(x$canonical_certificate$release_ready)) "READY" else "BLOCKED", "`"),
    paste0("- Release digest: `", x$digest, "`"), "",
    "This record binds immutable scientific identities and a release-ready canonical certificate. It does not certify biological interpretation beyond the linked validation records.", "",
    "| Component | SHA256 |", "|---|---|", rows
  )
}

#' @rdname scientific-release-bundle
#' @export
write_scientific_release_bundle <- function(x, path, overwrite = FALSE) {
  validate_scientific_release_bundle(x)
  if (!isTRUE(x$canonical_certificate$release_ready)) stop("refusing to finalize a blocked scientific release", call. = FALSE)
  if (dir.exists(path)) {
    if (!isTRUE(overwrite)) stop("output directory already exists", call. = FALSE)
    unlink(path, recursive = TRUE, force = TRUE)
  }
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(unclass(x), file.path(path, "scientific-release.json"), auto_unbox = TRUE, pretty = TRUE, null = "null")
  writeLines(render_scientific_release_bundle(x), file.path(path, "scientific-release.md"), useBytes = TRUE)
  data.table::fwrite(scientific_release_bundle_table(x), file.path(path, "scientific-release.tsv"), sep = "\t")
  jsonlite::write_json(x$canonical_certificate, file.path(path, "canonical-release-certificate.json"), auto_unbox = TRUE, pretty = TRUE, null = "null")
  data.table::fwrite(x$canonical_evidence, file.path(path, "canonical-evidence.tsv"), sep = "\t")
  files <- c("scientific-release.json", "scientific-release.md", "scientific-release.tsv", "canonical-release-certificate.json", "canonical-evidence.tsv")
  checksums <- vapply(file.path(path, files), digest::digest, character(1), algo = "sha256", file = TRUE)
  writeLines(paste(checksums, files, sep = "  "), file.path(path, "scientific-release-manifest.sha256"), useBytes = TRUE)
  validate_scientific_release_bundle(path)
  invisible(normalizePath(path, winslash = "/", mustWork = TRUE))
}
