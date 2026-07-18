scientific_release_digest_names <- function() c(
  "analysis_registry", "provenance_dag", "artifact_lineage", "fair_bundle", "manuscript",
  "regeneration_plan", "regeneration_execution", "regeneration_verification", "benchmark", "scientific_validation"
)

scientific_release_scalar <- function(x, name, allow_empty = FALSE) {
  x <- trimws(as.character(x))
  if (length(x) != 1L || is.na(x) || (!allow_empty && !nzchar(x))) stop(name, " must be a single non-empty value", call. = FALSE)
  x
}

scientific_release_digests <- function(x) {
  required <- scientific_release_digest_names()
  if (is.null(names(x)) || !all(required %in% names(x))) stop("digests must contain: ", paste(required, collapse = ", "), call. = FALSE)
  x <- x[required]
  x <- vapply(x, scientific_release_scalar, character(1), name = "digest")
  if (any(!grepl("^[[:xdigit:]]{64}$", x))) stop("all release digests must be SHA256 values", call. = FALSE)
  x
}

scientific_release_dependencies <- function(x) {
  if (is.null(x)) return(data.frame(package = character(), version = character(), stringsAsFactors = FALSE))
  if (is.list(x) && !is.data.frame(x)) x <- data.frame(package = names(x), version = unlist(x, use.names = FALSE), stringsAsFactors = FALSE)
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  if (!all(c("package", "version") %in% names(x))) stop("dependencies must contain package and version", call. = FALSE)
  x <- x[, c("package", "version"), drop = FALSE]
  x$package <- trimws(as.character(x$package)); x$version <- trimws(as.character(x$version))
  if (any(!nzchar(x$package)) || any(!nzchar(x$version)) || anyDuplicated(x$package)) stop("dependencies require unique non-empty package names and versions", call. = FALSE)
  x <- x[order(x$package), , drop = FALSE]; rownames(x) <- NULL; x
}

scientific_release_payload <- function(x) list(
  schema_version = x$schema_version, release_id = x$release_id, package_name = x$package_name,
  package_version = x$package_version, git_commit = x$git_commit, git_tag = x$git_tag,
  release_date = x$release_date, r_version = x$r_version, platform = x$platform,
  dependencies = as.data.frame(x$dependencies, stringsAsFactors = FALSE), digests = x$digests
)

#' Create a deterministic scientific release bundle
#'
#' @param release_id Stable release identifier.
#' @param package_version Package version.
#' @param git_commit Git commit SHA.
#' @param git_tag Git tag.
#' @param release_date ISO-8601 release date.
#' @param digests Named SHA256 digest chain.
#' @param dependencies Named versions or a package/version data frame.
#' @param package_name Package name.
#' @param r_version R version string.
#' @param platform Platform string.
#' @return A validated `PopgenVCFScientificRelease`.
#' @export
new_scientific_release_bundle <- function(release_id, package_version, git_commit, git_tag, release_date,
                                          digests, dependencies = NULL, package_name = "popgenVCF",
                                          r_version = R.version.string, platform = R.version$platform) {
  date <- as.Date(release_date)
  if (is.na(date)) stop("release_date must be an ISO-8601 date", call. = FALSE)
  commit <- tolower(scientific_release_scalar(git_commit, "git_commit"))
  if (!grepl("^[[:xdigit:]]{40}$", commit)) stop("git_commit must be a 40-character Git SHA", call. = FALSE)
  out <- list(
    schema_version = "1.0", release_id = scientific_release_scalar(release_id, "release_id"),
    package_name = scientific_release_scalar(package_name, "package_name"),
    package_version = scientific_release_scalar(package_version, "package_version"),
    git_commit = commit, git_tag = scientific_release_scalar(git_tag, "git_tag"),
    release_date = format(date, "%Y-%m-%d"), r_version = scientific_release_scalar(r_version, "r_version"),
    platform = scientific_release_scalar(platform, "platform"),
    dependencies = data.table::as.data.table(scientific_release_dependencies(dependencies)),
    digests = as.list(scientific_release_digests(digests))
  )
  out$digest <- digest::digest(scientific_release_payload(out), algo = "sha256", serialize = TRUE)
  out <- structure(out, class = "PopgenVCFScientificRelease")
  validate_scientific_release_bundle(out)
  out
}

#' Return the scientific release digest-chain table
#' @param x A scientific release bundle.
#' @return A deterministic data table.
#' @export
scientific_release_bundle_table <- function(x) {
  validate_scientific_release_bundle(x)
  data.table::data.table(component = names(x$digests), sha256 = unlist(x$digests, use.names = FALSE))
}

#' Validate a scientific release bundle or written directory
#' @param x A `PopgenVCFScientificRelease` or directory.
#' @return `TRUE` invisibly.
#' @export
validate_scientific_release_bundle <- function(x) {
  if (is.character(x) && length(x) == 1L) {
    required <- c("scientific-release.json", "scientific-release.md", "scientific-release.tsv", "scientific-release.sha256")
    missing <- required[!file.exists(file.path(x, required))]
    if (length(missing)) stop("scientific release directory is missing: ", paste(missing, collapse = ", "), call. = FALSE)
    manifest <- read.table(file.path(x, "scientific-release.sha256"), col.names = c("sha256", "path"), colClasses = "character", stringsAsFactors = FALSE)
    for (i in seq_len(nrow(manifest))) {
      path <- file.path(x, manifest$path[[i]])
      actual <- if (file.exists(path)) digest::digest(path, algo = "sha256", file = TRUE) else ""
      if (!identical(actual, manifest$sha256[[i]])) stop("scientific release checksum mismatch: ", manifest$path[[i]], call. = FALSE)
    }
    return(invisible(TRUE))
  }
  if (!inherits(x, "PopgenVCFScientificRelease")) stop("x must be a PopgenVCFScientificRelease or directory", call. = FALSE)
  scientific_release_digests(x$digests)
  scientific_release_dependencies(x$dependencies)
  expected <- digest::digest(scientific_release_payload(x), algo = "sha256", serialize = TRUE)
  if (!identical(expected, x$digest)) stop("scientific release digest mismatch", call. = FALSE)
  invisible(TRUE)
}

#' Render a scientific release bundle as Markdown
#' @param x A validated scientific release bundle.
#' @return Markdown lines.
#' @export
render_scientific_release_bundle <- function(x) {
  validate_scientific_release_bundle(x)
  rows <- scientific_release_bundle_table(x)
  digest_rows <- vapply(seq_len(nrow(rows)), function(i) paste0("| ", rows$component[[i]], " | `", rows$sha256[[i]], "` |"), character(1))
  deps <- scientific_release_dependencies(x$dependencies)
  dep_rows <- if (nrow(deps)) vapply(seq_len(nrow(deps)), function(i) paste0("| ", deps$package[[i]], " | ", deps$version[[i]], " |"), character(1)) else "| _None recorded_ |  |"
  c("# PopgenVCF scientific release", "",
    paste0("- Release ID: `", x$release_id, "`"), paste0("- Package: `", x$package_name, " ", x$package_version, "`"),
    paste0("- Git commit: `", x$git_commit, "`"), paste0("- Git tag: `", x$git_tag, "`"),
    paste0("- Release date: `", x$release_date, "`"), paste0("- R: `", x$r_version, "`"),
    paste0("- Platform: `", x$platform, "`"), paste0("- Release digest: `", x$digest, "`"), "",
    "## Digest chain", "", "| Component | SHA256 |", "|---|---|", digest_rows, "",
    "## Dependencies", "", "| Package | Version |", "|---|---|", dep_rows)
}

#' Write a deterministic scientific release bundle
#' @param x A validated scientific release bundle.
#' @param path Output directory.
#' @param overwrite Whether an existing directory may be replaced.
#' @return Normalized output path invisibly.
#' @export
write_scientific_release_bundle <- function(x, path, overwrite = FALSE) {
  validate_scientific_release_bundle(x)
  if (dir.exists(path)) {
    if (!isTRUE(overwrite)) stop("output directory already exists", call. = FALSE)
    unlink(path, recursive = TRUE, force = TRUE)
  }
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(unclass(x), file.path(path, "scientific-release.json"), auto_unbox = TRUE, pretty = TRUE, null = "null")
  writeLines(render_scientific_release_bundle(x), file.path(path, "scientific-release.md"), useBytes = TRUE)
  data.table::fwrite(scientific_release_bundle_table(x), file.path(path, "scientific-release.tsv"), sep = "\t")
  files <- c("scientific-release.json", "scientific-release.md", "scientific-release.tsv")
  hashes <- vapply(file.path(path, files), digest::digest, character(1), algo = "sha256", file = TRUE)
  writeLines(paste(hashes, files), file.path(path, "scientific-release.sha256"), useBytes = TRUE)
  validate_scientific_release_bundle(path)
  invisible(normalizePath(path, winslash = "/", mustWork = TRUE))
}
