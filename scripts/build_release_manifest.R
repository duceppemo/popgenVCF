#!/usr/bin/env Rscript

require_release_packages <- function() {
  required <- c("digest", "jsonlite")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("Missing required packages: ", paste(missing, collapse = ", "), call. = FALSE)
  }
}

normalize_relative_path <- function(path, root) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  prefix <- paste0(root, "/")
  if (!startsWith(path, prefix)) {
    stop("Asset is outside the release root: ", path, call. = FALSE)
  }
  substring(path, nchar(prefix) + 1L)
}

collect_release_assets <- function(asset_dir, excluded = c(
  "release-manifest.json",
  "release-SHA256SUMS.txt"
)) {
  require_release_packages()
  if (!dir.exists(asset_dir)) {
    stop("Asset directory does not exist: ", asset_dir, call. = FALSE)
  }

  files <- list.files(
    asset_dir,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  files <- files[file.info(files)$isdir %in% FALSE]
  relative <- vapply(files, normalize_relative_path, character(1), root = asset_dir)
  keep <- !relative %in% excluded
  files <- files[keep]
  relative <- relative[keep]

  if (!length(files)) {
    stop("No release payload assets were found", call. = FALSE)
  }

  order_index <- order(relative, method = "radix")
  files <- files[order_index]
  relative <- relative[order_index]

  data.frame(
    path = relative,
    size_bytes = as.numeric(file.info(files)$size),
    sha256 = vapply(files, digest::digest, character(1), algo = "sha256", file = TRUE),
    stringsAsFactors = FALSE
  )
}

build_release_manifest <- function(
    asset_dir,
    package_name,
    package_version,
    release_id,
    git_tag,
    git_commit,
    r_version,
    workflow_name,
    workflow_run_id,
    workflow_run_attempt,
    created_at = Sys.getenv("POPGENVCF_RELEASE_CREATED_AT", "1970-01-01T00:00:00Z")) {
  assets <- collect_release_assets(asset_dir)

  list(
    schema_version = "1.0",
    record_type = "popgenvcf_release_manifest",
    package = list(name = package_name, version = package_version),
    release = list(id = release_id, git_tag = git_tag, git_commit = git_commit),
    runtime = list(r_version = r_version),
    workflow = list(
      name = workflow_name,
      run_id = workflow_run_id,
      run_attempt = workflow_run_attempt
    ),
    created_at = created_at,
    payload_asset_count = nrow(assets),
    payload_assets = unname(split(assets, seq_len(nrow(assets))))
  )
}

write_release_manifest <- function(manifest, asset_dir) {
  require_release_packages()
  manifest_path <- file.path(asset_dir, "release-manifest.json")
  checksum_path <- file.path(asset_dir, "release-SHA256SUMS.txt")

  jsonlite::write_json(
    manifest,
    manifest_path,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null",
    digits = NA
  )

  assets <- manifest$payload_assets
  checksum_lines <- vapply(assets, function(asset) {
    sprintf("%s  %s", asset$sha256, asset$path)
  }, character(1))
  checksum_lines <- c(
    checksum_lines,
    sprintf(
      "%s  %s",
      digest::digest(manifest_path, algo = "sha256", file = TRUE),
      basename(manifest_path)
    )
  )
  writeLines(checksum_lines, checksum_path, useBytes = TRUE)

  invisible(list(manifest = manifest_path, checksums = checksum_path))
}

verify_release_manifest <- function(asset_dir) {
  require_release_packages()
  manifest_path <- file.path(asset_dir, "release-manifest.json")
  checksum_path <- file.path(asset_dir, "release-SHA256SUMS.txt")
  if (!file.exists(manifest_path) || !file.exists(checksum_path)) {
    stop("Release manifest control records are missing", call. = FALSE)
  }

  manifest <- jsonlite::read_json(manifest_path, simplifyVector = FALSE)
  assets <- manifest$payload_assets
  if (length(assets) != manifest$payload_asset_count) {
    stop("Manifest asset count does not match the payload list", call. = FALSE)
  }

  expected_paths <- vapply(assets, `[[`, character(1), "path")
  if (anyDuplicated(expected_paths)) {
    stop("Manifest contains duplicate asset paths", call. = FALSE)
  }

  for (asset in assets) {
    path <- file.path(asset_dir, asset$path)
    if (!file.exists(path) || isTRUE(file.info(path)$isdir)) {
      stop("Required release asset is missing: ", asset$path, call. = FALSE)
    }
    actual_size <- as.numeric(file.info(path)$size)
    if (!identical(actual_size, as.numeric(asset$size_bytes))) {
      stop("Release asset size mismatch: ", asset$path, call. = FALSE)
    }
    actual_sha <- digest::digest(path, algo = "sha256", file = TRUE)
    if (!identical(actual_sha, asset$sha256)) {
      stop("Release asset checksum mismatch: ", asset$path, call. = FALSE)
    }
  }

  current <- collect_release_assets(asset_dir)
  if (!identical(current$path, expected_paths)) {
    missing <- setdiff(expected_paths, current$path)
    unexpected <- setdiff(current$path, expected_paths)
    stop(
      "Release payload set differs from manifest",
      if (length(missing)) paste0("; missing: ", paste(missing, collapse = ", ")) else "",
      if (length(unexpected)) paste0("; unexpected: ", paste(unexpected, collapse = ", ")) else "",
      call. = FALSE
    )
  }

  checksum_lines <- readLines(checksum_path, warn = FALSE)
  expected_manifest_line <- sprintf(
    "%s  %s",
    digest::digest(manifest_path, algo = "sha256", file = TRUE),
    basename(manifest_path)
  )
  if (!expected_manifest_line %in% checksum_lines) {
    stop("Checksum control file does not authenticate the manifest", call. = FALSE)
  }

  invisible(TRUE)
}

run_tamper_test <- function(asset_dir) {
  manifest <- jsonlite::read_json(
    file.path(asset_dir, "release-manifest.json"),
    simplifyVector = FALSE
  )
  target <- manifest$payload_assets[[1L]]$path
  scratch <- tempfile("popgenvcf-release-tamper-")
  dir.create(scratch, recursive = TRUE)
  on.exit(unlink(scratch, recursive = TRUE, force = TRUE), add = TRUE)

  files <- list.files(asset_dir, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  relative <- vapply(files, normalize_relative_path, character(1), root = asset_dir)
  for (i in seq_along(files)) {
    destination <- file.path(scratch, relative[[i]])
    if (isTRUE(file.info(files[[i]])$isdir)) {
      dir.create(destination, recursive = TRUE, showWarnings = FALSE)
    } else {
      dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
      file.copy(files[[i]], destination, overwrite = TRUE)
    }
  }

  cat("\ntampered\n", file = file.path(scratch, target), append = TRUE)
  detected <- inherits(try(verify_release_manifest(scratch), silent = TRUE), "try-error")
  if (!detected) {
    stop("Tamper test failed: modified payload was accepted", call. = FALSE)
  }
  invisible(TRUE)
}

main <- function(args = commandArgs(trailingOnly = TRUE)) {
  if (length(args) != 10L) {
    stop(
      "Usage: build_release_manifest.R <asset_dir> <package_name> <package_version> ",
      "<release_id> <git_tag> <git_commit> <r_version> <workflow_name> ",
      "<workflow_run_id> <workflow_run_attempt>",
      call. = FALSE
    )
  }

  manifest <- build_release_manifest(
    asset_dir = args[[1L]],
    package_name = args[[2L]],
    package_version = args[[3L]],
    release_id = args[[4L]],
    git_tag = args[[5L]],
    git_commit = args[[6L]],
    r_version = args[[7L]],
    workflow_name = args[[8L]],
    workflow_run_id = args[[9L]],
    workflow_run_attempt = args[[10L]]
  )
  write_release_manifest(manifest, args[[1L]])
  verify_release_manifest(args[[1L]])
  run_tamper_test(args[[1L]])
  cat("Release manifest verified\n")
}

if (sys.nframe() == 0L) {
  main()
}
