#!/usr/bin/env Rscript
if (!requireNamespace("yaml", quietly = TRUE) || !requireNamespace("digest", quietly = TRUE)) {
  stop("yaml and digest are required")
}
manifest <- yaml::read_yaml("validation/datasets.yml")
outdir <- "validation/data"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
for (name in names(manifest$datasets)) {
  x <- manifest$datasets[[name]]
  if (!isTRUE(x$enabled) || isTRUE(x$bundled)) next
  if (is.null(x$download_url) || !nzchar(x$download_url)) {
    message("Skipping ", name, ": no download_url configured")
    next
  }
  if (is.null(x$license) || !nzchar(x$license)) stop("Dataset ", name, " lacks a license declaration")
  destination <- file.path(outdir, basename(x$download_url))
  utils::download.file(x$download_url, destination, mode = "wb", quiet = FALSE)
  observed <- digest::digest(file = destination, algo = "sha256")
  if (is.null(x$sha256) || !identical(tolower(observed), tolower(x$sha256))) {
    unlink(destination)
    stop("Checksum mismatch for ", name)
  }
  message("Validated ", name, " -> ", destination)
}
