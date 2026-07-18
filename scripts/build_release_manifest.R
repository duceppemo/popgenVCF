#!/usr/bin/env Rscript

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (!length(script_arg)) stop("Unable to resolve script location", call. = FALSE)
script_path <- normalizePath(sub("^--file=", "", script_arg[[1L]]), mustWork = TRUE)
implementation <- normalizePath(
  file.path(dirname(script_path), "..", "inst", "scripts", "build_release_manifest.R"),
  mustWork = TRUE
)
sys.source(implementation, envir = globalenv())
main()
