#!/usr/bin/env Rscript

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (!length(script_arg)) stop("Unable to resolve script location", call. = FALSE)
script_path <- normalizePath(sub("^--file=", "", script_arg[[1L]]), mustWork = TRUE)
source_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
module_path <- file.path(source_root, "inst", "scripts", "scientific_review_packet.R")
sys.source(module_path, envir = globalenv())

scientific_review_packet_main(
  commandArgs(trailingOnly = TRUE), source_root = source_root
)
