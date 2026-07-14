#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
if (requireNamespace("popgenVCF", quietly = TRUE)) {
  popgenVCF::cli_main(args)
} else {
  root <- normalizePath(dirname(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])), mustWork = TRUE)
  files <- list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE)
  preferred <- c("utils.R", "config.R", "cli.R")
  files <- c(file.path(root, "R", preferred), setdiff(files, file.path(root, "R", preferred)))
  invisible(lapply(files, sys.source, envir = globalenv()))
  cli_main(args)
}
