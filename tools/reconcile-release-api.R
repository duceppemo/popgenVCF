#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args) >= 1L) args[[1L]] else "."
output_dir <- if (length(args) >= 2L) args[[2L]] else file.path(root, "artifacts", "release-reconciliation")

source(file.path(root, "R", "release_reconciliation.R"), local = globalenv())
audit <- write_release_api_reconciliation(root = root, output_dir = output_dir)

cat(
  sprintf(
    "Release/API reconciliation passed for popgenVCF %s: %d exports, %d S3 methods, %d Rd aliases, %d advisory finding(s).\n",
    audit$version,
    length(audit$exports),
    nrow(audit$s3_methods),
    nrow(audit$aliases),
    sum(audit$findings$severity == "advisory")
  )
)
