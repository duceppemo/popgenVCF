#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args) >= 1L) args[[1L]] else "."
output <- if (length(args) >= 2L) args[[2L]] else file.path(root, "artifacts", "public-api-contract")
baseline <- if (length(args) >= 3L) args[[3L]] else file.path(root, "inst", "api-contract", "public-api-baseline.tsv")

root <- normalizePath(root, winslash = "/", mustWork = TRUE)
source(file.path(root, "R", "public_api_contract.R"), local = .GlobalEnv)

if (!requireNamespace("popgenVCF", quietly = TRUE)) {
  stop("Install or load popgenVCF before running the public API contract check.", call. = FALSE)
}

write_public_api_contract(output, baseline_file = if (file.exists(baseline)) baseline else NULL)
cat("Public API contract evidence written to ", normalizePath(output, winslash = "/", mustWork = TRUE), "\n", sep = "")
