#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args) >= 1L) args[[1L]] else "."
baseline <- if (length(args) >= 2L) args[[2L]] else file.path(root, "inst", "api-contract", "public-api-baseline.tsv")
metadata <- if (length(args) >= 3L) args[[3L]] else file.path(root, "inst", "api-contract", "public-api-baseline.dcf")

root <- normalizePath(root, winslash = "/", mustWork = TRUE)
source(file.path(root, "R", "public_api_contract.R"), local = .GlobalEnv)

if (!requireNamespace("popgenVCF", quietly = TRUE)) {
  stop("Install popgenVCF before refreshing the public API baseline.", call. = FALSE)
}

version <- as.character(utils::packageVersion("popgenVCF"))
dir.create(dirname(baseline), recursive = TRUE, showWarnings = FALSE)
snapshot <- public_api_contract_snapshot("popgenVCF")
utils::write.table(snapshot, baseline, sep = "\t", quote = FALSE, row.names = FALSE, na = "")
writeLines(c(
  paste0("Package: popgenVCF"),
  paste0("Version: ", version),
  "Contract-Format: 1",
  paste0("Entries: ", nrow(snapshot))
), metadata, useBytes = TRUE)
cat("Updated public API baseline for popgenVCF ", version, " at ", baseline, "\n", sep = "")
