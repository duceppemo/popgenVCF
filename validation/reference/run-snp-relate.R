#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args)) args[[1L]] else "validation/reports/snprelate.tsv"
x <- popgenVCF::run_scientific_validation(integration = TRUE)
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
data.table::fwrite(x$checks, out, sep = "\t")
if (!x$passed) quit(status = 1L)
