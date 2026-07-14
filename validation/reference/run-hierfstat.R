#!/usr/bin/env Rscript
if (!requireNamespace("hierfstat", quietly = TRUE)) stop("Install hierfstat")
args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args)) args[[1L]] else "validation/reports/hierfstat_pairwise_fst.tsv"
paths <- popgenVCF:::validation_fixture_paths()
dosage <- data.table::fread(paths$dosage, na.strings = "NA")
metadata <- data.table::fread(paths$metadata)
samples <- dosage[[1L]]
geno <- as.data.frame(dosage[, -1L])
encode <- function(x) ifelse(is.na(x), NA_integer_, c(11L, 12L, 22L)[as.integer(x) + 1L])
geno[] <- lapply(geno, encode)
hf <- data.frame(pop = as.integer(factor(metadata$population[match(samples, metadata$sample)])), geno,
                 check.names = FALSE)
fst <- hierfstat::pairwise.WCfst(hf)
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
data.table::fwrite(data.table::as.data.table(fst, keep.rownames = "population"), out, sep = "\t")
writeLines(c(paste0("hierfstat=", packageVersion("hierfstat")),
             paste0("R=", getRversion())), paste0(out, ".versions.txt"))
