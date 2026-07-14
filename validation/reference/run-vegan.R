#!/usr/bin/env Rscript
if (!requireNamespace("vegan", quietly = TRUE)) stop("Install vegan")
args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args)) args[[1L]] else "validation/reports/vegan_mantel.tsv"
paths <- popgenVCF:::validation_fixture_paths()
dosage <- data.table::fread(paths$dosage, na.strings = "NA")
meta <- data.table::fread(paths$metadata)
geno <- as.matrix(dosage[, -1L]); rownames(geno) <- dosage[[1L]]
for (j in seq_len(ncol(geno))) geno[is.na(geno[, j]), j] <- mean(geno[, j], na.rm = TRUE)
genetic <- stats::dist(geno)
geographic <- stats::as.dist(popgenVCF:::haversine_matrix(meta$latitude, meta$longitude, meta$sample))
set.seed(42)
m <- vegan::mantel(genetic, geographic, permutations = 999, method = "pearson")
result <- data.table::data.table(statistic = unname(m$statistic), significance = m$signif,
                                 permutations = m$permutations)
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
data.table::fwrite(result, out, sep = "\t")
writeLines(c(paste0("vegan=", packageVersion("vegan")), paste0("R=", getRversion())),
           paste0(out, ".versions.txt"))
