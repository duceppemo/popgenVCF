#!/usr/bin/env Rscript
if (!requireNamespace("adegenet", quietly = TRUE)) stop("Install adegenet")
args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args)) args[[1L]] else "validation/reports/adegenet_pca.tsv"
paths <- popgenVCF:::validation_fixture_paths()
dosage <- data.table::fread(paths$dosage, na.strings = "NA")
samples <- dosage[[1L]]
geno <- as.matrix(dosage[, -1L]); rownames(geno) <- samples
# Mean imputation mirrors common genotype-PCA preprocessing for this reference.
for (j in seq_len(ncol(geno))) geno[is.na(geno[, j]), j] <- mean(geno[, j], na.rm = TRUE)
gl <- adegenet::as.genlight(geno)
adegenet::indNames(gl) <- samples
pca <- adegenet::glPca(gl, nf = min(5L, nrow(geno) - 1L), parallel = FALSE)
result <- data.table::as.data.table(pca$scores, keep.rownames = "sample")
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
data.table::fwrite(result, out, sep = "\t")
data.table::fwrite(data.table::data.table(component = seq_along(pca$eig), eigenvalue = pca$eig),
                   sub("\\.tsv$", "_eigenvalues.tsv", out), sep = "\t")
writeLines(c(paste0("adegenet=", packageVersion("adegenet")),
             paste0("R=", getRversion())), paste0(out, ".versions.txt"))
