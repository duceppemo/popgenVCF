#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
source_dir <- if (length(args) >= 1L) args[[1L]] else Sys.getenv("POPGENVCF_CANONICAL_SOURCE_DIR")
output_dir <- if (length(args) >= 2L) args[[2L]] else "canonical-validation-evidence"

if (!nzchar(source_dir) || !dir.exists(source_dir)) {
  stop("Provide the staged canonical source directory as argument 1 or POPGENVCF_CANONICAL_SOURCE_DIR", call. = FALSE)
}

source <- popgenVCF::canonical_1000g_chrY_source()
verification <- popgenVCF::verify_canonical_source(source, source_dir)
if (!all(verification$passed)) {
  print(verification)
  stop("approved canonical source verification failed", call. = FALSE)
}

paths <- popgenVCF::write_approved_canonical_source_evidence(source, source_dir, output_dir)
cat("Approved canonical dataset verified and promoted to SHA-256.\n")
print(paths)
