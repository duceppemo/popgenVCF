#!/usr/bin/env Rscript
if (!requireNamespace("popgenVCF", quietly = TRUE)) {
  stop("Install popgenVCF before running the smoke test.", call. = FALSE)
}
vcf <- system.file("extdata", "tiny.vcf", package = "popgenVCF")
metadata <- system.file("extdata", "tiny_metadata.tsv", package = "popgenVCF")
stopifnot(file.exists(vcf), file.exists(metadata))
cfg <- popgenVCF::default_config()
cfg$input$vcf <- vcf
cfg$input$metadata <- metadata
cfg$output$directory <- tempfile("popgenVCF-smoke-")
cfg$analyses$dapc <- FALSE
cfg$analyses$amova <- FALSE
cfg$analyses$mantel <- FALSE
cfg$analyses$isolation_by_distance <- FALSE
cfg$analyses$chromosome_specific <- FALSE
cfg$analyses$bootstrap$enabled <- FALSE
cfg$report$enabled <- FALSE
cfg$qc$maf <- 0
analysis <- popgenVCF::run_pipeline(cfg)
stopifnot(popgenVCF::is_popgen_vcf_analysis(analysis))
print(summary(analysis))
