#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(popgenVCF))
args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args)) args[[1]] else "benchmarks/results/core.tsv"
sizes <- list(c(100L, 10000L), c(500L, 100000L), c(1000L, 1000000L))
quick <- identical(Sys.getenv("POPGENVCF_BENCHMARK_QUICK"), "true")
if (quick) sizes <- sizes[1]
results <- lapply(sizes, function(z) {
  b <- benchmark_stage(sprintf("synthetic_%d_samples_%d_snps", z[1], z[2]),
                       synthetic_genotypes(z[1], z[2], seed = 42L))
  cbind(samples = z[1], snps = z[2], b$metrics)
})
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
data.table::fwrite(data.table::rbindlist(results), out, sep = "\t")
cat("Wrote benchmark results to", out, "\n")
