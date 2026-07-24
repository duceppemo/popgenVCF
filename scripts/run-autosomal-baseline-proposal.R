#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 6L) {
  stop(paste(
    "Usage: run-autosomal-baseline-proposal.R",
    "<output-dir> <work-dir> <source-dir> <candidate-id> <git-commit> <generated-at>"
  ), call. = FALSE)
}

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (!length(script_arg)) stop("Unable to resolve script location", call. = FALSE)
script_path <- normalizePath(sub("^--file=", "", script_arg[[1L]]), mustWork = TRUE)
module_dir <- normalizePath(file.path(dirname(script_path), "..", "inst", "scripts"), mustWork = TRUE)
for (module in c(
  "canonical_production_execution.R", "canonical_production_bcftools.R",
  "canonical_production_checksum.R", "canonical_autosomal_baseline.R"
)) sys.source(file.path(module_dir, module), envir = environment())

output_dir <- args[[1L]]
work_dir <- canonical_production_dir(args[[2L]], "work_dir", create = TRUE, empty = TRUE)
source_dir <- canonical_production_dir(args[[3L]], "source_dir")
candidate_id <- canonical_production_scalar(args[[4L]], "candidate_id")
git_commit <- canonical_production_commit(args[[5L]])
generated_at <- canonical_production_timestamp(args[[6L]])
if (dir.exists(output_dir) && length(list.files(output_dir, all.files = TRUE, no.. = TRUE))) {
  stop("baseline proposal output must not contain pre-existing evidence", call. = FALSE)
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_dir <- canonical_production_dir(output_dir, "baseline proposal output", empty = TRUE)
if (canonical_production_is_within(work_dir, output_dir) ||
    canonical_production_is_within(source_dir, output_dir)) {
  stop("raw source and analysis work directories must be outside baseline evidence", call. = FALSE)
}

source <- popgenVCF::canonical_1000g_chr22_source()
verification <- popgenVCF::verify_canonical_source(source, source_dir)
if (!all(verification$passed)) stop("approved chromosome 22 source verification failed", call. = FALSE)
files <- source$files$filename
vcf_name <- files[grepl("\\.vcf\\.gz$", files)]
panel_name <- files[grepl("\\.panel$", files)]
if (length(vcf_name) != 1L || length(panel_name) != 1L) {
  stop("chromosome 22 source inventory is ambiguous", call. = FALSE)
}
source_vcf <- file.path(source_dir, vcf_name)
panel_path <- file.path(source_dir, panel_name)
contract <- canonical_autosomal_baseline_contract()
bcftools <- Sys.which("bcftools")
if (!nzchar(bcftools)) stop("bcftools is required for autosomal baseline derivation", call. = FALSE)
derived_vcf <- file.path(work_dir, "chr22-autosomal-baseline.vcf.gz")
view_args <- c(
  "view", "--regions", shQuote(contract$region), "--min-alleles", "2",
  "--max-alleles", "2", "--types", "snps", "--output-type", "z",
  "--output-file", shQuote(derived_vcf), shQuote(source_vcf)
)
canonical_production_system2(bcftools, view_args, "autosomal baseline VCF derivation")
canonical_production_system2(
  bcftools, c("index", "--tbi", "--force", shQuote(derived_vcf)),
  "autosomal baseline VCF indexing"
)
inventory <- canonical_production_variant_inventory(bcftools, derived_vcf)
if (inventory$variant_count < 100L) {
  stop("autosomal baseline interval retained fewer than 100 biallelic SNPs", call. = FALSE)
}

analysis_dir <- file.path(work_dir, "analysis")
cfg <- popgenVCF::default_config()
cfg$input$vcf <- derived_vcf
cfg$input$metadata <- panel_path
cfg$output$directory <- analysis_dir
cfg$output$figure_formats <- character()
cfg$compute$threads <- 4L
cfg$compute$seed <- contract$seed
cfg$qc$maf <- contract$maf_threshold
cfg$qc$max_variant_missing <- contract$maximum_variant_missing
cfg$qc$max_sample_missing <- contract$maximum_sample_missing
cfg$qc$ld_r2 <- contract$ld_r2
cfg$analyses$n_pcs <- contract$pca_components
cfg$analyses$dapc <- FALSE
cfg$analyses$amova <- FALSE
cfg$analyses$mantel <- FALSE
cfg$analyses$isolation_by_distance <- FALSE
cfg$analyses$chromosome_specific <- FALSE
cfg$analyses$bootstrap$enabled <- FALSE
cfg$report$enabled <- FALSE
analysis <- popgenVCF::run_pipeline(cfg, selected = "pca")

panel <- data.table::fread(panel_path, data.table = FALSE, check.names = FALSE)
version <- canonical_production_system2(bcftools, "--version", "bcftools version query")
subset <- list(
  region = contract$region, variant_filter = contract$variant_filter,
  variant_count = as.integer(inventory$variant_count),
  variant_count_method = inventory$method,
  source_vcf_sha256 = canonical_production_sha256(source_vcf),
  derived_vcf_sha256 = canonical_production_sha256(derived_vcf),
  derived_index_sha256 = canonical_production_sha256(paste0(derived_vcf, ".tbi")),
  command = paste("bcftools", paste(view_args, collapse = " ")),
  bcftools_version = sub("^bcftools[[:space:]]+", "", version[[1L]])
)
result <- write_canonical_autosomal_baseline_proposal(
  analysis = analysis, source = source, source_dir = source_dir,
  panel = panel, subset = subset, output_dir = output_dir,
  candidate_id = candidate_id, git_commit = git_commit,
  generated_at = generated_at
)

cat("Autosomal baseline proposal completed\n")
cat("Dataset:", result$dataset_id, "\n")
cat("Approval:", result$approval, "\n")
for (id in names(result$observations)) cat(id, ":", result$observations[[id]], "\n")
cat("Evidence:", result$output_dir, "\n")
cat("Checksums:", result$checksums, "\n")
