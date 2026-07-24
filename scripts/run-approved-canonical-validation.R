#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
allow_download <- "--allow-download" %in% args
verbose <- "--verbose" %in% args
source_arg <- grep("^--source-dir=", args, value = TRUE)
dataset_arg <- grep("^--dataset=", args, value = TRUE)
if (length(source_arg) > 1L) {
  stop("--source-dir may be supplied at most once", call. = FALSE)
}
if (length(dataset_arg) > 1L) stop("--dataset may be supplied at most once", call. = FALSE)
source_dir <- if (length(source_arg)) sub("^--source-dir=", "", source_arg[[1L]]) else NULL
dataset <- if (length(dataset_arg)) sub("^--dataset=", "", dataset_arg[[1L]]) else "chrY"
if (!dataset %in% c("chrY", "chr22")) stop("--dataset must be chrY or chr22", call. = FALSE)
positional <- args[!grepl("^--", args)]

if (length(positional) != 5L) {
  stop(
    paste(
      "Usage: run-approved-canonical-validation.R",
      "<output-dir> <data-dir> <candidate-id> <git-commit> <generated-at>",
      "[--dataset=chrY|chr22] [--source-dir=PATH] [--allow-download] [--verbose]"
    ),
    call. = FALSE
  )
}

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (!length(script_arg)) stop("Unable to resolve script location", call. = FALSE)
script_path <- normalizePath(sub("^--file=", "", script_arg[[1L]]), mustWork = TRUE)
module_dir <- normalizePath(
  file.path(dirname(script_path), "..", "inst", "scripts"),
  mustWork = TRUE
)
for (module in c(
  "canonical_production_execution.R",
  "canonical_production_bcftools.R",
  "canonical_production_checksum.R"
)) {
  sys.source(file.path(module_dir, module), envir = environment())
}

result <- run_canonical_production_execution(
  output_dir = positional[[1L]],
  data_dir = positional[[2L]],
  candidate_id = positional[[3L]],
  git_commit = positional[[4L]],
  generated_at = positional[[5L]],
  source = if (identical(dataset, "chr22")) popgenVCF::canonical_1000g_chr22_source() else
    popgenVCF::canonical_1000g_chrY_source(),
  source_dir = source_dir,
  allow_download = allow_download,
  quiet = !verbose,
  inspect = canonical_production_inspect_bcftools_compatible
)

cat("Canonical production execution passed\n")
cat("Dataset:", result$dataset_id, result$dataset_version, "\n")
cat("Samples:", result$sample_count, "\n")
cat("Variants:", format(result$variant_count, scientific = FALSE), "\n")
cat("Evidence:", result$output_dir, "\n")
cat("Gate record:", result$gate_record, "\n")
cat("Checksums:", result$checksums, "\n")
