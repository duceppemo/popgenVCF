#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
allow_download <- "--allow-download" %in% args
verbose <- "--verbose" %in% args
source_arg <- grep("^--source-dir=", args, value = TRUE)
if (length(source_arg) > 1L) {
  stop("--source-dir may be supplied at most once", call. = FALSE)
}
source_dir <- if (length(source_arg)) sub("^--source-dir=", "", source_arg[[1L]]) else NULL
positional <- args[!grepl("^--", args)]

if (length(positional) != 5L) {
  stop(
    paste(
      "Usage: run-approved-canonical-validation.R",
      "<output-dir> <data-dir> <candidate-id> <git-commit> <generated-at>",
      "[--source-dir=PATH] [--allow-download] [--verbose]"
    ),
    call. = FALSE
  )
}

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (!length(script_arg)) stop("Unable to resolve script location", call. = FALSE)
script_path <- normalizePath(sub("^--file=", "", script_arg[[1L]]), mustWork = TRUE)
module_path <- normalizePath(
  file.path(dirname(script_path), "..", "inst", "scripts", "canonical_production_execution.R"),
  mustWork = TRUE
)
sys.source(module_path, envir = environment())

result <- run_canonical_production_execution(
  output_dir = positional[[1L]],
  data_dir = positional[[2L]],
  candidate_id = positional[[3L]],
  git_commit = positional[[4L]],
  generated_at = positional[[5L]],
  source_dir = source_dir,
  allow_download = allow_download,
  quiet = !verbose
)

cat("Canonical production execution passed\n")
cat("Dataset:", result$dataset_id, result$dataset_version, "\n")
cat("Samples:", result$sample_count, "\n")
cat("Variants:", format(result$variant_count, scientific = FALSE), "\n")
cat("Evidence:", result$output_dir, "\n")
cat("Gate record:", result$gate_record, "\n")
cat("Checksums:", result$checksums, "\n")
