#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args) >= 1L) args[[1L]] else "benchmark-release"
baseline_dir <- if (length(args) >= 2L && nzchar(args[[2L]])) args[[2L]] else NA_character_
release_id <- Sys.getenv("POPGENVCF_RELEASE_ID", unset = Sys.getenv("GITHUB_REF_NAME", unset = "development"))
git_sha <- Sys.getenv("GITHUB_SHA", unset = "unknown")
container_digest <- Sys.getenv("POPGENVCF_CONTAINER_DIGEST", unset = NA_character_)

suppressPackageStartupMessages(library(popgenVCF))

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
archive_dir <- file.path(output_dir, "archive")
report_dir <- file.path(output_dir, "report")

archive <- if (!is.na(baseline_dir) && dir.exists(baseline_dir) &&
               file.exists(file.path(baseline_dir, "archive.rds"))) {
  read_benchmark_archive(baseline_dir, verify = TRUE)
} else {
  new_benchmark_archive(metadata = list(project = "popgenVCF", schema = "release-history-v1"))
}

core <- run_scientific_validation(integration = TRUE, threads = 2L)
structure_validation <- run_population_structure_validation(integration = FALSE)
performance <- run_performance_benchmark(new_performance_benchmark_spec(
  id = "release-smoke",
  runner = function(threads) {
    x <- matrix(seq_len(40000L), nrow = 200L)
    sum(crossprod(x)) / threads
  },
  threads = 1L,
  warmup = 1L,
  iterations = 5L,
  gating = FALSE,
  metadata = list(profile = "release-smoke")
))

record <- new_release_benchmark_record(
  release = release_id,
  package_version = as.character(utils::packageVersion("popgenVCF")),
  git_sha = git_sha,
  container_digest = container_digest,
  components = list(
    scientific_validation = core$checks,
    population_structure_validation = structure_validation$checks,
    performance = performance
  ),
  provenance = list(
    workflow = Sys.getenv("GITHUB_WORKFLOW", unset = "local"),
    run_id = Sys.getenv("GITHUB_RUN_ID", unset = NA_character_),
    repository = Sys.getenv("GITHUB_REPOSITORY", unset = NA_character_)
  ),
  environment = performance_environment_fingerprint(),
  datasets = list(synthetic_fixture = "package-embedded"),
  parameters = list(threads = 2L, performance_iterations = 5L)
)

comparison <- NULL
if (length(archive$records)) {
  baseline <- latest_release_benchmark(archive, exclude = release_id)
  comparison <- compare_release_benchmarks(record, baseline)
}
if (release_id %in% names(archive$records)) {
  stop("release already exists in archive: ", release_id, call. = FALSE)
}
archive <- register_release_benchmark(archive, record)
write_benchmark_archive(archive, archive_dir, overwrite = TRUE)
write_regression_report(archive, report_dir, comparison = comparison, render = TRUE)

summary <- data.table::data.table(
  release = release_id,
  git_sha = git_sha,
  scientific_validation_passed = isTRUE(core$passed),
  population_structure_passed = isTRUE(structure_validation$passed),
  comparison_status = if (is.null(comparison)) "no-baseline" else comparison$status,
  archive_verified = isTRUE(verify_benchmark_archive(archive_dir))
)
data.table::fwrite(summary, file.path(output_dir, "release_benchmark_summary.tsv"), sep = "\t")

if (!isTRUE(core$passed) || !isTRUE(structure_validation$passed)) {
  stop("scientific release validation failed", call. = FALSE)
}
if (!is.null(comparison) && identical(comparison$status, "failed")) {
  stop("release regression comparison failed", call. = FALSE)
}
