#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args) >= 1L) args[[1L]] else "benchmark-release"
baseline_dir <- if (length(args) >= 2L && nzchar(args[[2L]])) args[[2L]] else NA_character_
release_id <- Sys.getenv("POPGENVCF_RELEASE_ID", unset = Sys.getenv("GITHUB_REF_NAME", unset = "development"))
git_sha <- Sys.getenv("GITHUB_SHA", unset = "unknown")
container_digest <- Sys.getenv("POPGENVCF_CONTAINER_DIGEST", unset = NA_character_)
golden_store_dir <- Sys.getenv("POPGENVCF_GOLDEN_STORE", unset = "")

suppressPackageStartupMessages(library(popgenVCF))

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
archive_dir <- file.path(output_dir, "archive")
report_dir <- file.path(output_dir, "report")
dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)

archive <- if (!is.na(baseline_dir) && dir.exists(baseline_dir) &&
               file.exists(file.path(baseline_dir, "archive.rds"))) {
  read_benchmark_archive(baseline_dir, verify = TRUE)
} else {
  new_benchmark_archive(metadata = list(project = "popgenVCF", schema = "release-history-v1"))
}

core <- run_scientific_validation(integration = TRUE, threads = 2L)
structure_validation <- run_population_structure_validation(integration = FALSE)

# The performance benchmark exercises the actual analysis functions
# run_pipeline() itself calls (PCA, IBS, diversity, FST) against a
# deterministic synthetic genotype matrix, entirely in-memory and VCF-free
# (no bcftools dependency), so it stays fast enough for ordinary
# pull-request CI while still measuring something real rather than an
# unrelated matrix multiplication.
benchmark_geno <- synthetic_genotypes(samples = 60L, snps = 2000L, seed = 1L)
benchmark_sample_ids <- rownames(benchmark_geno)
benchmark_snp_ids <- colnames(benchmark_geno)
benchmark_population <- rep(c("A", "B", "C"), each = 20L)
benchmark_metadata <- data.table::data.table(
  sample = benchmark_sample_ids, population = benchmark_population
)
benchmark_ids <- list(
  chromosome = rep(1L, ncol(benchmark_geno)),
  position = seq_len(ncol(benchmark_geno)),
  snp = benchmark_snp_ids
)
performance <- run_performance_benchmark(new_performance_benchmark_spec(
  id = "pipeline-core-analyses",
  runner = function(threads) {
    gds_path <- tempfile(fileext = ".gds")
    on.exit(unlink(gds_path), add = TRUE)
    SNPRelate::snpgdsCreateGeno(
      gds_path, genmat = benchmark_geno, sample.id = benchmark_sample_ids,
      snp.id = benchmark_snp_ids, snp.chromosome = benchmark_ids$chromosome,
      snp.position = benchmark_ids$position,
      snp.allele = rep("A/G", ncol(benchmark_geno)), snpfirstdim = FALSE
    )
    gds <- SNPRelate::snpgdsOpen(gds_path)
    on.exit(SNPRelate::snpgdsClose(gds), add = TRUE)
    popgenVCF:::run_pca(gds, benchmark_sample_ids, benchmark_snp_ids, benchmark_metadata, n_pcs = 5L, threads = threads)
    popgenVCF:::run_ibs(gds, benchmark_sample_ids, benchmark_snp_ids, benchmark_metadata, threads = threads)
    popgenVCF:::compute_diversity(gds, benchmark_sample_ids, benchmark_snp_ids, benchmark_metadata, benchmark_ids)
    popgenVCF:::run_fst(gds, benchmark_snp_ids, benchmark_metadata)
    invisible(NULL)
  },
  threads = 1L,
  warmup = 1L,
  iterations = 5L,
  gating = FALSE,
  metadata = list(dataset_tier = "synthetic", analyses = "pca,ibs,diversity,fst")
))

golden <- NULL
if (nzchar(golden_store_dir)) {
  if (!dir.exists(golden_store_dir)) {
    stop("POPGENVCF_GOLDEN_STORE does not exist: ", golden_store_dir, call. = FALSE)
  }
  golden_store <- read_golden_store(golden_store_dir, verify = TRUE)
  golden <- compare_golden_outputs(list(
    scientific_validation = core$checks,
    population_structure_validation = structure_validation$checks
  ), golden_store)
  data.table::fwrite(
    golden_output_table(golden),
    file.path(report_dir, "golden_output_comparison.tsv"),
    sep = "\t"
  )
}

components <- list(
  scientific_validation = core$checks,
  population_structure_validation = structure_validation$checks,
  performance = performance
)
if (!is.null(golden)) components$golden_outputs <- golden

record <- new_release_benchmark_record(
  release = release_id,
  package_version = as.character(utils::packageVersion("popgenVCF")),
  git_sha = git_sha,
  container_digest = container_digest,
  components = components,
  provenance = list(
    workflow = Sys.getenv("GITHUB_WORKFLOW", unset = "local"),
    run_id = Sys.getenv("GITHUB_RUN_ID", unset = NA_character_),
    repository = Sys.getenv("GITHUB_REPOSITORY", unset = NA_character_)
  ),
  environment = performance_environment_fingerprint(),
  datasets = list(synthetic_fixture = "package-embedded"),
  parameters = list(
    threads = 2L,
    performance_iterations = 5L,
    golden_store = if (nzchar(golden_store_dir)) normalizePath(golden_store_dir) else NA_character_
  )
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

# Also produce continuous_benchmarks.{tsv,json,md}: this is the artifact
# shape inst/scripts/scientific_review_packet.R actually looks for
# (scientific_review_find_one(evidence_dir, "continuous_benchmarks.json")),
# distinct from the release_benchmark_archive.rds trend history above. The
# two systems previously ran independently -- CI was green, but never
# produced the evidence the release-review packet expects. Colocating this
# file inside archive_dir lets it round-trip through the existing
# archive.tar.gz packaging/extraction without any workflow changes.
continuous_status <- "not-run"
git_sha_valid <- grepl("^[0-9a-f]{40}$", git_sha)
if (git_sha_valid) {
  continuous_summary <- performance$summary[1L]
  observation <- new_continuous_benchmark_observation(
    benchmark_id = "pipeline-core-analyses", module = "pca_ibs_diversity_fst",
    dataset_tier = "synthetic", release = release_id, git_sha = git_sha,
    runtime_seconds = continuous_summary$runtime_median,
    peak_memory_mb = continuous_summary$memory_median_mb,
    throughput = 1 / continuous_summary$runtime_median,
    scaling_efficiency = continuous_summary$scaling_efficiency,
    threads = 1L, repetitions = 5L,
    environment = performance_environment_fingerprint()
  )
  budget <- new_release_performance_budget(id = "pipeline-core-analyses-budget")
  continuous_comparisons <- list()
  prior_continuous_path <- if (!is.na(baseline_dir) && dir.exists(baseline_dir)) {
    file.path(baseline_dir, "continuous_benchmarks.json")
  } else NA_character_
  if (!is.na(prior_continuous_path) && file.exists(prior_continuous_path)) {
    prior_payload <- jsonlite::read_json(prior_continuous_path, simplifyVector = FALSE)
    prior_match <- Filter(function(o) {
      identical(o$benchmark_id, observation$benchmark_id) &&
        identical(o$module, observation$module) &&
        identical(o$dataset_tier, observation$dataset_tier) &&
        identical(as.integer(o$threads), observation$threads)
    }, prior_payload$observations)
    if (length(prior_match) == 1L) {
      p <- prior_match[[1L]]
      prior_observation <- tryCatch(new_continuous_benchmark_observation(
        benchmark_id = p$benchmark_id, module = p$module, dataset_tier = p$dataset_tier,
        release = p$release, git_sha = p$git_sha, runtime_seconds = p$runtime_seconds,
        peak_memory_mb = p$peak_memory_mb, throughput = p$throughput,
        scaling_efficiency = p$scaling_efficiency, threads = p$threads,
        repetitions = p$repetitions, environment = p$environment
      ), error = function(e) NULL)
      if (!is.null(prior_observation)) {
        continuous_comparisons <- list(compare_continuous_release_benchmark(
          observation, prior_observation, budget
        ))
      }
    }
  }
  write_continuous_benchmark_evidence(
    list(observation), continuous_comparisons, archive_dir, require_release_ready = FALSE
  )
  continuous_status <- if (length(continuous_comparisons)) {
    continuous_comparisons[[1L]]$status
  } else "no-baseline"
} else {
  message("Skipping continuous_benchmarks evidence: git_sha is not a full Git SHA (", git_sha, ")")
}

summary <- data.table::data.table(
  release = release_id,
  git_sha = git_sha,
  scientific_validation_passed = isTRUE(core$passed),
  population_structure_passed = isTRUE(structure_validation$passed),
  golden_output_status = if (is.null(golden)) "not-configured" else golden$status,
  comparison_status = if (is.null(comparison)) "no-baseline" else comparison$status,
  continuous_benchmark_status = continuous_status,
  archive_verified = isTRUE(verify_benchmark_archive(archive_dir))
)
data.table::fwrite(summary, file.path(output_dir, "release_benchmark_summary.tsv"), sep = "\t")

if (!isTRUE(core$passed) || !isTRUE(structure_validation$passed)) {
  stop("scientific release validation failed", call. = FALSE)
}
if (!is.null(golden) && identical(golden$status, "failed")) {
  stop("golden-output regression validation failed", call. = FALSE)
}
if (!is.null(comparison) && identical(comparison$status, "failed")) {
  stop("release regression comparison failed", call. = FALSE)
}
if (identical(continuous_status, "failed")) {
  stop("continuous release benchmark comparison failed", call. = FALSE)
}