#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args) >= 1L) args[[1L]] else "benchmark-release"
baseline_dir <- if (length(args) >= 2L && nzchar(args[[2L]])) args[[2L]] else NA_character_
release_id <- Sys.getenv("POPGENVCF_RELEASE_ID", unset = Sys.getenv("GITHUB_REF_NAME", unset = "development"))
git_sha <- Sys.getenv("GITHUB_SHA", unset = "unknown")
container_digest <- Sys.getenv("POPGENVCF_CONTAINER_DIGEST", unset = NA_character_)
golden_store_dir <- Sys.getenv("POPGENVCF_GOLDEN_STORE", unset = "")

# Dataset tiers exercised by the continuous performance benchmark below.
# "synthetic" is the sole default so ordinary pull-request/tag-push CI stays
# exactly as fast as it is today when this variable is unset. "canonical",
# "medium", and "large" are opt-in (release-benchmark-archive.yml's
# workflow_dispatch benchmark_tiers input) for scheduled/release-time runs,
# per docs/CONTINUOUS_RELEASE_BENCHMARKING.md's stated policy.
requested_tiers <- unique(trimws(strsplit(
  Sys.getenv("POPGENVCF_BENCHMARK_TIERS", unset = "synthetic"), ","
)[[1L]]))
requested_tiers <- tolower(requested_tiers[nzchar(requested_tiers)])
known_tiers <- c("synthetic", "canonical", "medium", "large")
unknown_tiers <- setdiff(requested_tiers, known_tiers)
if (length(unknown_tiers)) {
  stop("Unknown POPGENVCF_BENCHMARK_TIERS value(s): ", paste(unknown_tiers, collapse = ", "), call. = FALSE)
}
if (!length(requested_tiers)) requested_tiers <- "synthetic"

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

# Real, already-approved chromosome 22 acquisition for the "canonical" tier,
# reusing the exact source, acquisition path, and bounded 1Mb region
# (22:20000000-21000000) the production_baseline/external_concordance/
# ancestry_three_backend gates already use -- never a new, unreviewed data
# subset. run_canonical_production_execution()'s normal gate-evidence output
# is written to a throwaway scratch directory; only the verified raw source
# in data_dir is used here. A benchmark run is not a canonical_validation
# gate execution.
canonical_benchmark_dataset <- function(git_sha) {
  bcftools <- Sys.which("bcftools")
  if (!nzchar(bcftools)) {
    stop("bcftools is required for the canonical benchmark tier", call. = FALSE)
  }
  for (module in c(
    "canonical_production_execution.R", "canonical_production_bcftools.R",
    "canonical_production_checksum.R", "canonical_autosomal_baseline.R"
  )) {
    path <- system.file("scripts", module, package = "popgenVCF")
    if (!nzchar(path)) stop("Missing installed helper script: ", module, call. = FALSE)
    sys.source(path, envir = environment())
  }

  work_root <- tempfile("popgenvcf-canonical-benchmark-")
  dir.create(work_root, recursive = TRUE)
  on.exit(unlink(work_root, recursive = TRUE, force = TRUE), add = TRUE)
  data_dir <- file.path(work_root, "source")
  dir.create(data_dir, recursive = TRUE)
  scratch_evidence_dir <- file.path(work_root, "scratch-evidence")

  source <- popgenVCF::canonical_1000g_chr22_source()
  valid_sha <- grepl("^[0-9a-f]{40}$", git_sha)
  run_canonical_production_execution(
    output_dir = scratch_evidence_dir, data_dir = data_dir,
    candidate_id = paste0("benchmark-", if (valid_sha) substr(git_sha, 1L, 12L) else "local"),
    git_commit = if (valid_sha) git_sha else strrep("0", 40L),
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    source = source, allow_download = TRUE, quiet = TRUE,
    # chr22 requires the same bcftools-compatible inspection override
    # run-approved-canonical-validation.R already uses; the default
    # inspection function is chrY-oriented and misclassifies chr22's index.
    inspect = canonical_production_inspect_bcftools_compatible
  )

  files <- source$files$filename
  vcf_name <- files[grepl("\\.vcf\\.gz$", files)]
  panel_name <- files[grepl("\\.panel$", files)]
  if (length(vcf_name) != 1L || length(panel_name) != 1L) {
    stop("chromosome 22 source inventory is ambiguous", call. = FALSE)
  }
  source_vcf <- file.path(data_dir, vcf_name)
  panel_path <- file.path(data_dir, panel_name)
  contract <- canonical_autosomal_baseline_contract()

  derived_vcf <- file.path(work_root, "chr22-benchmark-region.vcf.gz")
  view_args <- c(
    "view", "--regions", shQuote(contract$region), "--min-alleles", "2",
    "--max-alleles", "2", "--types", "snps", "--output-type", "z",
    "--output-file", shQuote(derived_vcf), shQuote(source_vcf)
  )
  canonical_production_system2(bcftools, view_args, "canonical benchmark VCF derivation")

  gds_path <- file.path(work_root, "chr22-benchmark-region.gds")
  SNPRelate::snpgdsVCF2GDS(derived_vcf, gds_path, method = "biallelic.only", verbose = FALSE)
  gds <- SNPRelate::snpgdsOpen(gds_path, readonly = TRUE)
  ids <- popgenVCF:::get_gds_ids(gds)
  geno <- SNPRelate::snpgdsGetGeno(
    gds, sample.id = ids$sample, snp.id = ids$snp, snpfirstdim = FALSE, verbose = FALSE
  )
  SNPRelate::snpgdsClose(gds)
  dimnames(geno) <- list(as.character(ids$sample), as.character(ids$snp))

  panel <- data.table::fread(panel_path, data.table = FALSE, check.names = FALSE)
  panel_metadata <- canonical_autosomal_sample_metadata(panel, ids$sample)

  list(
    genotype = geno, sample_ids = as.character(ids$sample), snp_ids = as.character(ids$snp),
    metadata = data.table::data.table(
      sample = panel_metadata$sample_id, population = panel_metadata$population
    ),
    chromosome = ids$chromosome, position = ids$position
  )
}

# Sizes for "medium"/"large" are larger *synthetic* datasets (per an explicit
# scoping decision -- no medium/large real dataset is registered/approved
# anywhere in this repo, and sourcing one is a data-governance decision, not
# something to invent here), chosen from real local timing measurements:
# synthetic 60x2000 ~0.23s/rep, medium 300x20000 ~1.9s/rep, large
# 1000x100000 ~37s/rep (measured against the actual run_pca/run_ibs/
# compute_diversity/run_fst pipeline functions, GDS creation included).
benchmark_tier_dataset <- function(tier, git_sha) {
  if (identical(tier, "canonical")) return(canonical_benchmark_dataset(git_sha))
  size <- switch(tier,
    synthetic = list(samples = 60L, snps = 2000L),
    medium = list(samples = 300L, snps = 20000L),
    large = list(samples = 1000L, snps = 100000L),
    stop("Unknown dataset tier: ", tier, call. = FALSE)
  )
  geno <- synthetic_genotypes(samples = size$samples, snps = size$snps, seed = 1L)
  sample_ids <- rownames(geno)
  snp_ids <- colnames(geno)
  list(
    genotype = geno, sample_ids = sample_ids, snp_ids = snp_ids,
    metadata = data.table::data.table(
      sample = sample_ids,
      population = rep(c("A", "B", "C"), length.out = length(sample_ids))
    ),
    chromosome = rep(1L, ncol(geno)), position = seq_len(ncol(geno))
  )
}

# Every tier is timed through the exact same runner shape: build a fresh GDS
# from the tier's genotype matrix (so GDS creation overhead is counted
# identically everywhere), then run the actual analysis functions
# run_pipeline() itself calls (PCA, IBS, diversity, FST).
benchmark_tier_spec <- function(tier, dataset) {
  new_performance_benchmark_spec(
    id = "pipeline-core-analyses",
    runner = function(threads) {
      gds_path <- tempfile(fileext = ".gds")
      on.exit(unlink(gds_path), add = TRUE)
      SNPRelate::snpgdsCreateGeno(
        gds_path, genmat = dataset$genotype, sample.id = dataset$sample_ids,
        snp.id = dataset$snp_ids, snp.chromosome = dataset$chromosome,
        snp.position = dataset$position,
        snp.allele = rep("A/G", ncol(dataset$genotype)), snpfirstdim = FALSE
      )
      gds <- SNPRelate::snpgdsOpen(gds_path)
      on.exit(SNPRelate::snpgdsClose(gds), add = TRUE)
      ids <- list(chromosome = dataset$chromosome, position = dataset$position, snp = dataset$snp_ids)
      popgenVCF:::run_pca(gds, dataset$sample_ids, dataset$snp_ids, dataset$metadata, n_pcs = 5L, threads = threads)
      popgenVCF:::run_ibs(gds, dataset$sample_ids, dataset$snp_ids, dataset$metadata, threads = threads)
      popgenVCF:::compute_diversity(gds, dataset$sample_ids, dataset$snp_ids, dataset$metadata, ids)
      popgenVCF:::run_fst(gds, dataset$snp_ids, dataset$metadata)
      invisible(NULL)
    },
    threads = 1L,
    warmup = 1L,
    iterations = 5L,
    gating = FALSE,
    metadata = list(dataset_tier = tier, analyses = "pca,ibs,diversity,fst")
  )
}

performance_by_tier <- stats::setNames(lapply(requested_tiers, function(tier) {
  message("Running '", tier, "' performance benchmark tier")
  run_performance_benchmark(benchmark_tier_spec(tier, benchmark_tier_dataset(tier, git_sha)))
}), requested_tiers)

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

# The release_benchmark_archive.rds trend-history system (distinct from
# continuous_benchmarks.json below) has no dataset_tier concept and is out of
# scope for this change; it keeps tracking exactly one performance result,
# preferring "synthetic" (today's only tier) so its trend history is
# unaffected when only the default tier is requested.
trend_performance <- performance_by_tier[["synthetic"]] %||% performance_by_tier[[1L]]

components <- list(
  scientific_validation = core$checks,
  population_structure_validation = structure_validation$checks,
  performance = trend_performance
)
if (!is.null(golden)) components$golden_outputs <- golden

dataset_descriptions <- as.list(stats::setNames(vapply(requested_tiers, function(tier) {
  if (identical(tier, "canonical")) {
    "1000g_phase3_chr22_v5a:22:20000000-21000000 (real, approved chromosome 22 subset)"
  } else {
    "package-embedded synthetic"
  }
}, character(1L)), requested_tiers))

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
  datasets = dataset_descriptions,
  parameters = list(
    threads = 2L,
    performance_iterations = 5L,
    benchmark_tiers = paste(requested_tiers, collapse = ","),
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
# archive.tar.gz packaging/extraction without any workflow changes. One
# observation (and, when a matching prior exists, one comparison) is built
# per requested dataset_tier.
continuous_status <- "not-run"
git_sha_valid <- grepl("^[0-9a-f]{40}$", git_sha)
if (git_sha_valid) {
  budget <- new_release_performance_budget(id = "pipeline-core-analyses-budget")
  prior_continuous_path <- if (!is.na(baseline_dir) && dir.exists(baseline_dir)) {
    file.path(baseline_dir, "continuous_benchmarks.json")
  } else NA_character_
  prior_payload <- if (!is.na(prior_continuous_path) && file.exists(prior_continuous_path)) {
    jsonlite::read_json(prior_continuous_path, simplifyVector = FALSE)
  } else NULL

  observations <- list()
  continuous_comparisons <- list()
  tier_status <- character()
  for (tier in names(performance_by_tier)) {
    performance <- performance_by_tier[[tier]]
    continuous_summary <- performance$summary[1L]
    observation <- new_continuous_benchmark_observation(
      benchmark_id = "pipeline-core-analyses", module = "pca_ibs_diversity_fst",
      dataset_tier = tier, release = release_id, git_sha = git_sha,
      runtime_seconds = continuous_summary$runtime_median,
      peak_memory_mb = continuous_summary$memory_median_mb,
      throughput = 1 / continuous_summary$runtime_median,
      scaling_efficiency = continuous_summary$scaling_efficiency,
      threads = 1L, repetitions = 5L,
      environment = performance_environment_fingerprint()
    )
    observations[[length(observations) + 1L]] <- observation

    prior_match <- if (!is.null(prior_payload)) Filter(function(o) {
      identical(o$benchmark_id, observation$benchmark_id) &&
        identical(o$module, observation$module) &&
        identical(o$dataset_tier, observation$dataset_tier) &&
        identical(as.integer(o$threads), observation$threads)
    }, prior_payload$observations) else list()

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
        cmp <- compare_continuous_release_benchmark(observation, prior_observation, budget)
        continuous_comparisons[[length(continuous_comparisons) + 1L]] <- cmp
        tier_status[[tier]] <- cmp$status
        next
      }
    }
    tier_status[[tier]] <- "no-baseline"
  }
  write_continuous_benchmark_evidence(
    observations, continuous_comparisons, archive_dir, require_release_ready = FALSE
  )
  status_rank <- c(passed = 1L, `no-baseline` = 2L, `insufficient-evidence` = 3L, failed = 4L)
  continuous_status <- names(status_rank)[max(status_rank[tier_status])]
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
  benchmark_tiers = paste(requested_tiers, collapse = ","),
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
