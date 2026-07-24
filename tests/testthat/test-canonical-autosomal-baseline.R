canonical_autosomal_test_analysis <- function() {
  root <- tempfile("canonical-autosomal-analysis-")
  dir.create(file.path(root, "tables"), recursive = TRUE)
  required <- c(
    "analysis_execution_ledger.tsv", "analysis_execution_plan.tsv",
    "analysis_summary.tsv", "analysis_validation.tsv", "pipeline.log",
    file.path("tables", "01_sample_QC.tsv"),
    file.path("tables", "02_sample_metadata_match.tsv"),
    file.path("tables", "06_QC_independent_counts.tsv"),
    file.path("tables", "07_QC_sequential_counts.tsv"),
    file.path("tables", "12_PCA_scores.tsv"),
    file.path("tables", "13_PCA_variance.tsv")
  )
  for (path in file.path(root, required)) writeLines("fixture", path)

  cfg <- popgenVCF::default_config()
  cfg$compute$threads <- 2L
  analysis <- popgenVCF::new_popgen_vcf_analysis(cfg, dirs = list(root = root))
  analysis$samples$ids <- c("S1", "S2")
  analysis$samples$metadata <- data.frame(
    sample = c("S1", "S2"), population = c("P1", "P2"),
    stringsAsFactors = FALSE
  )
  analysis$variants$qc_ids <- seq_len(80L)
  analysis$variants$ld_ids <- seq_len(20L)
  analysis <- popgenVCF::set_analysis_result(analysis, "pca", list(
    scores = data.frame(sample = c("S1", "S2"), PC1 = c(-1, 1), PC2 = c(0, 0)),
    variance = data.frame(component = c("PC1", "PC2"), proportion = c(0.60, 0.25))
  ))
  analysis$status <- "complete"
  analysis
}

canonical_autosomal_test_subset <- function(fixture) {
  list(
    region = "22:20000000-21000000", variant_filter = "biallelic SNPs",
    variant_count = 100L, variant_count_method = "fixture",
    source_vcf_sha256 = canonical_production_test_env$canonical_production_sha256(
      file.path(fixture$mirror, fixture$source$files$filename[[1L]])
    ),
    derived_vcf_sha256 = paste(rep("1", 64L), collapse = ""),
    derived_index_sha256 = paste(rep("2", 64L), collapse = ""),
    command = "bcftools view --regions 22:20000000-21000000 fixture.vcf.gz",
    bcftools_version = "fixture-1.0"
  )
}

canonical_autosomal_test_proposal <- function(transform_subset = identity) {
  fixture <- canonical_production_fixture()
  output <- tempfile("canonical-autosomal-proposal-")
  subset <- transform_subset(canonical_autosomal_test_subset(fixture))
  result <- canonical_production_test_env$write_canonical_autosomal_baseline_proposal(
    analysis = canonical_autosomal_test_analysis(),
    source = fixture$source,
    source_dir = fixture$mirror,
    panel = data.table::fread(
      file.path(fixture$mirror, fixture$source$files$filename[[3L]]),
      data.table = FALSE
    ),
    subset = subset,
    output_dir = output,
    candidate_id = "0.10.0-autosomal-baseline-fixture",
    git_commit = paste(rep("a", 40L), collapse = ""),
    generated_at = "2026-07-23T12:00:00Z",
    environment = list(r_version = "fixture", platform = "fixture-platform")
  )
  list(fixture = fixture, output = output, result = result)
}

test_that("autosomal execution writes a checksum-bound unapproved proposal", {
  proposal <- canonical_autosomal_test_proposal()
  output <- proposal$output

  expect_identical(proposal$result$approval, "proposed")
  expect_true(
    canonical_production_test_env$verify_canonical_autosomal_baseline_proposal(output)
  )
  expect_setequal(names(proposal$result$observations), c(
    "subset_variant_count", "retained_sample_count", "qc_variant_count",
    "ld_pruned_variant_count", "pca_pc1_variance_proportion",
    "pca_pc2_variance_proportion"
  ))

  record <- jsonlite::read_json(
    file.path(output, "autosomal-baseline-proposal-record.json"),
    simplifyVector = TRUE
  )
  expect_identical(record$approval, "proposed")
  expect_identical(record$production_baseline_gate, "not_passed")
  expect_identical(record$metric_count, 6L)
  expect_false(record$raw_genotype_data_in_evidence)

  snapshot <- jsonlite::read_json(
    file.path(output, "autosomal-baseline-proposal.json"),
    simplifyVector = FALSE
  )
  expect_identical(snapshot$approval, "proposed")
  expect_length(snapshot$baseline_registry$metrics, 6L)
  expect_length(snapshot$approved_by, 0L)
  expect_length(snapshot$approved_at, 0L)
  expect_false(any(grepl(
    "\\.(vcf|vcf\\.gz|tbi|gds)$",
    list.files(output, recursive = TRUE), ignore.case = TRUE
  )))
})

test_that("autosomal proposal metrics cannot satisfy the approval gate", {
  fixture <- canonical_production_fixture()
  analysis <- canonical_autosomal_test_analysis()
  observed <- canonical_production_test_env$canonical_autosomal_observations(analysis, 100L)
  descriptor <- popgenVCF::canonical_dataset_from_source(fixture$source, fixture$mirror)
  registry <- canonical_production_test_env$canonical_autosomal_baseline_registry(
    observed, descriptor$id, list(git_commit = paste(rep("b", 40L), collapse = ""))
  )
  snapshot <- popgenVCF::new_canonical_real_data_baseline_snapshot(
    dataset = descriptor,
    registry = registry,
    sample_metadata = canonical_production_test_env$canonical_autosomal_sample_metadata(
      data.table::fread(
        file.path(fixture$mirror, fixture$source$files$filename[[3L]]),
        data.table = FALSE
      ),
      analysis$samples$ids
    ),
    dataset_version = descriptor$version,
    generated_by = "fixture",
    generated_at = "2026-07-23T12:00:00Z",
    source_commit = paste(rep("b", 40L), collapse = ""),
    approval = "proposed"
  )

  expect_error(
    popgenVCF::validate_canonical_real_data_baseline_snapshot(
      snapshot, require_approved = TRUE
    ),
    "not approved"
  )
})

test_that("autosomal proposal verification rejects tampering and raw data", {
  tampered <- canonical_autosomal_test_proposal()
  cat(
    "tampered\n",
    file = file.path(tampered$output, "autosomal-baseline-observations.tsv"),
    append = TRUE
  )
  expect_error(
    canonical_production_test_env$verify_canonical_autosomal_baseline_proposal(
      tampered$output
    ),
    "checksum verification failed"
  )

  injected <- canonical_autosomal_test_proposal()
  writeLines("raw data are forbidden", file.path(injected$output, "injected.vcf"))
  expect_error(
    canonical_production_test_env$verify_canonical_autosomal_baseline_proposal(
      injected$output
    ),
    "checksum inventory is incomplete"
  )
})

test_that("autosomal observations reject incomplete scientific results", {
  analysis <- canonical_autosomal_test_analysis()
  analysis$results$pca$variance <- analysis$results$pca$variance[1L, , drop = FALSE]
  expect_error(
    canonical_production_test_env$canonical_autosomal_observations(analysis, 100L),
    "at least two PCA"
  )

  fixture <- canonical_production_fixture()
  panel <- data.table::fread(
    file.path(fixture$mirror, fixture$source$files$filename[[3L]]),
    data.table = FALSE
  )
  expect_error(
    canonical_production_test_env$canonical_autosomal_sample_metadata(panel, "S1"),
    "inventories do not match"
  )
})

test_that("autosomal proposal rejects contract drift and a stale source binding", {
  expect_error(
    canonical_autosomal_test_proposal(function(subset) {
      subset$region <- "22:21000001-22000000"
      subset
    }),
    "does not match"
  )

  expect_error(
    canonical_autosomal_test_proposal(function(subset) {
      subset$source_vcf_sha256 <- paste(rep("0", 64L), collapse = "")
      subset
    }),
    "not bound"
  )
})
