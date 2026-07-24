test_that("canonical production execution writes a checksum-verified gate bundle", {
  fixture <- canonical_production_fixture()
  output <- tempfile("canonical-evidence-")
  data_dir <- tempfile("canonical-data-")

  result <- canonical_production_test_env$run_canonical_production_execution(
    output_dir = output,
    data_dir = data_dir,
    candidate_id = "0.10.0-production-fixture",
    git_commit = paste(rep("a", 40L), collapse = ""),
    generated_at = "2026-07-22T23:59:59Z",
    source = fixture$source,
    source_dir = fixture$mirror,
    inspect = fixture$inspect,
    environment = list(
      r_version = "fixture",
      platform = "fixture-platform",
      packages = list(popgenVCF = as.character(utils::packageVersion("popgenVCF")))
    )
  )

  expect_equal(result$dataset_id, fixture$source$id)
  expect_equal(result$sample_count, 2L)
  expect_equal(result$variant_count, 3)
  expect_true(canonical_production_test_env$verify_canonical_production_evidence(output))

  expected <- c(
    "canonical-production-execution.json",
    "canonical-production-environment.tsv",
    "canonical-production-artifacts.tsv",
    "canonical-production-SHA256SUMS.txt",
    "canonical-validation-gate-record.json",
    "canonical_dataset_structure.tsv",
    "canonical_sample_metadata.tsv",
    file.path("source", "canonical_source_acquisition.tsv"),
    file.path("source", "canonical_source_verification.tsv"),
    file.path("source", "canonical_dataset_registry.tsv"),
    file.path("source", "dataset", "canonical_dataset.tsv"),
    file.path("source", "dataset", "canonical_dataset_verification.tsv"),
    file.path("source", "dataset", "canonical_validation_methods.md")
  )
  expect_true(all(file.exists(file.path(output, expected))))
  expect_false(any(file.exists(file.path(output, fixture$source$files$filename))))

  execution <- jsonlite::read_json(
    file.path(output, "canonical-production-execution.json"),
    simplifyVector = TRUE
  )
  expect_identical(execution$status, "passed")
  expect_identical(execution$gate_states$canonical_validation, "passed")
  expect_identical(execution$gate_states$production_baseline, "not_run")
  expect_identical(execution$gate_states$external_concordance, "not_run")
  expect_false(execution$data_retention$raw_dataset_in_evidence_bundle)
  expect_setequal(names(execution$commands), c("sample_inventory", "variant_count"))

  gate <- jsonlite::read_json(
    file.path(output, "canonical-validation-gate-record.json"),
    simplifyVector = TRUE
  )
  expect_identical(gate$gate_id, "canonical_validation")
  expect_identical(gate$status, "passed")
  expect_null(gate$approval)
  expect_gt(nrow(gate$artifacts), 0L)
  expect_true(all(grepl("^[a-f0-9]{64}$", gate$artifacts$sha256)))

  artifacts <- data.table::fread(file.path(output, "canonical-production-artifacts.tsv"))
  expect_false(any(grepl("^/|(^|/)\\.\\.(/|$)", artifacts$path)))
  expect_true(all(file.exists(file.path(output, artifacts$path))))
})

test_that("production inspection accepts an explicit mixed-sex autosomal policy", {
  fixture <- canonical_production_fixture()
  fixture$source$chromosome_scope <- "chr22"
  fixture$source$sample_sex_policy <- "mixed"
  inspection <- fixture$inspect(fixture$source, fixture$mirror)
  inspection$sample_metadata$sex <- c("male", "female")

  validated <- canonical_production_test_env$canonical_production_validate_inspection(
    inspection, fixture$source
  )
  expect_equal(validated$summary$chromosome_scope, "chr22")
  expect_equal(validated$summary$sample_sex_policy, "mixed")

  inspection$summary$sex_policy_satisfied <- FALSE
  expect_error(
    canonical_production_test_env$canonical_production_validate_inspection(
      inspection, fixture$source
    ),
    "does not satisfy"
  )
})

test_that("canonical production execution fails closed on altered source data", {
  fixture <- canonical_production_fixture()
  cat("tampered\n", file = file.path(fixture$mirror, fixture$source$files$filename[[1L]]),
      append = TRUE)
  output <- tempfile("canonical-evidence-")

  expect_error(
    canonical_production_test_env$run_canonical_production_execution(
      output_dir = output,
      data_dir = tempfile("canonical-data-"),
      candidate_id = "0.10.0-production-fixture",
      git_commit = paste(rep("b", 40L), collapse = ""),
      generated_at = "2026-07-22T23:59:59Z",
      source = fixture$source,
      source_dir = fixture$mirror,
      inspect = fixture$inspect,
      environment = list(r_version = "fixture")
    ),
    "upstream MD5 verification failed"
  )
  expect_false(file.exists(file.path(output, "canonical-validation-gate-record.json")))
})

test_that("canonical production execution requires an explicit acquisition source", {
  fixture <- canonical_production_fixture()
  expect_error(
    canonical_production_test_env$run_canonical_production_execution(
      output_dir = tempfile("canonical-evidence-"),
      data_dir = tempfile("canonical-data-"),
      candidate_id = "0.10.0-production-fixture",
      git_commit = paste(rep("c", 40L), collapse = ""),
      generated_at = "2026-07-22T23:59:59Z",
      source = fixture$source,
      allow_download = FALSE,
      inspect = fixture$inspect,
      environment = list(r_version = "fixture")
    ),
    "downloads are disabled"
  )
})

test_that("canonical production evidence detects post-run tampering", {
  fixture <- canonical_production_fixture()
  output <- tempfile("canonical-evidence-")
  canonical_production_test_env$run_canonical_production_execution(
    output_dir = output,
    data_dir = tempfile("canonical-data-"),
    candidate_id = "0.10.0-production-fixture",
    git_commit = paste(rep("d", 40L), collapse = ""),
    generated_at = "2026-07-22T23:59:59Z",
    source = fixture$source,
    source_dir = fixture$mirror,
    inspect = fixture$inspect,
    environment = list(r_version = "fixture")
  )
  cat("tampered\n", file = file.path(output, "canonical_dataset_structure.tsv"), append = TRUE)
  expect_error(
    canonical_production_test_env$verify_canonical_production_evidence(output),
    "checksum verification failed"
  )
})

test_that("canonical production execution keeps raw data outside evidence", {
  fixture <- canonical_production_fixture()
  parent <- tempfile("canonical-parent-")
  dir.create(parent)
  output <- file.path(parent, "evidence")
  data_dir <- file.path(output, "raw-data")

  expect_error(
    canonical_production_test_env$run_canonical_production_execution(
      output_dir = output,
      data_dir = data_dir,
      candidate_id = "0.10.0-production-fixture",
      git_commit = paste(rep("e", 40L), collapse = ""),
      generated_at = "2026-07-22T23:59:59Z",
      source = fixture$source,
      source_dir = fixture$mirror,
      inspect = fixture$inspect,
      environment = list(r_version = "fixture")
    ),
    "data_dir must be outside output_dir"
  )
})

test_that("canonical production evidence rejects unlisted injected files", {
  fixture <- canonical_production_fixture()
  output <- tempfile("canonical-evidence-")
  canonical_production_test_env$run_canonical_production_execution(
    output_dir = output,
    data_dir = tempfile("canonical-data-"),
    candidate_id = "0.10.0-production-fixture",
    git_commit = paste(rep("f", 40L), collapse = ""),
    generated_at = "2026-07-22T23:59:59Z",
    source = fixture$source,
    source_dir = fixture$mirror,
    inspect = fixture$inspect,
    environment = list(r_version = "fixture")
  )
  writeLines("unexpected", file.path(output, "unlisted.txt"))
  expect_error(
    canonical_production_test_env$verify_canonical_production_evidence(output),
    "checksum inventory is incomplete"
  )
})
