benchmark_archive_script_env <- local({
  script_path <- normalizePath(
    testthat::test_path("..", "..", "scripts", "build_release_benchmark_archive.R"),
    mustWork = TRUE
  )
  lines <- readLines(script_path, warn = FALSE)
  fn_start <- grep("^canonical_benchmark_dataset <- function", lines)
  fn_end <- grep("^performance_by_tier <- ", lines)[[1L]] - 1L
  fn_source <- paste(lines[fn_start:fn_end], collapse = "\n")
  env <- new.env(parent = globalenv())
  eval(parse(text = fn_source), envir = env)
  env
})

test_that("benchmark_tier_dataset builds correctly-shaped synthetic/medium/large matrices", {
  env <- benchmark_archive_script_env
  expected_sizes <- list(
    synthetic = c(samples = 60L, snps = 2000L),
    medium = c(samples = 300L, snps = 20000L),
    large = c(samples = 1000L, snps = 100000L)
  )
  for (tier in names(expected_sizes)) {
    dataset <- env$benchmark_tier_dataset(tier, "unknown")
    expect_identical(dim(dataset$genotype), unname(expected_sizes[[tier]]))
    expect_identical(length(dataset$sample_ids), unname(expected_sizes[[tier]][["samples"]]))
    expect_identical(length(dataset$snp_ids), unname(expected_sizes[[tier]][["snps"]]))
    expect_identical(nrow(dataset$metadata), unname(expected_sizes[[tier]][["samples"]]))
    expect_true(all(c("sample", "population") %in% names(dataset$metadata)))
    expect_identical(length(dataset$chromosome), unname(expected_sizes[[tier]][["snps"]]))
    expect_identical(length(dataset$position), unname(expected_sizes[[tier]][["snps"]]))
  }
})

test_that("benchmark_tier_dataset rejects unknown tiers", {
  env <- benchmark_archive_script_env
  expect_error(env$benchmark_tier_dataset("huge", "unknown"), "Unknown dataset tier")
})

test_that("benchmark_tier_spec produces a spec that runs and reports the requested dataset_tier", {
  env <- benchmark_archive_script_env
  dataset <- env$benchmark_tier_dataset("synthetic", "unknown")
  spec <- env$benchmark_tier_spec("synthetic", dataset)
  expect_identical(spec$metadata$dataset_tier, "synthetic")
  result <- popgenVCF::run_performance_benchmark(spec)
  expect_s3_class(result, "PopgenVCFPerformanceResult")
  expect_true(all(is.finite(result$summary$runtime_median)))
})

test_that("canonical_benchmark_dataset acquires the real, already-approved chr22 region (opt-in, real network)", {
  skip_if_not(nzchar(Sys.getenv("POPGENVCF_TEST_CANONICAL_BENCHMARK")),
    "set POPGENVCF_TEST_CANONICAL_BENCHMARK=true to run this real chr22 acquisition test")
  skip_if(Sys.which("bcftools") == "", "bcftools is not available")
  env <- benchmark_archive_script_env
  dataset <- env$canonical_benchmark_dataset(paste(rep("a", 40L), collapse = ""))
  expect_true(all(dataset$chromosome == 22L))
  expect_true(all(dataset$position >= 20000000L & dataset$position <= 21000000L))
  expect_identical(nrow(dataset$metadata), length(dataset$sample_ids))
  expect_true(length(unique(dataset$metadata$population)) > 1L)
  expect_identical(dim(dataset$genotype), c(length(dataset$sample_ids), length(dataset$snp_ids)))
})
