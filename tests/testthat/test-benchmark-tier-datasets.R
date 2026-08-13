benchmark_tier_module_path <- function() {
  # Prefer the checked-out source tree over a possibly-stale installed copy
  # -- see the identical rationale on rc_policy_path() in
  # helper-release-candidate-dossier.R.
  source_path <- testthat::test_path("..", "..", "inst", "scripts", "continuous_benchmark_tiers.R")
  if (file.exists(source_path)) return(source_path)
  installed <- system.file("scripts", "continuous_benchmark_tiers.R", package = "popgenVCF")
  if (nzchar(installed)) return(installed)
  source_path
}

benchmark_archive_script_env <- local({
  env <- new.env(parent = globalenv())
  sys.source(benchmark_tier_module_path(), envir = env)
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

local_fake_system_file <- function(env, value) {
  # resolve_benchmark_helper_script() is a loose sys.source()d function, not
  # part of any package namespace, so testthat::local_mocked_bindings() (which
  # patches a *namespace's* view of a function) has nothing to attach to.
  # Inject system.file directly into its own defining environment instead --
  # R resolves free variables by walking the enclosing environment chain
  # starting from where the function was defined, so this binding shadows
  # base::system.file for calls made from inside `env` only, without
  # affecting anything else.
  old <- if (exists("system.file", envir = env, inherits = FALSE)) get("system.file", envir = env) else NULL
  assign("system.file", value, envir = env)
  withr::defer({
    if (is.null(old)) rm("system.file", envir = env) else assign("system.file", old, envir = env)
  }, envir = parent.frame())
}

test_that("resolve_benchmark_helper_script() prefers a reachable source tree over system.file()", {
  env <- benchmark_archive_script_env
  root <- withr::local_tempdir()
  dir.create(file.path(root, "inst", "scripts"), recursive = TRUE)
  writeLines("from-source-tree", file.path(root, "inst", "scripts", "fake_module.R"))
  withr::local_dir(root)

  fake_installed <- withr::local_tempfile()
  writeLines("from-installed-copy", fake_installed)
  local_fake_system_file(env, function(...) fake_installed)

  path <- env$resolve_benchmark_helper_script("fake_module.R")
  expect_identical(readLines(path), "from-source-tree")
})

test_that("resolve_benchmark_helper_script() falls back to system.file() when no source tree is reachable", {
  env <- benchmark_archive_script_env
  root <- withr::local_tempdir()
  withr::local_dir(root)

  fake_installed <- withr::local_tempfile()
  writeLines("from-installed-copy", fake_installed)
  local_fake_system_file(env, function(...) fake_installed)

  path <- env$resolve_benchmark_helper_script("fake_module.R")
  expect_identical(path, fake_installed)
  expect_identical(readLines(path), "from-installed-copy")
})

test_that("resolve_benchmark_helper_script() fails closed when neither is reachable", {
  env <- benchmark_archive_script_env
  root <- withr::local_tempdir()
  withr::local_dir(root)

  local_fake_system_file(env, function(...) "")

  expect_error(
    env$resolve_benchmark_helper_script("fake_module.R"),
    "Missing helper script: fake_module.R"
  )
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
