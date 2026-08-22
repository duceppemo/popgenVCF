test_that("continuous benchmark observations are deterministic", {
  observation <- new_continuous_benchmark_observation(
    benchmark_id = "pca-canonical", module = "pca", dataset_tier = "canonical",
    release = "0.10.0-rc1", git_sha = paste(rep("a", 40), collapse = ""),
    runtime_seconds = 10, peak_memory_mb = 128, throughput = 500,
    scaling_efficiency = 0.85, threads = 4, repetitions = 7,
    environment = list(r_version = "4.5.1", cpu = "test")
  )
  expect_s3_class(observation, "PopgenVCFContinuousBenchmarkObservation")
  expect_identical(names(observation$environment), c("cpu", "r_version"))
  expect_invisible(validate_continuous_benchmark_observation(observation))
  expect_error(new_continuous_benchmark_observation(
    "bad", "pca", "synthetic", "current", paste(rep("a", 40), collapse = ""),
    1, 1, 1, 1, threads = 1.5
  ), "whole number")
})

test_that("performance budgets classify regressions", {
  sha <- paste(rep("b", 40), collapse = "")
  environment <- list(cpu = "test", r_version = "4.5.1")
  baseline <- new_continuous_benchmark_observation(
    "fst-synthetic", "fst", "synthetic", "0.9.28", sha,
    runtime_seconds = 10, peak_memory_mb = 100, throughput = 100,
    scaling_efficiency = 0.8, repetitions = 5, environment = environment
  )
  current <- new_continuous_benchmark_observation(
    "fst-synthetic", "fst", "synthetic", "0.9.29", sha,
    runtime_seconds = 10.5, peak_memory_mb = 105, throughput = 98,
    scaling_efficiency = 0.78, repetitions = 5, environment = environment
  )
  budget <- new_release_performance_budget("default")
  expect_invisible(validate_release_performance_budget(budget))
  comparison <- compare_continuous_release_benchmark(current, baseline, budget)
  expect_identical(comparison$status, "passed")
  expect_true(comparison$release_ready)
  expect_invisible(validate_continuous_benchmark_comparison(comparison))

  regressed <- current
  regressed$runtime_seconds <- 12
  failed <- compare_continuous_release_benchmark(regressed, baseline, budget)
  expect_identical(failed$status, "failed")
  expect_false(failed$release_ready)
})

test_that("identity and environment mismatches cannot become release evidence", {
  sha <- paste(rep("c", 40), collapse = "")
  baseline <- new_continuous_benchmark_observation(
    "ibs-canonical", "ibs", "canonical", "baseline", sha,
    10, 100, 100, 0.8, threads = 2, repetitions = 5,
    environment = list(cpu = "A")
  )
  wrong_identity <- new_continuous_benchmark_observation(
    "ibs-canonical", "ibs", "canonical", "current", sha,
    9, 90, 110, 0.9, threads = 4, repetitions = 5,
    environment = list(cpu = "A")
  )
  expect_error(compare_continuous_release_benchmark(
    wrong_identity, baseline, new_release_performance_budget("default")
  ), "identity-compatible")

  different_environment <- new_continuous_benchmark_observation(
    "ibs-canonical", "ibs", "canonical", "current", sha,
    9, 90, 110, 0.9, threads = 2, repetitions = 5,
    environment = list(cpu = "B")
  )
  comparison <- compare_continuous_release_benchmark(
    different_environment, baseline, new_release_performance_budget("default")
  )
  expect_identical(comparison$status, "insufficient-evidence")
  expect_false(comparison$environment_compatible)
  expect_false(comparison$release_ready)
})

test_that("environment_compatible tolerates a routine kernel ABI build bump but still catches a real OS/kernel difference", {
  sha <- paste(rep("e", 40), collapse = "")
  budget <- new_release_performance_budget("default")
  base_env <- list(
    os = "Linux", release = "6.17.0-1020-azure", machine = "x86_64",
    r_version = "4.6.1", platform = "x86_64-pc-linux-gnu",
    physical_cores = 4L, logical_cores = 4L, blas = "openblas"
  )
  baseline <- new_continuous_benchmark_observation(
    "pca-canonical", "pca", "canonical", "baseline", sha,
    10, 100, 100, 0.8, repetitions = 5, environment = base_env
  )

  # Same kernel major.minor.patch and flavor, only the ABI build number
  # (routine monthly runner patching) differs -- must be treated compatible.
  bumped_env <- base_env
  bumped_env$release <- "6.17.0-1022-azure"
  bumped <- new_continuous_benchmark_observation(
    "pca-canonical", "pca", "canonical", "current", sha,
    10, 100, 100, 0.8, repetitions = 5, environment = bumped_env
  )
  bumped_comparison <- compare_continuous_release_benchmark(bumped, baseline, budget)
  expect_true(bumped_comparison$environment_compatible)
  expect_identical(bumped_comparison$status, "passed")

  # A genuinely different kernel major version must still be caught.
  different_kernel_env <- base_env
  different_kernel_env$release <- "5.15.0-1020-azure"
  different_kernel <- new_continuous_benchmark_observation(
    "pca-canonical", "pca", "canonical", "current", sha,
    10, 100, 100, 0.8, repetitions = 5, environment = different_kernel_env
  )
  expect_false(compare_continuous_release_benchmark(
    different_kernel, baseline, budget
  )$environment_compatible)

  # A genuinely different kernel flavor must still be caught.
  different_flavor_env <- base_env
  different_flavor_env$release <- "6.17.0-1022-generic"
  different_flavor <- new_continuous_benchmark_observation(
    "pca-canonical", "pca", "canonical", "current", sha,
    10, 100, 100, 0.8, repetitions = 5, environment = different_flavor_env
  )
  expect_false(compare_continuous_release_benchmark(
    different_flavor, baseline, budget
  )$environment_compatible)

  # A different R version must still be caught -- normalization only ever
  # touches the release field, nothing else.
  different_r_env <- bumped_env
  different_r_env$r_version <- "4.5.0"
  different_r <- new_continuous_benchmark_observation(
    "pca-canonical", "pca", "canonical", "current", sha,
    10, 100, 100, 0.8, repetitions = 5, environment = different_r_env
  )
  expect_false(compare_continuous_release_benchmark(
    different_r, baseline, budget
  )$environment_compatible)

  # An unrecognized release-string format (not Ubuntu's
  # version-build-flavor pattern) is left untouched, not silently relaxed.
  unrecognized_env <- base_env
  unrecognized_env$release <- "23.6.0"
  unrecognized_baseline <- new_continuous_benchmark_observation(
    "pca-canonical", "pca", "canonical", "baseline", sha,
    10, 100, 100, 0.8, repetitions = 5, environment = unrecognized_env
  )
  unrecognized_env2 <- unrecognized_env
  unrecognized_env2$release <- "23.6.1"
  unrecognized_current <- new_continuous_benchmark_observation(
    "pca-canonical", "pca", "canonical", "current", sha,
    10, 100, 100, 0.8, repetitions = 5, environment = unrecognized_env2
  )
  expect_false(compare_continuous_release_benchmark(
    unrecognized_current, unrecognized_baseline, budget
  )$environment_compatible)
})

test_that("both current and baseline require adequate repetitions", {
  sha <- paste(rep("d", 40), collapse = "")
  baseline <- new_continuous_benchmark_observation(
    "ibs-canonical", "ibs", "canonical", "baseline", sha,
    10, 100, 100, 0.8, repetitions = 2
  )
  current <- new_continuous_benchmark_observation(
    "ibs-canonical", "ibs", "canonical", "current", sha,
    9, 90, 110, 0.9, repetitions = 5
  )
  comparison <- compare_continuous_release_benchmark(
    current, baseline, new_release_performance_budget("strict", minimum_repetitions = 5)
  )
  expect_identical(comparison$status, "insufficient-evidence")
  expect_false(comparison$repetitions_complete)
  expect_false(comparison$release_ready)
})

test_that("benchmark evidence is deterministic and fail closed", {
  sha <- paste(rep("e", 40), collapse = "")
  baseline <- new_continuous_benchmark_observation(
    "pca-synthetic", "pca", "synthetic", "baseline", sha,
    10, 100, 100, 0.8, repetitions = 5
  )
  current <- new_continuous_benchmark_observation(
    "pca-synthetic", "pca", "synthetic", "current", sha,
    10, 100, 100, 0.8, repetitions = 5
  )
  comparison <- compare_continuous_release_benchmark(
    current, baseline, new_release_performance_budget("default")
  )
  output <- tempfile("continuous-benchmark-")
  paths <- write_continuous_benchmark_evidence(
    list(current), list(comparison), output, require_release_ready = TRUE
  )
  expect_true(all(file.exists(paths)))
  table <- data.table::fread(paths[["tsv"]])
  expect_equal(table$git_sha, sha)
  expect_true(nzchar(table$environment_sha256))

  expect_error(
    write_continuous_benchmark_evidence(
      list(current), list(), tempfile(), require_release_ready = TRUE
    ),
    "not release ready"
  )

  mutated <- comparison
  mutated$release_ready <- FALSE
  expect_error(validate_continuous_benchmark_comparison(mutated), "inconsistent")
  expect_error(
    write_continuous_benchmark_evidence(
      list(current), list(mutated), tempfile(), require_release_ready = TRUE
    ),
    "inconsistent"
  )

  wrong_release <- comparison
  wrong_release$current_release <- "different-current"
  expect_invisible(validate_continuous_benchmark_comparison(wrong_release))
  expect_error(
    write_continuous_benchmark_evidence(
      list(current), list(wrong_release), tempfile(), require_release_ready = TRUE
    ),
    "exact supplied current observation"
  )

  malformed_checks <- comparison
  malformed_checks$checks$passed[[1L]] <- NA
  expect_error(validate_continuous_benchmark_comparison(malformed_checks), "checks")
})

test_that("duplicate observations are rejected", {
  sha <- paste(rep("f", 40), collapse = "")
  observation <- new_continuous_benchmark_observation(
    "pca-synthetic", "pca", "synthetic", "current", sha,
    10, 100, 100, 0.8, repetitions = 5
  )
  expect_error(write_continuous_benchmark_evidence(
    list(observation, observation), output_dir = tempfile()
  ), "must be unique")
})

test_that("multiple dataset_tier observations for the same benchmark_id round-trip together", {
  sha <- paste(rep("1", 40), collapse = "")
  tiers <- c("synthetic", "canonical", "medium", "large")
  baseline_observations <- lapply(tiers, function(tier) {
    new_continuous_benchmark_observation(
      "pipeline-core-analyses", "pca_ibs_diversity_fst", tier, "baseline", sha,
      runtime_seconds = 10, peak_memory_mb = 100, throughput = 0.1,
      scaling_efficiency = 1, repetitions = 5
    )
  })
  current_observations <- lapply(tiers, function(tier) {
    new_continuous_benchmark_observation(
      "pipeline-core-analyses", "pca_ibs_diversity_fst", tier, "current", sha,
      runtime_seconds = 10.5, peak_memory_mb = 101, throughput = 0.095,
      scaling_efficiency = 0.98, repetitions = 5
    )
  })
  budget <- new_release_performance_budget("multi-tier-budget")
  comparisons <- Map(
    compare_continuous_release_benchmark, current_observations, baseline_observations,
    MoreArgs = list(budget = budget)
  )

  output <- tempfile("multi-tier-continuous-benchmark-")
  paths <- write_continuous_benchmark_evidence(
    current_observations, comparisons, output, require_release_ready = TRUE
  )
  payload <- jsonlite::read_json(paths[["json"]], simplifyVector = FALSE)
  expect_length(payload$observations, length(tiers))
  expect_setequal(
    vapply(payload$observations, `[[`, character(1L), "dataset_tier"), tiers
  )
  expect_length(payload$comparisons, length(tiers))
  expect_true(all(vapply(payload$comparisons, `[[`, character(1L), "status") == "passed"))
  expect_true(isTRUE(payload$release_ready))

  table <- data.table::fread(paths[["tsv"]])
  expect_setequal(table$dataset_tier, tiers)
})
