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
})

test_that("performance budgets classify regressions", {
  sha <- paste(rep("b", 40), collapse = "")
  baseline <- new_continuous_benchmark_observation(
    "fst-synthetic", "fst", "synthetic", "0.9.28", sha,
    runtime_seconds = 10, peak_memory_mb = 100, throughput = 100,
    scaling_efficiency = 0.8, repetitions = 5
  )
  current <- new_continuous_benchmark_observation(
    "fst-synthetic", "fst", "synthetic", "0.9.29", sha,
    runtime_seconds = 10.5, peak_memory_mb = 105, throughput = 98,
    scaling_efficiency = 0.78, repetitions = 5
  )
  budget <- new_release_performance_budget("default")
  comparison <- compare_continuous_release_benchmark(current, baseline, budget)
  expect_identical(comparison$status, "passed")
  expect_true(comparison$release_ready)

  regressed <- current
  regressed$runtime_seconds <- 12
  failed <- compare_continuous_release_benchmark(regressed, baseline, budget)
  expect_identical(failed$status, "failed")
  expect_false(failed$release_ready)
})

test_that("insufficient repetitions never gate a release", {
  sha <- paste(rep("c", 40), collapse = "")
  baseline <- new_continuous_benchmark_observation(
    "ibs-canonical", "ibs", "canonical", "baseline", sha,
    10, 100, 100, 0.8, repetitions = 5
  )
  current <- new_continuous_benchmark_observation(
    "ibs-canonical", "ibs", "canonical", "current", sha,
    9, 90, 110, 0.9, repetitions = 2
  )
  comparison <- compare_continuous_release_benchmark(
    current, baseline, new_release_performance_budget("strict", minimum_repetitions = 5)
  )
  expect_identical(comparison$status, "insufficient-evidence")
  expect_false(comparison$release_ready)
})

test_that("benchmark evidence is deterministic and fail closed", {
  sha <- paste(rep("d", 40), collapse = "")
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

  comparison$release_ready <- FALSE
  expect_error(
    write_continuous_benchmark_evidence(
      list(current), list(comparison), tempfile(), require_release_ready = TRUE
    ),
    "not release ready"
  )
})
