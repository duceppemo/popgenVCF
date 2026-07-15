test_that("performance specifications validate inputs", {
  expect_error(new_performance_benchmark_spec("x", 1), "runner")
  expect_error(new_performance_benchmark_spec("x", identity, threads = 0), "positive")
  expect_error(new_performance_benchmark_spec("x", identity, iterations = 0), "positive")
  expect_error(new_performance_benchmark_spec("x", identity, runtime_regression = -1), "nonnegative")
})

test_that("performance measurements produce stable summaries", {
  runner <- function(threads) {
    path <- file.path(Sys.getenv("POPGENVCF_PERFORMANCE_TEMP"), "payload.bin")
    writeBin(raw(1024L * threads), path)
    sum(seq_len(1000L)) / threads
  }
  fingerprint <- list(host = "fixture", cores = 4L)
  spec <- new_performance_benchmark_spec(
    "fixture", runner, threads = c(1L, 2L), warmup = 0L,
    iterations = 2L, seed = 10L
  )
  result <- run_performance_benchmark(spec, fingerprint)

  expect_s3_class(result, "PopgenVCFPerformanceResult")
  expect_equal(nrow(result$measurements), 4L)
  expect_equal(result$summary$threads, c(1L, 2L))
  expect_true(all(result$summary$runtime_median >= 0))
  expect_true(all(result$summary$disk_median_mb > 0))
  expect_equal(result$summary$speedup[1L], 1)
  expect_equal(result$fingerprint_id,
               digest::digest(fingerprint, algo = "sha256", serialize = TRUE))

  tab <- performance_benchmark_table(result)
  expect_equal(names(tab)[1:2], c("id", "fingerprint_id"))
})

test_that("baseline comparisons detect gating and informational regressions", {
  fingerprint <- list(host = "fixture")
  make_result <- function(runtime, memory = 10, disk = 1, gating = TRUE) {
    structure(list(
      schema_version = "1.0", id = "fixture", fingerprint = fingerprint,
      fingerprint_id = digest::digest(fingerprint, algo = "sha256", serialize = TRUE),
      measurements = data.table::data.table(),
      summary = data.table::data.table(
        threads = 1L, runtime_median = runtime, runtime_mad = 0,
        runtime_q05 = runtime, runtime_q95 = runtime,
        memory_median_mb = memory, memory_mad_mb = 0,
        disk_median_mb = disk, disk_mad_mb = 0,
        speedup = 1, scaling_efficiency = 1
      ),
      thresholds = c(runtime_seconds = 0.20, peak_memory_mb = 0.25,
                     temporary_disk_mb = 0.25),
      gating = gating, metadata = list()
    ), class = "PopgenVCFPerformanceResult")
  }

  baseline <- make_result(1)
  observed <- make_result(1.5)
  strict <- compare_performance_baseline(observed, baseline)
  expect_equal(strict$status, "failed")
  expect_true(strict$comparisons[metric == "runtime_seconds", regressed])

  informational <- compare_performance_baseline(observed, baseline, gating = FALSE)
  expect_equal(informational$status, "passed")
  expect_true(any(informational$comparisons$regressed))
})

test_that("incompatible machines are rejected unless explicitly permitted", {
  make_result <- function(host) structure(list(
    schema_version = "1.0", id = "fixture", fingerprint = list(host = host),
    fingerprint_id = digest::digest(list(host = host), algo = "sha256", serialize = TRUE),
    measurements = data.table::data.table(),
    summary = data.table::data.table(
      threads = 1L, runtime_median = 1, runtime_mad = 0,
      runtime_q05 = 1, runtime_q95 = 1, memory_median_mb = 1,
      memory_mad_mb = 0, disk_median_mb = 0, disk_mad_mb = 0,
      speedup = 1, scaling_efficiency = 1
    ),
    thresholds = c(runtime_seconds = .2, peak_memory_mb = .25,
                   temporary_disk_mb = .25),
    gating = FALSE, metadata = list()
  ), class = "PopgenVCFPerformanceResult")

  a <- make_result("a"); b <- make_result("b")
  expect_error(compare_performance_baseline(a, b), "fingerprints differ")
  comparison <- compare_performance_baseline(a, b, allow_incompatible = TRUE)
  expect_false(comparison$compatible)
})

test_that("performance baselines round-trip deterministically", {
  result <- structure(list(
    schema_version = "1.0", id = "fixture", fingerprint = list(host = "x"),
    fingerprint_id = "abc", measurements = data.table::data.table(),
    summary = data.table::data.table(threads = 1L), thresholds = numeric(),
    gating = FALSE, metadata = list()
  ), class = "PopgenVCFPerformanceResult")
  path <- tempfile(fileext = ".rds")
  save_performance_baseline(result, path)
  expect_identical(read_performance_baseline(path), result)
})
