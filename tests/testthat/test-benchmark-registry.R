test_that("benchmark registry executes deterministic passing benchmarks", {
  dataset <- new_benchmark_dataset(
    "tiny-vector", loader = function() c(a = 1, b = 2),
    metadata = list(samples = 2L)
  )
  spec <- new_benchmark_spec(
    "identity-vector", "numerical", dataset,
    runner = function(x) list(observed = x, provenance = list(engine = "fixture")),
    reference = c(a = 1, b = 2), absolute_tolerance = 0,
    relative_tolerance = 0, runtime_budget_seconds = 10
  )
  registry <- register_benchmark(new_benchmark_registry(), spec)
  expect_equal(list_benchmarks(registry)$id, "identity-vector")
  suite <- run_benchmark_suite(registry)
  expect_s3_class(suite, "PopgenVCFBenchmarkSuite")
  expect_true(suite$passed)
  expect_equal(benchmark_suite_table(suite)$status, "passed")
  expect_equal(suite$results[[1L]]$comparisons$absolute_error, c(0, 0))
})

test_that("tolerance and resource budget failures are explicit", {
  dataset <- new_benchmark_dataset("budget-data", loader = function() 1)
  numerical <- new_benchmark_spec(
    "numerical-failure", "scientific", dataset,
    runner = function(x) x + 1, reference = 1,
    absolute_tolerance = 0, relative_tolerance = 0
  )
  memory <- new_benchmark_spec(
    "memory-failure", "performance", dataset,
    runner = function(x) list(observed = x, memory_mb = 20),
    reference = 1, memory_budget_mb = 10
  )
  registry <- new_benchmark_registry(list(numerical, memory))
  suite <- run_benchmark_suite(registry)
  tab <- benchmark_suite_table(suite)
  expect_false(suite$passed)
  expect_equal(tab[id == "numerical-failure", status], "failed")
  expect_match(tab[id == "numerical-failure", message], "numerical")
  expect_equal(tab[id == "memory-failure", status], "failed")
  expect_match(tab[id == "memory-failure", message], "memory")
})

test_that("empty observed/reference with a real reference supplied fails loudly instead of vacuously passing", {
  # Real gap found in a pre-release audit: a benchmark runner that returns
  # empty observed values (a real upstream bug, e.g. a scan that found zero
  # windows/records) produced a 0-row comparisons table, and the old
  # `!nrow(comparisons) || all(...)` check reported that as numerically
  # passed with zero metrics actually verified -- masking the real failure.
  dataset <- new_benchmark_dataset("empty-data", loader = function() numeric(0))
  spec <- new_benchmark_spec(
    "empty-observed", "numerical", dataset,
    runner = function(x) list(observed = x), reference = numeric(0),
    absolute_tolerance = 0, relative_tolerance = 0
  )
  result <- run_benchmark(spec)
  expect_equal(result$status, "failed")
  expect_match(result$message, "numerical")
})

test_that("observed values missing a reference metric name fail loudly instead of silently comparing NA", {
  # Real bug: observed[names(reference)] silently inserted NA for any
  # reference name absent from observed (same length, different names
  # passes the earlier length check) -- `passed` then became NA, and this
  # function's own `if (numerical_pass && ...)` crashed with "missing
  # value where TRUE/FALSE needed" instead of a clear, catchable failure.
  dataset <- new_benchmark_dataset("mismatch-data", loader = function() c(fst = 0.1, pi = 0.2))
  spec <- new_benchmark_spec(
    "name-mismatch", "numerical", dataset,
    runner = function(x) list(observed = c(fst = 0.1, other_metric = 0.2)),
    reference = c(fst = 0.1, pi = 0.2)
  )
  result <- run_benchmark(spec)
  expect_equal(result$status, "error")
  expect_match(result$message, "pi")
})

test_that("a spec with genuinely no reference still passes vacuously -- the legitimate informational case", {
  dataset <- new_benchmark_dataset("no-reference-data", loader = function() c(a = 1))
  spec <- new_benchmark_spec(
    "no-reference", "performance", dataset,
    runner = function(x) list(observed = x), reference = NULL
  )
  result <- run_benchmark(spec)
  expect_equal(result$status, "passed")
})

test_that("optional requirements produce transparent skips", {
  dataset <- new_benchmark_dataset("optional-data", loader = function() 1)
  spec <- new_benchmark_spec(
    "optional-tool", "external", dataset,
    runner = function(x) stop("must not run"), reference = 1,
    requirements = function() "optional executable is unavailable"
  )
  suite <- run_benchmark_suite(new_benchmark_registry(list(spec)))
  expect_true(suite$passed)
  expect_equal(suite$results[[1L]]$status, "skipped")
  expect_match(suite$results[[1L]]$message, "unavailable")
})

test_that("benchmark suites filter and serialize deterministically", {
  dataset <- new_benchmark_dataset("filter-data", loader = function() c(x = 3))
  a <- new_benchmark_spec("a-numerical", "numerical", dataset, identity, c(x = 3))
  b <- new_benchmark_spec("b-performance", "performance", dataset, identity, c(x = 3))
  registry <- new_benchmark_registry(list(b, a))
  suite <- run_benchmark_suite(registry, categories = "numerical")
  expect_equal(benchmark_suite_table(suite)$id, "a-numerical")
  path <- tempfile(fileext = ".rds")
  save_benchmark_suite(suite, path)
  expect_identical(read_benchmark_suite(path), suite)
})

test_that("malformed benchmark contracts are rejected", {
  expect_error(new_benchmark_dataset("x", loader = 1), "loader")
  dataset <- new_benchmark_dataset("x", loader = function() 1)
  expect_error(new_benchmark_spec("x", "numerical", dataset, runner = 1), "runner")
  spec <- new_benchmark_spec("x", "numerical", dataset, identity, 1)
  expect_error(new_benchmark_registry(list(spec, spec)), "duplicate")
  expect_error(run_benchmark_suite(new_benchmark_registry()), "no benchmarks")
})
