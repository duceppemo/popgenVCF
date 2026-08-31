make_release_record <- function(release, value, created_at = "2026-01-01 UTC") {
  new_release_benchmark_record(
    release = release,
    package_version = sub("^v", "", release),
    git_sha = paste0("sha-", release),
    created_at = created_at,
    components = list(validation = data.frame(metric = "x", value = value)),
    provenance = list(source = "fixture")
  )
}

test_that("release comparisons report shared and unique components", {
  baseline <- make_release_record("v0.9.0", 1)
  current <- make_release_record("v0.10.0", 2)
  current$components$extra <- data.frame(x = 1)
  current$component_digests <- vapply(
    current$components, digest::digest, character(1L),
    algo = "sha256", serialize = TRUE
  )

  comparison <- compare_release_benchmarks(current, baseline)
  expect_s3_class(comparison, "PopgenVCFReleaseComparison")
  expect_equal(comparison$status, "passed")
  expect_equal(comparison$current_only, "extra")
  expect_equal(comparison$details$status, "changed")

  tab <- release_comparison_table(comparison)
  expect_equal(names(tab)[1:3], c(
    "current_release", "baseline_release", "overall_status"
  ))
})

test_that("a validation component's pass/fail regression flips the overall comparison to failed, not the uninformative 'changed'", {
  # Real bug found in a pre-release audit: the digest-comparison branch only
  # ever emits "unchanged"/"changed", and "changed" was never in the set
  # that flips overall status to "failed" -- so a genuine validation
  # regression (scripts/build_release_benchmark_archive.R archives
  # run_scientific_validation()'s own $checks data.table, with a logical
  # `passed` column, as exactly this kind of digest-compared component) was
  # silently reported as an overall "passed" comparison, defeating that
  # script's own release-gate stop() (which only fires on status == "failed").
  make_checks_record <- function(release, all_passed) {
    checks <- data.table::data.table(
      check = c("check_a", "check_b"),
      passed = c(TRUE, all_passed)
    )
    new_release_benchmark_record(
      release = release, package_version = sub("^v", "", release),
      git_sha = paste0("sha-", release),
      components = list(scientific_validation = checks),
      provenance = list(source = "fixture")
    )
  }
  baseline <- make_checks_record("v0.9.0", all_passed = TRUE)
  current <- make_checks_record("v0.10.0", all_passed = FALSE)

  comparison <- compare_release_benchmarks(current, baseline)
  expect_equal(comparison$status, "failed")
  expect_equal(
    comparison$details$status[comparison$details$component == "scientific_validation"],
    "failed"
  )
})

test_that("a component's non-regressing content change still stays 'changed', not a false 'failed'", {
  # A digest change alone (this fixture's plain data.frame `value` column, or
  # a validation component whose checks still all pass despite different
  # content) is legitimate benchmark evolution, not a regression -- must not
  # become an over-eager blanket "any change fails" policy.
  make_checks_record <- function(release, note) {
    checks <- data.table::data.table(
      check = c("check_a", "check_b"), passed = c(TRUE, TRUE), note = note
    )
    new_release_benchmark_record(
      release = release, package_version = sub("^v", "", release),
      git_sha = paste0("sha-", release),
      components = list(scientific_validation = checks),
      provenance = list(source = "fixture")
    )
  }
  baseline <- make_checks_record("v0.9.0", "old note")
  current <- make_checks_record("v0.10.0", "new note")

  comparison <- compare_release_benchmarks(current, baseline)
  expect_equal(comparison$status, "passed")
  expect_equal(comparison$details$status, "changed")
})

test_that("latest release selection uses semantic versions", {
  archive <- new_benchmark_archive()
  archive <- register_release_benchmark(archive, make_release_record("v0.9.0", 1))
  archive <- register_release_benchmark(archive, make_release_record("v0.10.0", 2))
  expect_equal(latest_release_benchmark(archive)$release, "v0.10.0")
  expect_equal(latest_release_benchmark(archive, exclude = "v0.10.0")$release, "v0.9.0")
})

test_that("regression reports support source-only generation", {
  archive <- new_benchmark_archive(list(
    make_release_record("v0.9.0", 1),
    make_release_record("v0.10.0", 2)
  ))
  comparison <- compare_release_benchmarks(
    get_release_benchmark(archive, "v0.10.0"),
    get_release_benchmark(archive, "v0.9.0")
  )
  output <- tempfile("regression-report-")
  files <- write_regression_report(
    archive, output, comparison = comparison, render = FALSE
  )
  expect_false(files$rendered)
  expect_true(all(file.exists(c(
    files$qmd, files$releases, files$comparison, files$data
  ))))
  source <- readLines(files$qmd, warn = FALSE)
  expect_equal(source[[1L]], "---")
  expect_true(any(grepl("Current versus baseline", source, fixed = TRUE)))
})

test_that("invalid report and comparison inputs fail clearly", {
  expect_error(
    compare_release_benchmarks(list(), list()),
    "PopgenVCFReleaseBenchmarkRecord"
  )
  expect_error(latest_release_benchmark(new_benchmark_archive()), "no eligible")
  expect_error(write_regression_report(list(), tempfile(), render = FALSE), "invalid")
})
