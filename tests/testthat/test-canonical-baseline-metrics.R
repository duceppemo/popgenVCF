baseline_metric <- function(id, expected, comparator = "exact", tolerance = 0,
                            dataset_id = "dataset_a", analysis = "pca") {
  new_canonical_baseline_metric(
    id = id, dataset_id = dataset_id, analysis = analysis,
    expected = expected, comparator = comparator, tolerance = tolerance,
    version = "2026.1", rationale = "Regression tolerance justified by deterministic fixture",
    provenance = list(generator = "testthat")
  )
}

test_that("baseline registries are deterministic and reject duplicates", {
  registry <- new_canonical_baseline_registry()
  registry <- register_canonical_baseline_metric(registry, baseline_metric("z", 2))
  registry <- register_canonical_baseline_metric(registry, baseline_metric("a", 1))
  expect_equal(names(registry$metrics), c("a", "z"))
  expect_error(register_canonical_baseline_metric(registry, baseline_metric("a", 1)),
               "already registered")
})

test_that("exact and set comparators preserve type and membership semantics", {
  exact <- baseline_metric("exact", 1L)
  expect_true(compare_canonical_baseline_metric(exact, 1L)$passed)
  expect_false(compare_canonical_baseline_metric(exact, 1)$passed)

  set <- baseline_metric("set", c("A", "B"), comparator = "set")
  expect_true(compare_canonical_baseline_metric(set, c("B", "A"))$passed)
  failed <- compare_canonical_baseline_metric(set, c("A", "C"))
  expect_false(failed$passed)
  expect_equal(failed$deviation, 2)
})

test_that("absolute and relative tolerance boundaries are inclusive", {
  absolute <- baseline_metric("absolute", c(1, 2), "absolute", 0.1)
  expect_true(compare_canonical_baseline_metric(absolute, c(1.1, 1.9))$passed)
  expect_false(compare_canonical_baseline_metric(absolute, c(1.1001, 2))$passed)

  relative <- baseline_metric("relative", c(10, 20), "relative", 0.05)
  expect_true(compare_canonical_baseline_metric(relative, c(10.5, 19))$passed)
  expect_false(compare_canonical_baseline_metric(relative, c(10.51, 20))$passed)
})

test_that("distribution comparison uses normalized decile deviation", {
  distribution <- baseline_metric("distribution", 1:100, "distribution", 0.02)
  close <- compare_canonical_baseline_metric(distribution, 1:100 + 1)
  far <- compare_canonical_baseline_metric(distribution, 1:100 + 10)
  expect_true(close$passed)
  expect_false(far$passed)
  expect_true(close$deviation < far$deviation)
})

test_that("evaluation reports missing and failed observations deterministically", {
  registry <- new_canonical_baseline_registry(list(
    baseline_metric("metric_b", 2, "absolute", 0.01, analysis = "fst"),
    baseline_metric("metric_a", 1, "exact", analysis = "pca")
  ))
  result <- evaluate_canonical_baselines(registry, list(metric_a = 1))
  table <- canonical_baseline_table(result)
  expect_equal(table$metric_id, c("metric_b", "metric_a"))
  expect_false(result$passed)
  expect_match(table$detail[table$metric_id == "metric_b"], "missing")
})

test_that("baseline evidence contains deterministic TSV JSON and methods files", {
  registry <- new_canonical_baseline_registry(list(baseline_metric("metric", pi, "absolute", 1e-12)))
  result <- evaluate_canonical_baselines(registry, list(metric = pi))
  paths <- write_canonical_baseline_evidence(result, tempfile())
  expect_true(all(file.exists(paths)))
  table <- data.table::fread(paths[["tsv"]])
  json <- jsonlite::read_json(paths[["json"]], simplifyVector = TRUE)
  expect_true(table$passed)
  expect_true(json$passed)
})

test_that("baseline callbacks integrate with canonical validation suites", {
  directory <- tempfile(); dir.create(directory)
  file <- file.path(directory, "fixture.vcf")
  writeLines("##fileformat=VCFv4.2", file)
  descriptor <- new_canonical_dataset(
    id = "dataset_a", version = "1", title = "Fixture", license = "CC0-1.0",
    citation = "Fixture citation", organism = "Test organism", analyses = "pca",
    files = data.frame(filename = basename(file),
      sha256 = digest::digest(file, algo = "sha256", file = TRUE),
      size_bytes = unname(file.info(file)$size), source = NA_character_)
  )
  datasets <- register_canonical_dataset(
    new_canonical_dataset_registry(), descriptor, approval = "approved",
    reviewed_by = "reviewer", reviewed_at = "2026-07-22")
  baselines <- new_canonical_baseline_registry(list(baseline_metric("sample_count", 3L)))
  validation <- canonical_baseline_validation(baselines,
    function(descriptor, directory) list(sample_count = 3L))
  suite <- register_canonical_validation(
    new_canonical_validation_suite("baseline_suite", "Baseline suite"),
    "dataset_a", directory, validation = validation)
  result <- run_canonical_validation_suite(suite, datasets)
  expect_true(canonical_validation_suite_table(result)$passed)
  expect_equal(canonical_baseline_coverage(baselines)$metrics, 1L)
})
