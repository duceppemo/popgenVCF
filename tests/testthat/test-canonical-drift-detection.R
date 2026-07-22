drift_metric <- function(id = "m", expected = 1, tolerance = 0.1,
                         version = "1", comparator = "absolute",
                         dataset_id = "dataset", analysis = "pca") {
  new_canonical_baseline_metric(id = id, dataset_id = dataset_id,
    analysis = analysis, expected = expected, comparator = comparator,
    tolerance = tolerance, version = version,
    rationale = "Regression-test tolerance")
}

drift_snapshot <- function(id, metrics) {
  new_canonical_baseline_snapshot(id,
    new_canonical_baseline_registry(metrics), paste0("2026-07-", id))
}

test_that("drift threshold profiles validate ordered boundaries", {
  expect_s3_class(new_canonical_drift_profile(), "PopgenVCFCanonicalDriftProfile")
  expect_error(new_canonical_drift_profile(2, 1, 5), "increasing")
  expect_error(new_canonical_drift_profile(1, 1, 5), "unique")
})

test_that("drift classes are stable minor moderate major and breaking", {
  previous <- drift_snapshot("01", list(drift_metric()))
  classify <- function(value) {
    current <- drift_snapshot("02", list(drift_metric(expected = value, version = "2")))
    assess_canonical_baseline_drift(previous, current)$classification
  }
  expect_identical(classify(1), "stable")
  expect_identical(classify(1.05), "minor")
  expect_identical(classify(1.15), "moderate")
  expect_identical(classify(1.35), "major")
  expect_identical(classify(1.7), "breaking")
})

test_that("classification boundaries tolerate floating-point representation", {
  previous <- drift_snapshot("01", list(drift_metric()))
  current <- drift_snapshot("02", list(drift_metric(expected = 1.1, version = "2")))
  table <- canonical_drift_table(assess_canonical_baseline_drift(previous, current))
  expect_identical(table$classification, "minor")
  expect_equal(table$normalized_drift, 1, tolerance = 1e-12)
})

test_that("set and exact changes are stable or breaking", {
  old <- drift_snapshot("01", list(
    drift_metric("set", c("a", "b"), 0, comparator = "set"),
    drift_metric("exact", "x", 0, comparator = "exact")))
  unchanged <- drift_snapshot("02", list(
    drift_metric("set", c("b", "a"), 0, version = "2", comparator = "set"),
    drift_metric("exact", "x", 0, version = "2", comparator = "exact")))
  changed <- drift_snapshot("03", list(
    drift_metric("set", c("a", "c"), 0, version = "3", comparator = "set"),
    drift_metric("exact", "y", 0, version = "3", comparator = "exact")))
  expect_true(all(canonical_drift_table(assess_canonical_baseline_drift(old, unchanged))$classification == "stable"))
  expect_true(all(canonical_drift_table(assess_canonical_baseline_drift(unchanged, changed))$classification == "breaking"))
})

test_that("added and removed metrics fail closed as breaking", {
  old <- drift_snapshot("01", list(drift_metric("a"), drift_metric("removed")))
  new <- drift_snapshot("02", list(drift_metric("a", version = "2"), drift_metric("added", version = "2")))
  table <- canonical_drift_table(assess_canonical_baseline_drift(old, new))
  expect_identical(table$metric_id, c("a", "added", "removed"))
  expect_identical(table$classification, c("stable", "breaking", "breaking"))
  expect_match(table$detail[table$metric_id == "added"], "added")
  expect_match(table$detail[table$metric_id == "removed"], "removed")
})

test_that("history records ordered transitions and cumulative drift", {
  snapshots <- list(
    drift_snapshot("01", list(drift_metric(expected = 1, version = "1"))),
    drift_snapshot("02", list(drift_metric(expected = 1.05, version = "2"))),
    drift_snapshot("03", list(drift_metric(expected = 1.15, version = "3"))))
  history <- canonical_drift_history(snapshots)
  expect_s3_class(history, "PopgenVCFCanonicalDriftHistory")
  expect_identical(history$table$transition, c(1L, 2L))
  expect_identical(history$table$from_snapshot, c("01", "02"))
  expect_equal(history$cumulative$cumulative_normalized_drift, 1.5, tolerance = 1e-12)
  expect_error(canonical_drift_history(list(snapshots[[1]])), "at least two")
})

test_that("dataset and analysis summaries are deterministic", {
  old <- drift_snapshot("01", list(
    drift_metric("b", expected = 1, analysis = "fst"),
    drift_metric("a", expected = 1, analysis = "pca")))
  new <- drift_snapshot("02", list(
    drift_metric("b", expected = 1.7, version = "2", analysis = "fst"),
    drift_metric("a", expected = 1.05, version = "2", analysis = "pca")))
  summary <- canonical_drift_summary(assess_canonical_baseline_drift(old, new))
  expect_identical(summary$analysis, c("fst", "pca"))
  expect_identical(summary$maximum_classification, c("breaking", "minor"))
})

test_that("drift evidence is deterministic and complete", {
  old <- drift_snapshot("01", list(drift_metric()))
  new <- drift_snapshot("02", list(drift_metric(expected = 1.05, version = "2")))
  assessment <- assess_canonical_baseline_drift(old, new)
  out <- tempfile(); paths <- write_canonical_drift_evidence(assessment, out)
  expect_identical(names(paths), c("metrics", "summary", "json", "methods"))
  expect_true(all(file.exists(paths)))
  expect_match(readLines(paths[["methods"]]), "1 metric transition")
  json <- jsonlite::read_json(paths[["json"]], simplifyVector = TRUE)
  expect_identical(json$schema_version, "1.0")
})
