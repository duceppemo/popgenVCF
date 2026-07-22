test_that("aggregate drift severity uses the maximum class", {
  metric <- function(id, expected, analysis = "pca") {
    new_canonical_baseline_metric(id, "dataset", analysis, expected,
      comparator = "absolute", tolerance = 0.1, version = "1",
      rationale = "Aggregate severity regression test")
  }
  old <- new_canonical_baseline_snapshot("old",
    new_canonical_baseline_registry(list(metric("stable", 1), metric("breaking", 1))),
    "2026-07-01")
  new <- new_canonical_baseline_snapshot("new",
    new_canonical_baseline_registry(list(metric("stable", 1), metric("breaking", 2))),
    "2026-07-22")
  assessment <- assess_canonical_baseline_drift(old, new)
  expect_identical(assessment$classification, "breaking")
  expect_identical(canonical_drift_summary(assessment)$maximum_classification, "breaking")
})
