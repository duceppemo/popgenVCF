gate_validation_result <- function(status = "pass") {
  structure(list(schema_version = "1.0", suite_id = "suite", title = "Suite",
    fail_fast = TRUE, results = list(dataset = list(status = status,
      verification = NULL, validation = NULL, error = NA_character_, elapsed_seconds = 0)),
    elapsed_seconds = 0), class = "PopgenVCFCanonicalValidationSuiteResult")
}

gate_baseline_result <- function(passed = TRUE) {
  structure(list(schema_version = "1.0", table = data.frame(), passed = passed),
            class = "PopgenVCFCanonicalBaselineResult")
}

gate_drift_result <- function(classification = "stable") {
  structure(list(schema_version = "1.0", previous_id = "v1", current_id = "v2",
    profile = new_canonical_drift_profile(), table = data.frame(),
    classification = classification), class = "PopgenVCFCanonicalDriftAssessment")
}

gate_reconciliation <- function(release_ready = TRUE) {
  structure(list(schema_version = "1.0", table = data.frame(),
    missing_expected = data.frame(), release_ready = release_ready),
    class = "PopgenVCFCanonicalChangeReconciliation")
}

test_that("fully conforming canonical release is ready", {
  result <- evaluate_canonical_release_gate("rc-1", gate_validation_result(),
    gate_baseline_result(), gate_drift_result(), gate_reconciliation(),
    evaluated_at = "2026-07-22T00:00:00Z", provenance = list(commit = "abc"))

  expect_true(result$release_ready)
  expect_true(all(canonical_release_gate_table(result)$passed))
  certificate <- canonical_release_certificate(result)
  expect_true(certificate$release_ready)
  expect_equal(certificate$provenance$commit, "abc")
})

test_that("validation and baseline failures block the release", {
  result <- evaluate_canonical_release_gate("rc-2", gate_validation_result("fail"),
    gate_baseline_result(FALSE), gate_drift_result(), gate_reconciliation(),
    evaluated_at = "2026-07-22T00:00:00Z")

  expect_false(result$release_ready)
  expect_equal(result$blocking_reasons$component, c("validation", "baselines"))
})

test_that("authorized non-stable drift passes the drift gate", {
  result <- evaluate_canonical_release_gate("rc-3", gate_validation_result(),
    gate_baseline_result(), gate_drift_result("major"), gate_reconciliation(TRUE),
    evaluated_at = "2026-07-22T00:00:00Z")

  expect_true(result$release_ready)
  drift_row <- canonical_release_gate_table(result)
  drift_row <- drift_row[drift_row$component == "drift", ]
  expect_true(drift_row$passed)
  expect_match(drift_row$detail, "formally reconciled")
})

test_that("unreconciled drift fails closed", {
  result <- evaluate_canonical_release_gate("rc-4", gate_validation_result(),
    gate_baseline_result(), gate_drift_result("minor"), gate_reconciliation(FALSE),
    evaluated_at = "2026-07-22T00:00:00Z")

  expect_false(result$release_ready)
  expect_setequal(result$blocking_reasons$component, c("drift", "reconciliation"))
})

test_that("missing required evidence blocks and optional evidence does not", {
  blocked <- evaluate_canonical_release_gate("rc-5", evaluated_at = "2026-07-22")
  expect_false(blocked$release_ready)
  expect_equal(nrow(blocked$blocking_reasons), 4L)

  policy <- new_canonical_release_gate_policy(require_drift = FALSE,
    require_reconciliation = FALSE)
  ready <- evaluate_canonical_release_gate("rc-6", gate_validation_result(),
    gate_baseline_result(), policy = policy, evaluated_at = "2026-07-22")
  expect_true(ready$release_ready)
})

test_that("empty validation suites fail closed", {
  empty <- structure(list(schema_version = "1.0", suite_id = "empty", title = "Empty",
    fail_fast = TRUE, results = list(), elapsed_seconds = 0),
    class = "PopgenVCFCanonicalValidationSuiteResult")
  result <- evaluate_canonical_release_gate("rc-empty", empty, gate_baseline_result(),
    gate_drift_result(), gate_reconciliation(), evaluated_at = "2026-07-22")
  expect_false(result$release_ready)
  expect_match(result$blocking_reasons$detail[1], "no executed datasets")
})

test_that("release gate evidence is deterministic and complete", {
  result <- evaluate_canonical_release_gate("rc-evidence", gate_validation_result(),
    gate_baseline_result(), gate_drift_result(), gate_reconciliation(),
    evaluated_at = "2026-07-22T00:00:00Z")
  output <- write_canonical_release_gate_evidence(result, tempfile("release-gate-"))
  expect_true(all(file.exists(output)))
  expect_setequal(names(output), c("components", "blocking", "certificate", "report"))
  expect_match(readLines(output[["report"]]), "READY")
  certificate <- jsonlite::read_json(output[["certificate"]], simplifyVector = TRUE)
  expect_true(certificate$release_ready)
})

test_that("release gate policy validates logical values", {
  expect_error(new_canonical_release_gate_policy(require_validation = NA),
               "non-missing logicals")
  expect_error(evaluate_canonical_release_gate("x", evaluated_at = "2026-07-22",
    provenance = list("unnamed")), "named list")
})
