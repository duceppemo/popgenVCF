change_metric <- function(id = "m1", expected = 10, version = "1") {
  new_canonical_baseline_metric(id, "dataset", "analysis", expected,
    comparator = "absolute", tolerance = 1, version = version,
    rationale = "synthetic governance test")
}

change_assessment <- function(old = 10, new = 11.5) {
  a <- new_canonical_baseline_snapshot("v1",
    new_canonical_baseline_registry(list(change_metric(expected = old, version = "1"))),
    "2026-01-01")
  b <- new_canonical_baseline_snapshot("v2",
    new_canonical_baseline_registry(list(change_metric(expected = new, version = "2"))),
    "2026-02-01")
  assess_canonical_baseline_drift(a, b)
}

test_that("change request validation and lifecycle are explicit", {
  request <- new_canonical_change_request("CR-1", "m1", c(m1 = "moderate"),
    "Method correction changes this metric", requested_by = "scientist")
  expect_equal(request$status, "pending")
  registry <- new_canonical_change_registry(list(request))
  registry <- set_canonical_change_status(registry, "cr-1", "approved", "reviewer", "2026-02-02")
  expect_equal(registry$requests[["cr-1"]]$status, "approved")
  expect_error(new_canonical_change_request("bad", "m1", c(other = "minor"),
    "bad scope", requested_by = "scientist"), "every metric")
  expect_error(new_canonical_change_request("bad", "m1", c(m1 = "minor"),
    "bad decision", status = "approved", requested_by = "scientist"), "required")
})

test_that("approved drift reconciles and excessive drift blocks release", {
  approved <- new_canonical_change_request("cr-1", "m1", c(m1 = "moderate"),
    "Expected algorithmic correction", status = "approved", requested_by = "scientist",
    decided_by = "reviewer", decided_at = "2026-02-02")
  registry <- new_canonical_change_registry(list(approved))
  result <- reconcile_canonical_changes(change_assessment(new = 11.5), registry)
  expect_equal(result$table$reconciliation, "approved_change")
  expect_true(result$release_ready)

  result <- reconcile_canonical_changes(change_assessment(new = 16), registry)
  expect_equal(result$table$reconciliation, "exceeds_approval")
  expect_false(result$release_ready)
})

test_that("unapproved drift and missing expected changes are detected", {
  empty <- new_canonical_change_registry()
  result <- reconcile_canonical_changes(change_assessment(new = 11.5), empty)
  expect_equal(result$table$reconciliation, "unexpected_change")
  expect_false(result$release_ready)

  approved <- new_canonical_change_request("cr-2", "m1", c(m1 = "minor"),
    "Expected recalibration", status = "approved", requested_by = "scientist",
    decided_by = "reviewer", decided_at = "2026-02-02")
  result <- reconcile_canonical_changes(change_assessment(new = 10),
    new_canonical_change_registry(list(approved)))
  expect_equal(result$table$reconciliation, "approved_change")
  expect_equal(result$missing_expected$metric_id, "m1")
  expect_false(result$release_ready)
})

test_that("rejected and superseded requests do not authorize drift", {
  rejected <- new_canonical_change_request("cr-r", "m1", c(m1 = "breaking"),
    "Rejected proposal", status = "rejected", requested_by = "scientist",
    decided_by = "reviewer", decided_at = "2026-02-02")
  superseded <- new_canonical_change_request("cr-s", "m1", c(m1 = "breaking"),
    "Obsolete proposal", status = "superseded", requested_by = "scientist",
    decided_by = "reviewer", decided_at = "2026-02-03")
  result <- reconcile_canonical_changes(change_assessment(),
    new_canonical_change_registry(list(rejected, superseded)))
  expect_equal(result$table$reconciliation, "unexpected_change")
})

test_that("a newer, stricter approval governs over an older, more permissive one for the same metric, via supersedes", {
  # Real bug found in a pre-release audit: when two requests both have
  # status = "approved" for the same metric (a real supersession scenario --
  # a stricter approval later replaces an earlier, more permissive one
  # without necessarily flipping the old one's own status), the old code
  # picked the governing request via candidates[[length(candidates)]], where
  # candidates is built from registry$requests -- kept ALPHABETICALLY sorted
  # by id, not by decision date or supersession. "req-10" (approved later,
  # stricter) sorts BEFORE "req-9" (approved earlier, permissive) because
  # '1' < '9', so the OLDER, more permissive request would silently govern
  # instead -- exactly the reverse of what supersedes is for. A real
  # "major" drift that the newer policy should block would instead be
  # judged "approved_change" under the stale "major"-permitting approval.
  permissive <- new_canonical_change_request("req-9", "m1", c(m1 = "major"),
    "Original, permissive approval", status = "approved", requested_by = "scientist",
    decided_by = "reviewer", decided_at = "2026-02-01")
  stricter <- new_canonical_change_request("req-10", "m1", c(m1 = "minor"),
    "Tightened, superseding approval", status = "approved", requested_by = "scientist",
    decided_by = "reviewer", decided_at = "2026-02-10", supersedes = "req-9")
  registry <- new_canonical_change_registry(list(permissive, stricter))
  # Confirm the alphabetical-sort trap this bug relied on is real: "req-10"
  # sorts before "req-9" in the registry's own stored order.
  expect_identical(names(registry$requests), c("req-10", "req-9"))

  result <- reconcile_canonical_changes(change_assessment(new = 13), registry)
  expect_equal(result$table$request_id, "req-10")
  expect_equal(result$table$reconciliation, "exceeds_approval")
  expect_false(result$release_ready)
})

test_that("the most recently decided approval governs when no supersedes chain resolves the tie", {
  earlier <- new_canonical_change_request("cr-a", "m1", c(m1 = "moderate"),
    "Earlier approval", status = "approved", requested_by = "scientist",
    decided_by = "reviewer", decided_at = "2026-02-02")
  later <- new_canonical_change_request("cr-b", "m1", c(m1 = "minor"),
    "Later approval, no explicit supersedes link", status = "approved",
    requested_by = "scientist", decided_by = "reviewer", decided_at = "2026-02-15")
  registry <- new_canonical_change_registry(list(earlier, later))

  result <- reconcile_canonical_changes(change_assessment(new = 11.5), registry)
  expect_equal(result$table$request_id, "cr-b")
})

test_that("evidence and release summary are deterministic", {
  approved <- new_canonical_change_request("cr-1", "m1", c(m1 = "moderate"),
    "Expected correction", status = "approved", requested_by = "scientist",
    decided_by = "reviewer", decided_at = "2026-02-02",
    provenance = list(issue = "274"))
  result <- reconcile_canonical_changes(change_assessment(),
    new_canonical_change_registry(list(approved)))
  summary <- canonical_change_summary(result)
  expect_true(summary$release_ready)
  out <- write_canonical_change_evidence(result, tempfile("change-evidence-"))
  expect_true(all(file.exists(out)))
  expect_match(readLines(out[["methods"]]), "Release ready: TRUE", fixed = TRUE)
})
