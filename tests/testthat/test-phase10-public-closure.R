test_that("Phase 10.1 public surface is complete and deterministic", {
  manifest <- popgenVCF:::phase10_1_public_surface_manifest()
  expect_identical(
    manifest$operation_id,
    c("analysis.execute", "artifact.list", "provenance.inspect", "report.render", "result.inspect")
  )
  expect_false(anyDuplicated(manifest$adapter))

  audit_a <- popgenVCF:::phase10_1_audit_public_surface()
  audit_b <- popgenVCF:::phase10_1_audit_public_surface()
  expect_true(audit_a$passed)
  expect_identical(audit_a$fingerprint, audit_b$fingerprint)
  expect_identical(audit_a$surface, manifest)
})

test_that("Phase 10.1 audit fails closed on missing adapters and exports", {
  manifest <- popgenVCF:::phase10_1_public_surface_manifest()
  symbols <- ls(asNamespace("popgenVCF"), all.names = TRUE)
  exports <- getNamespaceExports("popgenVCF")

  expect_error(
    popgenVCF:::phase10_1_audit_public_surface(
      available_symbols = setdiff(symbols, manifest$adapter[[1L]])
    ),
    "Missing Phase 10.1 adapter"
  )
  expect_error(
    popgenVCF:::phase10_1_audit_public_surface(
      exported_symbols = setdiff(exports, manifest$adapter[[1L]])
    ),
    "Unexported Phase 10.1 adapter"
  )
})

test_that("Phase 10.1 audit rejects registry drift", {
  descriptor <- phase10_api_descriptor()
  descriptor$operations <- descriptor$operations[-1L, , drop = FALSE]
  descriptor$fingerprint <- phase10_public_fingerprint(
    within(descriptor, rm(fingerprint))
  )
  expect_error(
    popgenVCF:::phase10_1_audit_public_surface(descriptor = descriptor),
    "does not match"
  )
})

test_that("Phase 10.1 closure evidence is deterministic and fail closed", {
  closure_a <- popgenVCF:::phase10_1_closure_evidence("ci::phase10.1::green")
  closure_b <- popgenVCF:::phase10_1_closure_evidence("ci::phase10.1::green")
  expect_true(closure_a$closure_approved)
  expect_identical(closure_a$fingerprint, closure_b$fingerprint)
  expect_identical(closure_a$next_milestone, "10.2")
  expect_length(closure_a$milestone_commits, 5L)

  blocked <- popgenVCF:::phase10_1_closure_evidence(
    "ci::phase10.1::green",
    unresolved_blockers = c("documentation", "documentation")
  )
  expect_false(blocked$closure_approved)
  expect_identical(blocked$unresolved_blockers, "documentation")
})

test_that("Phase 10.1 closure detects tampering and renders a report", {
  closure <- popgenVCF:::phase10_1_closure_evidence("ci::phase10.1::green")
  report <- popgenVCF:::phase10_1_closure_report(closure)
  expect_match(report[[1L]], "Phase 10.1 closure review", fixed = TRUE)
  expect_true(any(grepl("Next milestone: `10.2`", report, fixed = TRUE)))

  closure$next_milestone <- "11.0"
  expect_error(
    popgenVCF:::phase10_1_closure_report(closure),
    "fingerprint mismatch"
  )
})
