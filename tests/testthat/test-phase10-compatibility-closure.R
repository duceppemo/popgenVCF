phase10_2_closure_fixture <- function() {
  descriptor <- phase10_api_descriptor()
  compatibility <- compare_phase10_api_descriptors(descriptor, descriptor)
  guidance <- data.frame(
    operation_id = character(), action = character(),
    successor_operation = character(), schema_guidance = character(),
    deprecated_in = character(), removal_not_before = character(),
    stringsAsFactors = FALSE
  )
  policy <- phase10_api_evolution_policy()
  migration_plan <- new_phase10_api_migration_plan(compatibility, guidance, policy)
  identities <- do.call(rbind, lapply(
    c("package", "container", "apptainer", "documentation", "scientific_validation"),
    function(channel) new_phase10_release_identity(
      channel, "0.9.0", paste0("sha256:", channel), descriptor$fingerprint
    )
  ))
  conformance <- new_phase10_release_conformance(
    descriptor, compatibility, migration_plan, identities, policy
  )
  list(
    descriptor = descriptor,
    compatibility = compatibility,
    migration_plan = migration_plan,
    conformance = conformance,
    policy = policy
  )
}

test_that("Phase 10.2 closure audit binds the complete evidence chain", {
  x <- phase10_2_closure_fixture()
  audit_a <- phase10_2_audit_compatibility_closure(
    x$descriptor, x$compatibility, x$migration_plan, x$conformance, x$policy
  )
  audit_b <- phase10_2_audit_compatibility_closure(
    x$descriptor, x$compatibility, x$migration_plan, x$conformance, x$policy
  )

  expect_s3_class(audit_a, "PopgenVCFPhase10CompatibilityClosureAudit")
  expect_true(audit_a$passed)
  expect_identical(audit_a$fingerprint, audit_b$fingerprint)
  expect_length(audit_a$milestone_commits, 3L)
  expect_identical(
    audit_a$release_channels,
    sort(c("package", "container", "apptainer", "documentation", "scientific_validation"))
  )
})

test_that("Phase 10.2 closure audit fails closed on evidence drift", {
  x <- phase10_2_closure_fixture()
  x$compatibility$candidate_fingerprint <- "drifted"
  x$compatibility$fingerprint <- phase10_public_fingerprint(x$compatibility)

  expect_error(
    phase10_2_audit_compatibility_closure(
      x$descriptor, x$compatibility, x$migration_plan, x$conformance, x$policy
    ),
    "not bound"
  )
})

test_that("Phase 10.2 closure evidence is deterministic and fail closed", {
  x <- phase10_2_closure_fixture()
  audit <- phase10_2_audit_compatibility_closure(
    x$descriptor, x$compatibility, x$migration_plan, x$conformance, x$policy
  )
  closure_a <- phase10_2_closure_evidence(audit, "ci::phase10.2::green")
  closure_b <- phase10_2_closure_evidence(audit, "ci::phase10.2::green")

  expect_true(closure_a$closure_approved)
  expect_identical(closure_a$fingerprint, closure_b$fingerprint)
  expect_identical(closure_a$next_milestone, "0.9.1-publication-report-rendering")

  blocked <- phase10_2_closure_evidence(
    audit, "ci::phase10.2::green", c("documentation", "documentation")
  )
  expect_false(blocked$closure_approved)
  expect_identical(blocked$unresolved_blockers, "documentation")
})

test_that("Phase 10.2 closure detects mutation and renders a report", {
  x <- phase10_2_closure_fixture()
  audit <- phase10_2_audit_compatibility_closure(
    x$descriptor, x$compatibility, x$migration_plan, x$conformance, x$policy
  )
  closure <- phase10_2_closure_evidence(audit, "ci::phase10.2::green")
  report <- phase10_2_closure_report(closure)

  expect_match(report[[1L]], "Phase 10.2 compatibility closure review", fixed = TRUE)
  expect_true(any(grepl("0.9.1-publication-report-rendering", report, fixed = TRUE)))

  closure$next_milestone <- "unexpected"
  expect_error(phase10_2_closure_report(closure), "fingerprint mismatch")
})

test_that("Phase 10.2 closure rejects a mutated audit", {
  x <- phase10_2_closure_fixture()
  audit <- phase10_2_audit_compatibility_closure(
    x$descriptor, x$compatibility, x$migration_plan, x$conformance, x$policy
  )
  audit$release_version <- "9.9.9"
  expect_error(
    phase10_2_closure_evidence(audit, "ci::phase10.2::green"),
    "audit fingerprint mismatch"
  )
})
