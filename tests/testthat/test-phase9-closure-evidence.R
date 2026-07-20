test_that("Phase 9 milestone and evidence manifests are canonical", {
  milestones <- phase9_milestone_manifest()
  evidence <- phase9_closure_evidence_manifest()

  expect_identical(milestones$milestone_id, c(
    "9.1", "9.2", "9.3", "9.4", "9.5", "9.6", "9.7",
    "9.8", "9.9", "9.10", "9.11", "9.12", "9.13", "9.14"
  ))
  expect_true(all(grepl("^[0-9a-f]{40}$", milestones$merge_commit)))
  expect_setequal(evidence$evidence_domain, .phase9_required_evidence_domains)
})

test_that("closure assembly remains fail closed", {
  bundle <- phase9_assemble_closure(
    release_readiness_id = "release-ready-1",
    migration_registry_id = "migration-1",
    deprecation_portfolio_id = "deprecation-1",
    ci_evidence_id = "ci-1",
    release_ready = TRUE,
    unresolved_blockers = "benchmark matrix pending"
  )

  expect_false(bundle$closure_review$closure_approved)
  expect_identical(
    bundle$closure_review$unresolved_blockers,
    "benchmark matrix pending"
  )
})

test_that("closure assembly approves complete evidence deterministically", {
  args <- list(
    release_readiness_id = "release-ready-1",
    migration_registry_id = "migration-1",
    deprecation_portfolio_id = "deprecation-1",
    ci_evidence_id = "ci-1",
    release_ready = TRUE
  )

  first <- do.call(phase9_assemble_closure, args)
  second <- do.call(phase9_assemble_closure, rev(args))

  expect_true(first$closure_review$closure_approved)
  expect_identical(first$fingerprint, second$fingerprint)
  expect_identical(first$roadmap_handoff$next_phase_id, "10.1")
  expect_match(
    paste(phase9_closure_report(first), collapse = "\n"),
    "Closure approved: `TRUE`"
  )
})

test_that("closure assembly requires all evidence identities", {
  expect_error(
    phase9_assemble_closure(
      release_readiness_id = "",
      migration_registry_id = "migration-1",
      deprecation_portfolio_id = "deprecation-1",
      ci_evidence_id = "ci-1"
    ),
    "All closure assembly identities are required"
  )
})
