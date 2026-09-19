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

test_that("closure assembly rejects a non-scalar identity instead of silently flattening it away", {
  # c(release_readiness_id, migration_registry_id, ...) flattens four
  # separate scalar arguments into one plain atomic vector --
  # lengths() on an atomic vector reports 1 for every element
  # unconditionally, so `any(lengths(scalar_ids) != 1L)` could never
  # actually catch a caller passing e.g.
  # release_readiness_id = c("a", "b"): it silently flattened into the
  # vector's own extra positions instead of being rejected. Note: this
  # specific call path was already caught end-to-end pre-fix too, by a
  # different, downstream check inside phase9_closure_review() itself
  # (already correctly compensated) -- this test still confirms the
  # (now also fixed) scalar_ids check in phase9_closure_evidence.R
  # itself rejects it directly, at the layer it belongs to, rather than
  # relying on that separate downstream catch.
  expect_error(
    phase9_assemble_closure(
      release_readiness_id = c("release-a", "release-b"),
      migration_registry_id = "migration-1",
      deprecation_portfolio_id = "deprecation-1",
      ci_evidence_id = "ci-1"
    )
  )
})

test_that("phase9_closure_review verifies a caller-supplied fingerprint instead of trusting it verbatim", {
  # Same fix as phase9_roadmap_handoff() below: a caller-supplied
  # fingerprint was previously accepted via %||% with no verification.
  review_args <- list(
    milestone_commits = popgenVCF:::.phase9_milestone_commits,
    evidence_ids = popgenVCF:::.phase9_closure_evidence_ids,
    release_readiness_id = "release-ready-1",
    migration_registry_id = "migration-1",
    deprecation_portfolio_id = "deprecation-1",
    ci_evidence_id = "ci-1"
  )
  real <- do.call(popgenVCF:::phase9_closure_review, review_args)
  expect_identical(
    do.call(popgenVCF:::phase9_closure_review, modifyList(review_args, list(
      fingerprint = real$fingerprint
    )))$fingerprint,
    real$fingerprint
  )
  expect_error(
    do.call(popgenVCF:::phase9_closure_review, modifyList(review_args, list(
      fingerprint = "stale-or-forged-fingerprint"
    ))),
    "does not match"
  )
})

test_that("phase9_roadmap_handoff rejects a non-scalar identity, and verifies a caller-supplied fingerprint instead of trusting it verbatim", {
  # Same vacuous-lengths()-after-c()-flattening bug as
  # phase9_assemble_closure() above, in the sibling constructor.
  valid_args <- list(
    closure_review_id = "review-1", next_phase_id = "10.1",
    goal = "goal", scope_boundary = "scope",
    dependency_ids = "dep-1", entry_criteria = "criterion-1"
  )
  expect_error(
    do.call(popgenVCF:::phase9_roadmap_handoff, modifyList(valid_args, list(
      closure_review_id = c("review-a", "review-b")
    ))),
    "Closure, phase, goal, and scope identities are required"
  )

  # A caller-supplied fingerprint was previously accepted verbatim (via
  # %||%) with no verification against the record's real content -- an
  # arbitrary or stale fingerprint could be stamped onto the record,
  # defeating the point of a fingerprint as a self-consistency check.
  real <- do.call(popgenVCF:::phase9_roadmap_handoff, valid_args)
  expect_identical(
    do.call(popgenVCF:::phase9_roadmap_handoff, modifyList(valid_args, list(
      fingerprint = real$fingerprint
    )))$fingerprint,
    real$fingerprint
  )
  expect_error(
    do.call(popgenVCF:::phase9_roadmap_handoff, modifyList(valid_args, list(
      fingerprint = "stale-or-forged-fingerprint"
    ))),
    "does not match"
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
