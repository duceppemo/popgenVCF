test_that("evolution policy is deterministic and tamper evident", {
  first <- phase10_api_evolution_policy()
  second <- phase10_api_evolution_policy()
  expect_identical(first$fingerprint, second$fingerprint)

  first$minimum_deprecation_minor_releases <- 1L
  expect_error(
    popgenVCF:::.phase10_validate_evolution_policy(first),
    "Invalid public API evolution policy"
  )
})

test_that("compatible and additive changes accept simple guidance", {
  baseline <- phase10_api_descriptor("1.0.0")
  candidate <- baseline
  candidate$api_version <- "1.1.0"
  candidate$operations$request_schema[[1L]] <-
    "popgenvcf.public.analysis-request/1.1.0"
  candidate$fingerprint <- phase10_public_fingerprint(candidate)
  compatibility <- compare_phase10_api_descriptors(baseline, candidate)

  guidance <- new_phase10_migration_guidance(
    "analysis.execute", "adopt",
    schema_guidance = "Adopt the optional 1.1 request fields when needed."
  )
  plan <- new_phase10_api_migration_plan(compatibility, guidance)
  expect_identical(plan$classification, "additive")
  expect_true(validate_phase10_api_migration_plan(plan, compatibility))
  expect_true(any(grepl("analysis.execute", phase10_api_migration_report(plan, compatibility))))
})

test_that("deprecated operations require a valid migration schedule", {
  baseline <- phase10_api_descriptor("1.0.0")
  candidate <- baseline
  candidate$api_version <- "1.1.0"
  candidate$operations$lifecycle[[1L]] <- "deprecated"
  candidate$fingerprint <- phase10_public_fingerprint(candidate)
  compatibility <- compare_phase10_api_descriptors(baseline, candidate)

  missing_schedule <- new_phase10_migration_guidance(
    "analysis.execute", "replace",
    successor_operation = "analysis.execute.v2",
    schema_guidance = "Use the successor request envelope."
  )
  expect_error(
    new_phase10_api_migration_plan(compatibility, missing_schedule),
    "requires a removal schedule"
  )

  guidance <- new_phase10_migration_guidance(
    "analysis.execute", "replace",
    successor_operation = "analysis.execute.v2",
    schema_guidance = "Use the successor request envelope.",
    deprecated_in = "1.1.0",
    removal_not_before = "1.3.0"
  )
  plan <- new_phase10_api_migration_plan(compatibility, guidance)
  expect_true(validate_phase10_api_migration_plan(plan, compatibility))
})

test_that("breaking removals require explicit migration guidance", {
  baseline <- phase10_api_descriptor("1.0.0")
  candidate <- baseline
  candidate$api_version <- "2.0.0"
  candidate$operations <- candidate$operations[-1L, , drop = FALSE]
  candidate$fingerprint <- phase10_public_fingerprint(candidate)
  compatibility <- compare_phase10_api_descriptors(baseline, candidate)

  expect_error(
    new_phase10_api_migration_plan(compatibility, candidate$operations[0, ]),
    "Malformed or duplicate migration guidance"
  )

  guidance <- new_phase10_migration_guidance(
    "analysis.execute", "replace",
    successor_operation = "analysis.execute.v2",
    schema_guidance = "Translate the request to the version 2 envelope."
  )
  plan <- new_phase10_api_migration_plan(compatibility, guidance)
  expect_true(validate_phase10_api_migration_plan(plan, compatibility))
})

test_that("migration plans detect tampering and invalid successors", {
  baseline <- phase10_api_descriptor("1.0.0")
  candidate <- baseline
  candidate$api_version <- "1.1.0"
  candidate$operations$lifecycle[[1L]] <- "deprecated"
  candidate$fingerprint <- phase10_public_fingerprint(candidate)
  compatibility <- compare_phase10_api_descriptors(baseline, candidate)

  no_successor <- new_phase10_migration_guidance(
    "analysis.execute", "replace",
    schema_guidance = "Use the replacement operation.",
    deprecated_in = "1.1.0",
    removal_not_before = "1.3.0"
  )
  expect_error(
    new_phase10_api_migration_plan(compatibility, no_successor),
    "requires a successor operation"
  )

  guidance <- new_phase10_migration_guidance(
    "analysis.execute", "migrate",
    schema_guidance = "Update callers before removal.",
    deprecated_in = "1.1.0",
    removal_not_before = "1.3.0"
  )
  plan <- new_phase10_api_migration_plan(compatibility, guidance)
  plan$classification <- "compatible"
  expect_error(
    validate_phase10_api_migration_plan(plan, compatibility),
    "fingerprint verification failed"
  )
})
