test_that("runtime schema registry is complete and deterministic", {
  versions <- runtime_schema_versions()

  expect_type(versions, "integer")
  expect_identical(
    names(versions),
    c(
      "execution_plan", "execution_ledger", "attempt_ledger", "checkpoint",
      "scheduler_metadata", "resource_policy", "process_result",
      "process_workspace", "lifecycle_events"
    )
  )
  expect_true(all(versions == 1L))
  expect_identical(runtime_schema_versions(), versions)
})

test_that("runtime schemas classify current, legacy, and future versions", {
  expect_identical(classify_runtime_schema("checkpoint", 1L), "current")

  local_mocked_bindings(
    .runtime_schema_versions = c(checkpoint = 2L),
    .package = "popgenVCF"
  )
  expect_identical(classify_runtime_schema("checkpoint", 1L), "legacy")
  expect_identical(classify_runtime_schema("checkpoint", 2L), "current")
  expect_identical(classify_runtime_schema("checkpoint", 3L), "unsupported_future")
})

test_that("runtime schema validation fails closed", {
  expect_error(classify_runtime_schema("unknown", 1L), "unknown runtime schema kind")
  expect_error(classify_runtime_schema("checkpoint", 0L), "positive integer")
  expect_error(validate_runtime_schema("checkpoint", 2L), "unsupported future")

  local_mocked_bindings(
    .runtime_schema_versions = c(checkpoint = 2L),
    .package = "popgenVCF"
  )
  expect_error(validate_runtime_schema("checkpoint", 1L), "explicit migration")
  expect_invisible(validate_runtime_schema("checkpoint", 1L, allow_legacy = TRUE))
})

test_that("runtime schema metadata uses the canonical current version", {
  expect_identical(
    new_runtime_schema_metadata("execution_ledger"),
    list(kind = "execution_ledger", version = 1L)
  )
})
