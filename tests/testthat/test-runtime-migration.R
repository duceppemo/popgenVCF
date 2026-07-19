test_that("runtime migration registries reject invalid registrations", {
  registry <- new_runtime_migration_registry()
  expect_error(
    register_runtime_migration(registry, "unknown", 1L, 2L, identity, "bad"),
    "unknown runtime schema kind"
  )
  expect_error(
    register_runtime_migration(registry, "execution_plan", 1L, 3L, identity, "skip"),
    "adjacent"
  )
  registry <- register_runtime_migration(
    registry, "execution_plan", 1L, 2L, identity, "plan-v1-v2"
  )
  expect_error(
    register_runtime_migration(
      registry, "execution_plan", 1L, 2L, identity, "duplicate"
    ),
    "already registered"
  )
})

test_that("runtime migration paths are contiguous and fail closed", {
  testthat::local_mocked_bindings(
    .runtime_schema_versions = c(execution_plan = 3L),
    .package = "popgenVCF"
  )
  registry <- new_runtime_migration_registry()
  registry <- register_runtime_migration(
    registry, "execution_plan", 1L, 2L, identity, "plan-v1-v2"
  )
  expect_error(
    runtime_migration_path(registry, "execution_plan", 1L, 3L),
    "no registered runtime migration path"
  )
  registry <- register_runtime_migration(
    registry, "execution_plan", 2L, 3L, identity, "plan-v2-v3"
  )
  path <- runtime_migration_path(registry, "execution_plan", 1L, 3L)
  expect_identical(vapply(path, `[[`, character(1), "id"),
                   c("plan-v1-v2", "plan-v2-v3"))
})

test_that("runtime envelopes migrate deterministically with audit records", {
  testthat::local_mocked_bindings(
    .runtime_schema_versions = c(execution_plan = 2L),
    .package = "popgenVCF"
  )
  payload <- list(modules = "qc")
  legacy <- structure(list(
    kind = "execution_plan",
    schema = list(kind = "execution_plan", version = 1L),
    digest_algorithm = "sha256",
    digest = runtime_payload_digest(payload),
    payload = payload
  ), class = "PopgenVCFRuntimeEnvelope")
  registry <- register_runtime_migration(
    new_runtime_migration_registry(),
    "execution_plan", 1L, 2L,
    function(value) {
      value$schema_marker <- "v2"
      value
    },
    "execution-plan-v1-v2"
  )

  first <- migrate_runtime_integrity_envelope(legacy, registry)
  second <- migrate_runtime_integrity_envelope(legacy, registry)
  expect_s3_class(first, "PopgenVCFRuntimeMigrationResult")
  expect_identical(first$envelope$schema$version, 2L)
  expect_identical(first$envelope$payload$schema_marker, "v2")
  expect_identical(first$record$migration_fingerprint,
                   second$record$migration_fingerprint)
  expect_identical(first$record$source_payload_digest,
                   runtime_payload_digest(payload))
  expect_length(first$record$steps, 1L)
})

test_that("runtime migration rejects missing and nondeterministic steps", {
  testthat::local_mocked_bindings(
    .runtime_schema_versions = c(execution_plan = 2L),
    .package = "popgenVCF"
  )
  payload <- list(modules = "qc")
  legacy <- structure(list(
    kind = "execution_plan",
    schema = list(kind = "execution_plan", version = 1L),
    digest_algorithm = "sha256",
    digest = runtime_payload_digest(payload),
    payload = payload
  ), class = "PopgenVCFRuntimeEnvelope")
  expect_error(
    migrate_runtime_integrity_envelope(legacy, new_runtime_migration_registry()),
    "no registered runtime migration path"
  )

  counter <- 0L
  registry <- register_runtime_migration(
    new_runtime_migration_registry(),
    "execution_plan", 1L, 2L,
    function(value) {
      counter <<- counter + 1L
      value$counter <- counter
      value
    },
    "nondeterministic"
  )
  expect_error(
    migrate_runtime_integrity_envelope(legacy, registry),
    "nondeterministic"
  )
})

test_that("current runtime envelopes produce deterministic no-op records", {
  payload <- list(module = "qc")
  envelope <- new_runtime_integrity_envelope("execution_plan", payload)
  first <- migrate_runtime_integrity_envelope(
    envelope, new_runtime_migration_registry()
  )
  second <- migrate_runtime_integrity_envelope(
    envelope, new_runtime_migration_registry()
  )
  expect_length(first$record$steps, 0L)
  expect_identical(first$record$source_version, first$record$target_version)
  expect_identical(first$record$migration_fingerprint,
                   second$record$migration_fingerprint)
})
