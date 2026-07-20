test_that("legacy replay components must all migrate before assembly", {
  testthat::local_mocked_bindings(
    .runtime_schema_versions = c(execution_ledger = 2L, attempt_ledger = 2L),
    .package = "popgenVCF"
  )
  execution <- new_persisted_execution_ledger(data.frame(
    module = "qc", status = "success", attempt = 1L, stringsAsFactors = FALSE
  ))
  attempts <- new_attempt_ledger(data.frame(
    module = "qc", status = "success", attempt = 1L, stringsAsFactors = FALSE
  ))
  legacy <- function(kind, payload) structure(list(
    kind = kind, schema = list(kind = kind, version = 1L),
    digest_algorithm = "sha256", digest = runtime_payload_digest(payload),
    payload = payload
  ), class = "PopgenVCFRuntimeEnvelope")
  envelopes <- list(legacy("execution_ledger", execution),
                    legacy("attempt_ledger", attempts))

  partial <- register_runtime_migration(
    new_runtime_migration_registry(), "execution_ledger", 1L, 2L,
    identity, "execution-ledger-v1-v2"
  )
  expect_error(assemble_runtime_replay_from_envelopes(envelopes, partial),
               "no registered runtime migration path")

  complete <- register_runtime_migration(
    partial, "attempt_ledger", 1L, 2L, identity, "attempt-ledger-v1-v2"
  )
  assembled <- assemble_runtime_replay_from_envelopes(envelopes, complete)
  expect_true(assembled$bundle$verification$verified)
  expect_true(all(vapply(assembled$migration_records, function(x) length(x$steps) == 1L,
                         logical(1))))
})
