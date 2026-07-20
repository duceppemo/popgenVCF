test_that("current envelopes assemble deterministic replay bundles", {
  execution <- new_persisted_execution_ledger(data.frame(
    module = c("pca", "qc"), status = c("success", "success"),
    attempt = c(2L, 1L), stringsAsFactors = FALSE
  ))
  attempts <- new_attempt_ledger(data.frame(
    module = c("pca", "pca", "qc"), status = c("failed", "success", "success"),
    attempt = c(1L, 2L, 1L), stringsAsFactors = FALSE
  ))
  envelopes <- list(
    new_runtime_integrity_envelope("execution_ledger", execution),
    new_runtime_integrity_envelope("attempt_ledger", attempts)
  )
  registry <- new_runtime_migration_registry()
  first <- assemble_runtime_replay_from_envelopes(envelopes, registry)
  second <- assemble_runtime_replay_from_envelopes(rev(envelopes), registry)
  expect_s3_class(first, "PopgenVCFRuntimeReplayAssembly")
  expect_true(first$bundle$verification$verified)
  expect_length(first$migration_records, 2L)
  expect_true(all(vapply(first$migration_records, function(x) length(x$steps) == 0L,
                         logical(1))))
  expect_identical(first$assembly_fingerprint, second$assembly_fingerprint)
})

test_that("replay assembly rejects duplicates, missing ledgers, and mutation", {
  execution <- new_persisted_execution_ledger(data.frame(
    module = "qc", status = "success", stringsAsFactors = FALSE
  ))
  envelope <- new_runtime_integrity_envelope("execution_ledger", execution)
  registry <- new_runtime_migration_registry()
  expect_error(assemble_runtime_replay_from_envelopes(list(envelope, envelope), registry),
               "duplicate singleton kind")
  attempt <- new_attempt_ledger(data.frame(
    module = "qc", status = "success", attempt = 1L
  ))
  expect_error(assemble_runtime_replay_from_envelopes(
    list(new_runtime_integrity_envelope("attempt_ledger", attempt)), registry
  ), "exactly one execution ledger")
  mutated <- envelope
  mutated$payload$status <- "failed"
  expect_error(assemble_runtime_replay_from_envelopes(list(mutated), registry),
               "runtime integrity digest mismatch")
})
