test_that("runtime integrity envelopes are deterministic", {
  payload <- list(module = "pca", values = c(1, 2, 3))
  first <- new_runtime_integrity_envelope("execution_plan", payload)
  second <- new_runtime_integrity_envelope("execution_plan", payload)

  expect_identical(first$digest, second$digest)
  expect_identical(runtime_integrity_payload(first), payload)
})

test_that("payload mutation is detected", {
  envelope <- new_runtime_integrity_envelope(
    "execution_ledger",
    list(status = c("success", "blocked"))
  )
  envelope$payload$status[[1]] <- "failed"

  expect_error(
    validate_runtime_integrity_envelope(envelope),
    "digest mismatch"
  )
})

test_that("schema and envelope kinds must agree", {
  envelope <- new_runtime_integrity_envelope("checkpoint", list(value = 1))
  envelope$kind <- "execution_plan"

  expect_error(
    validate_runtime_integrity_envelope(envelope),
    "kind does not match"
  )
})

test_that("unknown algorithms and malformed envelopes fail closed", {
  envelope <- new_runtime_integrity_envelope("scheduler_metadata", list(seed = 11))
  envelope$digest_algorithm <- "md5"
  expect_error(validate_runtime_integrity_envelope(envelope), "unsupported")

  malformed <- envelope[c("kind", "schema")]
  expect_error(validate_runtime_integrity_envelope(malformed), "missing field")
})

test_that("unsupported future schemas fail before payload extraction", {
  envelope <- new_runtime_integrity_envelope("process_result", list(status = "success"))
  envelope$schema$version <- envelope$schema$version + 1L
  envelope$digest <- runtime_payload_digest(envelope$payload)

  expect_error(runtime_integrity_payload(envelope), "future|unsupported")
})
