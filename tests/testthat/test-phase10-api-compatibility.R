test_that("identical descriptors are compatible and deterministic", {
  baseline <- phase10_api_descriptor("1.0.0")
  candidate <- phase10_api_descriptor("1.0.0")

  first <- popgenVCF:::compare_phase10_api_descriptors(baseline, candidate)
  second <- popgenVCF:::compare_phase10_api_descriptors(baseline, candidate)

  expect_identical(first$classification, "compatible")
  expect_true(first$release_compatible)
  expect_true(all(first$changes$classification == "compatible"))
  expect_identical(first$fingerprint, second$fingerprint)
  expect_true(popgenVCF:::validate_phase10_api_compatibility(first))
})

test_that("new operations and compatible schema advances are additive", {
  baseline <- phase10_api_descriptor("1.0.0")
  candidate <- baseline
  candidate$api_version <- "1.1.0"
  candidate$operations$request_schema[[1L]] <-
    "popgenvcf.public.analysis-request/1.1.0"
  candidate$operations <- rbind(
    candidate$operations,
    data.frame(
      operation_id = "summary.export",
      request_schema = "popgenvcf.public.summary-export-request/1.0.0",
      response_schema = "popgenvcf.public.summary-export-response/1.0.0",
      lifecycle = "stable",
      stringsAsFactors = FALSE
    )
  )
  candidate$fingerprint <- phase10_public_fingerprint(candidate)

  result <- popgenVCF:::compare_phase10_api_descriptors(baseline, candidate)
  expect_identical(result$classification, "additive")
  expect_true(result$release_compatible)
  expect_true(any(result$changes$classification == "additive"))
})

test_that("explicit stable-to-deprecated transitions are classified", {
  baseline <- phase10_api_descriptor("1.0.0")
  candidate <- baseline
  candidate$api_version <- "1.1.0"
  candidate$operations$lifecycle[[1L]] <- "deprecated"
  candidate$fingerprint <- phase10_public_fingerprint(candidate)

  result <- popgenVCF:::compare_phase10_api_descriptors(baseline, candidate)
  expect_identical(result$classification, "deprecated")
  expect_true(result$release_compatible)
  expect_true(any(result$changes$classification == "deprecated"))
})

test_that("removed operations and major schema changes fail closed", {
  baseline <- phase10_api_descriptor("1.0.0")

  removed <- baseline
  removed$api_version <- "2.0.0"
  removed$operations <- removed$operations[-1L, , drop = FALSE]
  removed$fingerprint <- phase10_public_fingerprint(removed)
  removed_result <- popgenVCF:::compare_phase10_api_descriptors(baseline, removed)
  expect_identical(removed_result$classification, "breaking")
  expect_false(removed_result$release_compatible)
  expect_error(
    popgenVCF:::validate_phase10_api_compatibility(removed_result),
    "requires explicit approval"
  )
  expect_true(popgenVCF:::validate_phase10_api_compatibility(
    removed_result, allow_breaking = TRUE
  ))

  schema <- baseline
  schema$api_version <- "2.0.0"
  schema$operations$response_schema[[1L]] <-
    "popgenvcf.public.analysis-response/2.0.0"
  schema$fingerprint <- phase10_public_fingerprint(schema)
  schema_result <- popgenVCF:::compare_phase10_api_descriptors(baseline, schema)
  expect_identical(schema_result$classification, "breaking")
})

test_that("compatibility evidence detects tampering and renders a report", {
  result <- popgenVCF:::compare_phase10_api_descriptors(
    phase10_api_descriptor("1.0.0"),
    phase10_api_descriptor("1.0.0")
  )
  report <- popgenVCF:::phase10_api_compatibility_report(result)
  expect_match(report[[1L]], "Phase 10 public API compatibility report", fixed = TRUE)
  expect_true(any(grepl("Classification: **compatible**", report, fixed = TRUE)))

  result$classification <- "breaking"
  expect_error(
    popgenVCF:::validate_phase10_api_compatibility(result, allow_breaking = TRUE),
    "fingerprint verification failed"
  )
})
