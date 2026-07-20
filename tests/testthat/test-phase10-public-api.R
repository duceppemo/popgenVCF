test_that("public API discovery is deterministic and versioned", {
  descriptor <- phase10_api_descriptor()
  operations <- phase10_api_operations(descriptor)

  expect_s3_class(descriptor, "PopgenVCFPublicAPIDescriptor")
  expect_identical(operations$operation_id, sort(operations$operation_id))
  expect_true(all(operations$lifecycle == "stable"))
  expect_true(validate_phase10_api_descriptor(descriptor))
  expect_error(phase10_api_descriptor("2.0.0"), "Unsupported public API")
})

test_that("public requests canonicalize named inputs and reject internals", {
  request <- new_public_analysis_request(
    operation_id = "analysis.execute",
    analysis_id = "analysis-001",
    parameters = list(zeta = "last", alpha = "first"),
    input_ids = c(vcf = "sha256:vcf", metadata = "sha256:metadata")
  )

  expect_identical(names(request$parameters), c("alpha", "zeta"))
  expect_identical(names(request$input_ids), c("metadata", "vcf"))
  expect_true(validate_public_analysis_request(request))
  expect_error(
    new_public_analysis_request(
      "analysis.execute", "analysis-001",
      parameters = list(executor = "internal")
    ),
    "Internal runtime fields"
  )
  expect_error(
    new_public_analysis_request("scheduler.dispatch", "analysis-001"),
    "Unsupported public operation"
  )
})

test_that("public responses preserve scientific and artifact identities", {
  request <- new_public_analysis_request(
    "result.inspect", "analysis-002",
    input_ids = c(result = "sha256:result")
  )
  response <- new_public_analysis_response(
    request = request,
    status = "completed",
    scientific_values = list(global_fst = 0.12),
    artifact_ids = c(table = "artifact:fst-table"),
    provenance_ids = c(run = "provenance:run-002"),
    warnings = c("exploratory", "exploratory")
  )

  expect_true(validate_public_analysis_response(response, request))
  summary <- inspect_public_analysis_response(response)
  expect_identical(summary$artifact_count, 1L)
  expect_identical(response$warnings, "exploratory")
  expect_error(
    new_public_analysis_response(request, "failed"),
    "require an error record"
  )
})

test_that("public records fail closed on mutation", {
  request <- new_public_analysis_request(
    "artifact.list", "analysis-003",
    input_ids = c(result = "sha256:result")
  )
  mutated <- request
  mutated$analysis_id <- "analysis-mutated"

  expect_error(
    validate_public_analysis_request(mutated),
    "fingerprint verification failed"
  )
})

test_that("public record serialization round trips", {
  request <- new_public_analysis_request(
    "provenance.inspect", "analysis-004",
    parameters = list(scope = "scientific"),
    input_ids = c(result = "sha256:result")
  )
  path <- tempfile(fileext = ".json")
  write_public_api_record(request, path)
  restored <- read_public_api_record(path)

  expect_s3_class(restored, "PopgenVCFPublicAPIRequest")
  expect_identical(restored$fingerprint, request$fingerprint)
  expect_true(validate_public_analysis_request(restored))
})
