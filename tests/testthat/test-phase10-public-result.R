make_phase10_public_diversity_result <- function(path = "/private/results/diversity.tsv",
                                                   provenance = list(worker = "internal"),
                                                   parameters = list(threads = 8L)) {
  artifacts <- new_artifact_manifest(list(new_analysis_artifact(
    module = "diversity", name = "statistics", type = "table",
    path = path, format = "tsv", description = "Diversity statistics"
  )))
  new_diversity_result(
    data.frame(population = c("north", "south"), he = c(0.21, 0.24)),
    parameters = parameters,
    provenance = provenance,
    metadata = data.frame(sample = c("hidden-a", "hidden-b")),
    validation = data.frame(check = c("scientific", "object_schema"), passed = TRUE),
    artifacts = artifacts
  )
}

test_that("public core result inspection exposes canonical scientific content", {
  request <- new_public_analysis_request("result.inspect", "analysis-result-1")
  response <- inspect_public_result(request, make_phase10_public_diversity_result())

  expect_true(validate_public_analysis_response(response, request))
  expect_identical(response$status, "completed")
  expect_identical(response$scientific_values$analysis, "diversity")
  expect_identical(response$scientific_values$primary_table$population, c("north", "south"))
  expect_identical(names(response$artifact_ids), "diversity::statistics")
  expect_match(response$scientific_values$result_id, "^result::diversity::[0-9a-f]{64}$")
})

test_that("equivalent scientific results ignore internal provenance and paths", {
  request <- new_public_analysis_request("result.inspect", "analysis-result-2")
  first <- make_phase10_public_diversity_result(
    path = "/private/one.tsv",
    provenance = list(worker = "one", started_at = "2026-01-01"),
    parameters = list(threads = 1L)
  )
  second <- make_phase10_public_diversity_result(
    path = "/private/two.tsv",
    provenance = list(worker = "two", started_at = "2026-07-20"),
    parameters = list(threads = 64L)
  )

  response_one <- inspect_public_result(request, first)
  response_two <- inspect_public_result(request, second)

  expect_identical(response_one$fingerprint, response_two$fingerprint)
  expect_identical(
    response_one$scientific_values$result_id,
    response_two$scientific_values$result_id
  )
})

test_that("public result responses hide internal fields", {
  request <- new_public_analysis_request("result.inspect", "analysis-result-3")
  response <- inspect_public_result(request, make_phase10_public_diversity_result())
  serialized <- paste(capture.output(str(response)), collapse = "\n")

  expect_false(grepl("/private/results", serialized, fixed = TRUE))
  expect_false(grepl("worker", serialized, fixed = TRUE))
  expect_false(grepl("threads", serialized, fixed = TRUE))
  expect_false(grepl("hidden-a", serialized, fixed = TRUE))
})

test_that("ancestry inspection omits seed runtime and provenance", {
  replicate <- new_ancestry_replicate(
    sample_ids = c("s1", "s2"),
    q = matrix(c(0.8, 0.2, 0.3, 0.7), nrow = 2, byrow = TRUE),
    backend = "admixture", k = 2L, replicate = 1L, seed = 123L,
    metrics = c(cv_error = 0.12), runtime_seconds = 99,
    provenance = list(executable = "/private/admixture")
  )
  request <- new_public_analysis_request("result.inspect", "analysis-result-4")
  response <- inspect_public_result(request, new_ancestry_result(list(replicate)))

  expect_identical(response$status, "completed")
  expect_identical(response$scientific_values$analysis, "ancestry")
  expect_false("seed" %in% names(response$scientific_values$primary_table))
  expect_false("runtime_seconds" %in% names(response$scientific_values$primary_table))
  serialized <- paste(capture.output(str(response)), collapse = "\n")
  expect_false(grepl("/private/admixture", serialized, fixed = TRUE))
})

test_that("public result inspection fails closed", {
  wrong <- new_public_analysis_request("artifact.list", "analysis-result-5")
  response <- inspect_public_result(wrong, make_phase10_public_diversity_result())
  expect_identical(response$status, "rejected")
  expect_identical(response$error$code, "unsupported_operation")

  request <- new_public_analysis_request("result.inspect", "analysis-result-5")
  response <- inspect_public_result(request, list(not = "canonical"))
  expect_identical(response$status, "rejected")
  expect_identical(response$error$code, "invalid_result_object")
})
