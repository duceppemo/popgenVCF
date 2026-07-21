test_that("public artifact listing is canonical and hides internal paths", {
  manifest <- new_artifact_manifest()
  manifest <- register_artifact(manifest, new_analysis_artifact(
    module = "pca", name = "scores", type = "table",
    path = "/private/output/pca.tsv", format = "tsv",
    description = "PCA scores", metadata = list(worker = "internal")
  ))
  manifest <- register_artifact(manifest, new_analysis_artifact(
    module = "ancestry", name = "membership", type = "figure",
    path = "figures/q.svg", format = "svg",
    description = "Membership plot"
  ))
  request <- new_public_analysis_request("artifact.list", "analysis-1")

  response <- list_public_artifacts(request, manifest)

  expect_true(validate_public_analysis_response(response, request))
  expect_identical(response$status, "completed")
  expect_identical(
    names(response$scientific_values$artifacts),
    c("ancestry::membership", "pca::scores")
  )
  expect_identical(names(response$artifact_ids), c("ancestry::membership", "pca::scores"))
  serialized <- paste(capture.output(str(response)), collapse = "\n")
  expect_false(grepl("/private/output", serialized, fixed = TRUE))
  expect_false(grepl("worker", serialized, fixed = TRUE))
})

test_that("equivalent artifact manifests produce equivalent public responses", {
  a <- new_analysis_artifact("pca", "scores", "table", "one.tsv", "tsv")
  b <- new_analysis_artifact("pca", "plot", "figure", "one.svg", "svg")
  first <- new_artifact_manifest(list(a, b))
  second <- new_artifact_manifest(list(b, a))
  request <- new_public_analysis_request("artifact.list", "analysis-2")

  expect_identical(
    list_public_artifacts(request, first)$fingerprint,
    list_public_artifacts(request, second)$fingerprint
  )
})

test_that("artifact adapter rejects unsupported operations and objects", {
  request <- new_public_analysis_request("provenance.inspect", "analysis-3")
  response <- list_public_artifacts(request, new_artifact_manifest())
  expect_identical(response$status, "rejected")
  expect_identical(response$error$code, "unsupported_operation")

  request <- new_public_analysis_request("artifact.list", "analysis-3")
  response <- list_public_artifacts(request, list())
  expect_identical(response$status, "rejected")
  expect_identical(response$error$code, "invalid_artifact_manifest")
})

test_that("public provenance inspection is canonical and omits runtime details", {
  input <- new_provenance_node(
    "input", kind = "input", digest = paste(rep("a", 64), collapse = ""),
    parameters = list(secret_path = "/private/input.vcf"),
    started_at = "2026-01-01T00:00:00Z"
  )
  result <- new_provenance_node(
    "result", kind = "analysis", digest = paste(rep("b", 64), collapse = ""),
    software = list(worker = "internal"), completed_at = "2026-01-01T00:01:00Z"
  )
  dag <- new_provenance_dag(
    nodes = list(result, input),
    edges = list(new_provenance_edge("input", "result", "derived_from"))
  )
  request <- new_public_analysis_request("provenance.inspect", "analysis-4")

  response <- inspect_public_provenance(request, dag)

  expect_true(validate_public_analysis_response(response, request))
  expect_identical(response$status, "completed")
  expect_identical(response$scientific_values$topological_order, c("input", "result"))
  expect_identical(names(response$scientific_values$nodes), c("input", "result"))
  expect_identical(names(response$provenance_ids), c("input", "result"))
  serialized <- paste(capture.output(str(response)), collapse = "\n")
  expect_false(grepl("secret_path", serialized, fixed = TRUE))
  expect_false(grepl("started_at", serialized, fixed = TRUE))
  expect_false(grepl("completed_at", serialized, fixed = TRUE))
  expect_false(grepl("worker", serialized, fixed = TRUE))
})

test_that("equivalent provenance ordering produces equivalent responses", {
  a <- new_provenance_node("a", kind = "input")
  b <- new_provenance_node("b", kind = "analysis")
  edge <- new_provenance_edge("a", "b", "consumes")
  first <- new_provenance_dag(list(a, b), list(edge))
  second <- new_provenance_dag(list(b, a), list(edge))
  request <- new_public_analysis_request("provenance.inspect", "analysis-5")

  expect_identical(
    inspect_public_provenance(request, first)$fingerprint,
    inspect_public_provenance(request, second)$fingerprint
  )
})

test_that("provenance adapter rejects unsupported operations and invalid DAGs", {
  request <- new_public_analysis_request("artifact.list", "analysis-6")
  response <- inspect_public_provenance(request, new_provenance_dag())
  expect_identical(response$status, "rejected")
  expect_identical(response$error$code, "unsupported_operation")

  request <- new_public_analysis_request("provenance.inspect", "analysis-6")
  response <- inspect_public_provenance(request, list())
  expect_identical(response$status, "rejected")
  expect_identical(response$error$code, "invalid_provenance_dag")
})
