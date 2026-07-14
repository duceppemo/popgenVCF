test_that("artifacts and manifests enforce stable identifiers", {
  table_artifact <- new_analysis_artifact(
    module = "pca",
    name = "scores",
    type = "table",
    path = "tables/pca_scores.tsv",
    format = "tsv",
    description = "Per-sample PCA coordinates"
  )

  expect_s3_class(table_artifact, "PopgenVCFArtifact")
  expect_true(validate_analysis_artifact(table_artifact))

  manifest <- new_artifact_manifest()
  manifest <- register_artifact(manifest, table_artifact)
  expect_s3_class(manifest, "PopgenVCFArtifactManifest")
  expect_true(validate_artifact_manifest(manifest))

  tab <- artifact_manifest_table(manifest)
  expect_equal(nrow(tab), 1L)
  expect_equal(tab$module, "pca")
  expect_equal(tab$name, "scores")
  expect_equal(tab$type, "table")

  expect_error(register_artifact(manifest, table_artifact), "duplicate artifact identifier")
})

test_that("artifact validation rejects invalid declarations", {
  expect_error(
    new_analysis_artifact("pca", "scores", "unknown", "x.tsv", "tsv"),
    "unsupported artifact type"
  )
  expect_error(
    new_analysis_artifact("", "scores", "table", "x.tsv", "tsv"),
    "module must be one non-empty string"
  )
  expect_error(
    new_analysis_artifact("pca", "scores", "table", "x.tsv", "tsv", metadata = unname(list(1))),
    "metadata must be a named list"
  )
})

test_that("required files can be checked at publication time", {
  existing <- tempfile(fileext = ".tsv")
  writeLines("sample\tPC1", existing)
  on.exit(unlink(existing), add = TRUE)

  artifact <- new_analysis_artifact("pca", "scores", "table", existing, "tsv")
  manifest <- new_artifact_manifest(list(artifact))
  expect_true(validate_artifact_manifest(manifest, must_exist = TRUE))

  missing <- new_analysis_artifact("pca", "loadings", "table", tempfile(), "tsv")
  expect_error(
    validate_artifact_manifest(new_artifact_manifest(list(missing)), must_exist = TRUE),
    "artifact file does not exist"
  )
})
