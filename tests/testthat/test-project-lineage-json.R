test_that("portable projects preserve full lineage and JSON-safe summaries", {
  lineage <- new_artifact_lineage(
    list(new_lineage_execution("exec:pca", "pca")),
    list(new_lineage_artifact(
      "artifact:pca", "pca", "scores", "data", "rds",
      producer = "exec:pca", object = c(1, 2)
    ))
  )
  project <- new_popgenvcf_project(
    "lineage-json-project",
    project_id = "00000000-0000-0000-0000-000000000165"
  )
  project <- set_project_artifact_lineage(project, lineage)

  expect_s3_class(project$artifacts$artifact_lineage, "PopgenVCFArtifactLineage")
  expect_identical(project$provenance$artifact_lineage$digest, lineage$digest)

  path <- tempfile(fileext = ".popgenvcf")
  write_popgenvcf_project(project, path)
  restored <- read_popgenvcf_project(path)

  expect_s3_class(restored$artifacts$artifact_lineage, "PopgenVCFArtifactLineage")
  expect_identical(restored$artifacts$artifact_lineage$digest, lineage$digest)
  expect_identical(restored$provenance$artifact_lineage$digest, lineage$digest)
})
