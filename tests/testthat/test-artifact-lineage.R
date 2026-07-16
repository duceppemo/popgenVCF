test_that("artifact lineage records immutable producers and consumers", {
  input <- tempfile(fileext = ".tsv")
  writeLines(c("sample\tPC1", "A\t0.1"), input)
  executions <- list(
    new_lineage_execution("exec:pca", "pca", parameters = list(n_pcs = 2L)),
    new_lineage_execution("exec:report", "report")
  )
  artifact <- new_lineage_artifact(
    "artifact:pca:scores", "pca", "scores", "table", "tsv",
    producer = "exec:pca", consumers = "exec:report", path = input
  )
  lineage <- new_artifact_lineage(executions, list(artifact))
  expect_s3_class(lineage, "PopgenVCFArtifactLineage")
  expect_match(lineage$digest, "^[0-9a-f]{64}$")
  expect_equal(provenance_topological_order(lineage$dag),
               c("exec:pca", "artifact:pca:scores", "exec:report"))
  expect_true(verify_artifact_lineage(lineage))
  expect_equal(lineage_artifact_table(lineage)$producer, "exec:pca")
})

test_that("object artifacts can be verified without files", {
  value <- matrix(1:4, 2L)
  lineage <- new_artifact_lineage(
    list(new_lineage_execution("exec:ibs", "ibs")),
    list(new_lineage_artifact(
      "artifact:ibs:matrix", "ibs", "matrix", "data", "rds",
      producer = "exec:ibs", object = value
    ))
  )
  expect_true(verify_artifact_lineage(
    lineage, objects = list("artifact:ibs:matrix" = value)))
  expect_error(verify_artifact_lineage(
    lineage, objects = list("artifact:ibs:matrix" = value + 1)), "content changed")
})

test_that("lineage rejects invalid execution relationships", {
  executions <- list(
    new_lineage_execution("exec:a", "a"),
    new_lineage_execution("exec:b", "b")
  )
  expect_error(new_artifact_lineage(
    executions,
    list(new_lineage_artifact("artifact:x", "a", "x", "data", "rds",
                              producer = "exec:missing", object = 1))),
    "unknown artifact producer")
  expect_error(new_artifact_lineage(
    executions,
    list(new_lineage_artifact("artifact:x", "a", "x", "data", "rds",
                              producer = "exec:a", consumers = "exec:a", object = 1))),
    "cannot consume its own")
  expect_error(new_artifact_lineage(
    list(executions[[1]], executions[[1]]), list()), "execution IDs must be unique")
})

test_that("lineage exports machine-readable and graph formats", {
  file <- tempfile(fileext = ".txt"); writeLines("artifact", file)
  lineage <- new_artifact_lineage(
    list(new_lineage_execution("exec:module", "module")),
    list(new_lineage_artifact("artifact:file", "module", "file", "table", "txt",
                              producer = "exec:module", path = file))
  )
  out <- write_artifact_lineage(lineage, tempfile("lineage-export-"))
  expect_true(all(file.exists(out)))
  expect_true(any(grepl("graphml$", out)))
  expect_true(any(grepl("dot$", out)))
})

test_that("lineage can be embedded in portable projects", {
  lineage <- new_artifact_lineage(
    list(new_lineage_execution("exec:pca", "pca")),
    list(new_lineage_artifact("artifact:pca", "pca", "scores", "data", "rds",
                              producer = "exec:pca", object = c(1, 2))))
  project <- new_popgenvcf_project(
    "lineage-project", project_id = "00000000-0000-0000-0000-000000000065")
  project <- set_project_artifact_lineage(project, lineage)
  expect_identical(project$provenance$artifact_lineage$digest, lineage$digest)
  path <- tempfile(fileext = ".popgenvcf")
  write_popgenvcf_project(project, path)
  restored <- read_popgenvcf_project(path)
  expect_identical(restored$provenance$artifact_lineage$digest, lineage$digest)
})
