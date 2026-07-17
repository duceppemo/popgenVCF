test_that("manuscript cross references are stable and anchored", {
  figure <- tempfile(fileext = ".png")
  table <- tempfile(fileext = ".tsv")
  writeBin(as.raw(c(137, 80, 78, 71)), figure)
  writeLines("sample\tvalue\nS1\t1", table)
  lineage <- new_artifact_lineage(
    list(new_lineage_execution("exec:pca", "pca")),
    list(
      new_lineage_artifact("artifact:pca:figure", "pca", "PCA figure", "figure", "png",
                           producer = "exec:pca", path = figure),
      new_lineage_artifact("artifact:pca:table", "pca", "PCA scores", "table", "tsv",
                           producer = "exec:pca", path = table)
    )
  )
  project <- new_popgenvcf_project(
    "Cross references", project_id = "00000000-0000-0000-0000-000000000731")
  project <- set_project_artifact_lineage(project, lineage)
  manuscript <- new_manuscript(project)
  refs <- manuscript_cross_reference_table(manuscript)
  expect_equal(nrow(refs), 2L)
  expect_true(all(nzchar(refs$anchor)))
  expect_true(refs[id == "artifact:pca:figure", embeddable])
  expect_false(refs[id == "artifact:pca:table", embeddable])
  expect_identical(refs$anchor, manuscript_cross_reference_table(manuscript)$anchor)
})

test_that("written manuscripts copy assets and preserve bibliography", {
  figure <- tempfile(fileext = ".svg")
  writeLines(c("<svg xmlns=\"http://www.w3.org/2000/svg\"></svg>"), figure)
  lineage <- new_artifact_lineage(
    list(new_lineage_execution("exec:pca", "pca")),
    list(new_lineage_artifact("artifact:pca:figure", "pca", "PCA figure", "figure", "svg",
                              producer = "exec:pca", path = figure))
  )
  project <- new_popgenvcf_project(
    "Portable assets", project_id = "00000000-0000-0000-0000-000000000732")
  project <- set_project_artifact_lineage(project, lineage)
  publication <- new_publication_bundle(project)
  publication$bibliography <- "@article{popgenvcf-test, title={Test reference}}"
  manuscript <- new_manuscript(project, publication = publication)
  directory <- tempfile("manuscript-assets-")
  write_manuscript(manuscript, directory)
  expect_true(file.exists(file.path(directory, "cross-references.tsv")))
  expect_true(file.exists(file.path(directory, "references.bib")))
  expect_length(list.files(file.path(directory, "assets", "figures")), 1L)
  markdown <- readLines(file.path(directory, "manuscript.md"), warn = FALSE)
  expect_true(any(grepl("!\\[", markdown)))
  expect_true(any(grepl("references.bib", markdown, fixed = TRUE)))
  expect_true(validate_manuscript(directory))
})

test_that("copied manuscript assets are covered by checksums", {
  artifact <- tempfile(fileext = ".csv")
  writeLines("sample,value\nS1,1", artifact)
  lineage <- new_artifact_lineage(
    list(new_lineage_execution("exec:diversity", "diversity")),
    list(new_lineage_artifact("artifact:diversity:table", "diversity", "Diversity table",
                              "table", "csv", producer = "exec:diversity", path = artifact))
  )
  project <- new_popgenvcf_project(
    "Checksummed assets", project_id = "00000000-0000-0000-0000-000000000733")
  project <- set_project_artifact_lineage(project, lineage)
  directory <- tempfile("manuscript-checksum-")
  write_manuscript(new_manuscript(project), directory)
  copied <- list.files(file.path(directory, "assets"), recursive = TRUE, full.names = TRUE)
  expect_length(copied, 1L)
  writeLines("tampered", copied[[1L]])
  expect_error(validate_manuscript(directory), "checksum mismatch")
})
