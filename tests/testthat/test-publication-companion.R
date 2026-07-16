test_that("publication styles are deterministic and validated", {
  expect_identical(publication_style("Nature")$name, "Nature")
  expect_identical(publication_style("Molecular Ecology")$citation_style, "author-year")
  expect_error(publication_style("unknown-journal"), "unknown publication style")
  expect_error(publication_style(list(name = "Custom")), "missing")
})

test_that("publication plans support metadata-poor projects", {
  project <- new_popgenvcf_project(
    "QC-only project", project_id = "00000000-0000-0000-0000-000000000071")
  bundle <- new_publication_bundle(project)
  expect_s3_class(bundle, "PopgenVCFPublicationBundle")
  expect_identical(bundle$project_id, project$project_id)
  expect_match(bundle$methods, "popgenVCF")
  expect_equal(nrow(bundle$artifacts), 0L)
  expect_silent(validate_publication_bundle(bundle))
})

test_that("publication plans classify immutable lineage artifacts", {
  figure <- tempfile(fileext = ".pdf")
  table <- tempfile(fileext = ".tsv")
  writeLines("figure", figure)
  writeLines("sample\tvalue\nS1\t1", table)
  lineage <- new_artifact_lineage(
    list(new_lineage_execution("exec:pca", "pca")),
    list(
      new_lineage_artifact("artifact:pca:figure", "pca", "PCA scatterplot", "figure", "pdf",
                           producer = "exec:pca", path = figure),
      new_lineage_artifact("artifact:pca:scores", "pca", "PCA scores", "table", "tsv",
                           producer = "exec:pca", path = table)
    )
  )
  project <- new_popgenvcf_project(
    "Lineage publication", project_id = "00000000-0000-0000-0000-000000000072")
  project <- set_project_artifact_lineage(project, lineage)
  bundle <- new_publication_bundle(project, style = "PLOS")
  expect_setequal(bundle$artifacts$category, c("figure", "table"))
  expect_equal(nrow(bundle$captions), 2L)
  expect_true(all(grepl("immutable popgenVCF artifact", bundle$captions$caption)))
})

test_that("publication directories are portable and checksummed", {
  project <- new_popgenvcf_project(
    "Portable publication", project_id = "00000000-0000-0000-0000-000000000073")
  directory <- tempfile("publication-")
  result <- generate_publication_bundle(project, directory, include_project = TRUE)
  expect_true(dir.exists(result))
  expect_true(file.exists(file.path(directory, "manuscript", "methods.md")))
  expect_true(file.exists(file.path(directory, "supplementary", "analysis.popgenvcf")))
  expect_true(validate_publication_bundle(directory))

  writeLines("tampered", file.path(directory, "manuscript", "methods.md"))
  expect_error(validate_publication_bundle(directory), "checksum mismatch")
})

test_that("FAIR metadata is included when available", {
  project <- new_popgenvcf_project(
    "FAIR publication", project_id = "00000000-0000-0000-0000-000000000074")
  fair <- new_fair_metadata(project, creators = list(new_fair_creator("Jane Doe")))
  project <- set_project_fair_metadata(project, fair)
  directory <- tempfile("publication-fair-")
  generate_publication_bundle(project, directory, include_project = FALSE, include_fair = TRUE)
  expect_true(file.exists(file.path(directory, "FAIR", "ro-crate-metadata.json")))
  expect_true(validate_publication_bundle(directory))
})

test_that("publication plans embed in their originating project", {
  project <- new_popgenvcf_project(
    "Embedded publication", project_id = "00000000-0000-0000-0000-000000000075")
  bundle <- new_publication_bundle(project, style = "BMC")
  updated <- set_project_publication_bundle(project, bundle)
  expect_s3_class(updated$artifacts$publication_bundle, "PopgenVCFPublicationBundle")
  expect_identical(updated$provenance$publication$style, "BMC")

  other <- new_popgenvcf_project(
    "Other", project_id = "00000000-0000-0000-0000-000000000076")
  expect_error(set_project_publication_bundle(other, bundle), "another project")
})
