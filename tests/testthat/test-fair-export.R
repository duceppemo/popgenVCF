test_that("FAIR metadata preserves stable project and artifact identities", {
  value <- matrix(1:4, 2L)
  lineage <- new_artifact_lineage(
    list(new_lineage_execution("exec:pca", "pca")),
    list(new_lineage_artifact("artifact:pca:scores", "pca", "scores", "data", "rds",
                              producer = "exec:pca", object = value))
  )
  project <- new_popgenvcf_project(
    "FAIR project", project_id = "00000000-0000-0000-0000-000000000066")
  project <- set_project_artifact_lineage(project, lineage)
  creator <- new_fair_creator("Jane Doe", "0000-0002-1825-0097", "Example University")
  metadata <- new_fair_metadata(project, creators = list(creator), license = "CC-BY-4.0")

  expect_s3_class(metadata, "PopgenVCFFAIRMetadata")
  expect_identical(metadata$identifier,
                   "urn:popgenvcf:project:00000000-0000-0000-0000-000000000066")
  expect_equal(nrow(metadata$artifacts), 1L)
  expect_match(metadata$artifacts$urn, "^urn:popgenvcf:project:.*:artifact:")
  expect_identical(fair_identifier(project), metadata$identifier)
})

test_that("FAIR documents expose RO-Crate, CodeMeta, DataCite, and CFF records", {
  project <- new_popgenvcf_project(
    "Standards project", project_id = "00000000-0000-0000-0000-000000000067")
  metadata <- new_fair_metadata(
    project,
    creators = list(new_fair_creator("Marc Olivier", "0000-0002-1825-0097")),
    rights_uri = "https://spdx.org/licenses/MIT.html"
  )
  docs <- fair_documents(metadata)
  expect_named(docs, c("ro_crate", "codemeta", "datacite", "citation_cff"))
  expect_identical(docs$ro_crate$`@context`, "https://w3id.org/ro/crate/1.1/context")
  expect_identical(docs$codemeta$`@type`, "SoftwareSourceCode")
  expect_identical(docs$datacite$data$type, "dois")
  expect_identical(docs$citation_cff$`cff-version`, "1.2.0")
})

test_that("FAIR bundles are checksummed and validate", {
  project <- new_popgenvcf_project(
    "Archive project", project_id = "00000000-0000-0000-0000-000000000068")
  metadata <- new_fair_metadata(
    project, creators = list(new_fair_creator("Jane Doe")))
  directory <- tempfile("fair-bundle-")
  written <- write_fair_bundle(metadata, directory)
  expect_true(all(file.exists(written)))
  expect_true(validate_fair_bundle(directory))
  expect_true(file.exists(file.path(directory, "ro-crate-metadata.json")))
  expect_true(file.exists(file.path(directory, "CITATION.cff")))

  writeLines("tampered", file.path(directory, "codemeta.json"))
  expect_error(validate_fair_bundle(directory), "checksum mismatch")
})

test_that("FAIR metadata embeds safely in portable projects", {
  project <- new_popgenvcf_project(
    "Embedded FAIR", project_id = "00000000-0000-0000-0000-000000000069")
  metadata <- new_fair_metadata(project, creators = list(new_fair_creator("Jane Doe")))
  project <- set_project_fair_metadata(project, metadata)
  path <- tempfile(fileext = ".popgenvcf")
  write_popgenvcf_project(project, path)
  restored <- read_popgenvcf_project(path)
  expect_s3_class(restored$artifacts$fair_metadata, "PopgenVCFFAIRMetadata")
  expect_identical(restored$artifacts$fair_metadata$identifier, metadata$identifier)
  expect_identical(restored$provenance$fair$identifier, metadata$identifier)
})

test_that("creator and FAIR validation reject invalid identities", {
  expect_error(new_fair_creator("Jane Doe", "not-an-orcid"), "ORCID")
  project <- new_popgenvcf_project(
    "Validation", project_id = "00000000-0000-0000-0000-000000000070")
  metadata <- new_fair_metadata(project)
  metadata$identifier <- "invalid"
  expect_error(validate_fair_metadata(metadata), "identifier")
})
