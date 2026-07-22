release_provenance_script <- function() {
  installed <- system.file("scripts", "build_release_provenance.R", package = "popgenVCF")
  if (nzchar(installed)) return(installed)
  testthat::test_path("..", "..", "inst", "scripts", "build_release_provenance.R")
}

make_release_provenance_fixture <- function() {
  asset_dir <- tempfile("release-provenance-")
  dir.create(file.path(asset_dir, "archive-metadata"), recursive = TRUE)
  writeLines("source archive", file.path(asset_dir, "popgenVCF_0.10.0.tar.gz"))
  writeLines('{"spdxVersion":"SPDX-2.3","SPDXID":"SPDXRef-DOCUMENT"}',
             file.path(asset_dir, "popgenVCF-source-sbom.spdx.json"))
  writeLines('{"upload_type":"software"}',
             file.path(asset_dir, "archive-metadata", ".zenodo.json"))
  writeLines("cff-version: 1.2.0",
             file.path(asset_dir, "archive-metadata", "CITATION.cff"))
  asset_dir
}

test_that("source-release provenance is deterministic and checksum-linked", {
  environment <- new.env(parent = globalenv())
  sys.source(release_provenance_script(), envir = environment)
  asset_dir <- make_release_provenance_fixture()

  build <- function() environment$build_source_release_provenance(
    asset_dir = asset_dir,
    package_name = "popgenVCF",
    package_version = "0.10.0",
    release_id = "v0.10.0",
    git_tag = "v0.10.0",
    git_commit = paste(rep("a", 40L), collapse = ""),
    workflow_name = "Tagged source-package release",
    workflow_run_id = "123",
    workflow_run_attempt = "1",
    source_archive = "popgenVCF_0.10.0.tar.gz",
    source_sbom = "popgenVCF-source-sbom.spdx.json",
    archival_metadata_dir = "archive-metadata",
    created_at = "1970-01-01T00:00:00Z"
  )

  first <- build()
  path <- environment$write_source_release_provenance(first, asset_dir)
  expect_true(environment$verify_source_release_provenance(path, asset_dir))
  first_text <- readLines(path, warn = FALSE)

  second <- build()
  environment$write_source_release_provenance(second, asset_dir)
  expect_identical(readLines(path, warn = FALSE), first_text)
  expect_identical(first$record_type, "popgenvcf_source_release_provenance")
  expect_identical(first$release$git_commit, paste(rep("a", 40L), collapse = ""))
  expect_identical(first$control_chain$manifest, "release-manifest.json")
  expect_identical(first$control_chain$checksums, "release-SHA256SUMS.txt")
  expect_length(first$subjects, 2L)
  expect_length(first$archival_metadata, 2L)
})

test_that("source-release provenance detects payload drift", {
  environment <- new.env(parent = globalenv())
  sys.source(release_provenance_script(), envir = environment)
  asset_dir <- make_release_provenance_fixture()
  provenance <- environment$build_source_release_provenance(
    asset_dir = asset_dir,
    package_name = "popgenVCF",
    package_version = "0.10.0",
    release_id = "v0.10.0",
    git_tag = "v0.10.0",
    git_commit = paste(rep("b", 40L), collapse = ""),
    workflow_name = "test",
    workflow_run_id = "1",
    workflow_run_attempt = "1",
    source_archive = "popgenVCF_0.10.0.tar.gz",
    source_sbom = "popgenVCF-source-sbom.spdx.json",
    archival_metadata_dir = "archive-metadata"
  )
  path <- environment$write_source_release_provenance(provenance, asset_dir)
  cat("\nchanged\n", file = file.path(asset_dir, "popgenVCF-source-sbom.spdx.json"), append = TRUE)
  expect_error(
    environment$verify_source_release_provenance(path, asset_dir),
    "checksum mismatch|size mismatch"
  )
})

test_that("source-release provenance rejects invalid commit identities", {
  environment <- new.env(parent = globalenv())
  sys.source(release_provenance_script(), envir = environment)
  asset_dir <- make_release_provenance_fixture()
  expect_error(
    environment$build_source_release_provenance(
      asset_dir = asset_dir,
      package_name = "popgenVCF",
      package_version = "0.10.0",
      release_id = "v0.10.0",
      git_tag = "v0.10.0",
      git_commit = "not-a-sha",
      workflow_name = "test",
      workflow_run_id = "1",
      workflow_run_attempt = "1",
      source_archive = "popgenVCF_0.10.0.tar.gz",
      source_sbom = "popgenVCF-source-sbom.spdx.json",
      archival_metadata_dir = "archive-metadata"
    ),
    "40-character SHA"
  )
})
