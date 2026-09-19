test_that("submission_package_role classifies assets at manuscript_copy_assets()'s real 'assets/<folder>/...' paths", {
  # manuscript_copy_assets() (manuscript_asset_integration.R) places every
  # figure/table/supplementary artifact under "assets/<folder>/...", not
  # "<folder>/..." directly -- this function's own asset check tested the
  # un-prefixed form, silently mislabeling every real asset
  # "supporting_metadata" instead of "asset" in every generated
  # submission-package plan/manifest.
  expect_identical(popgenVCF:::submission_package_role("assets/figures/abc123_plot.png"), "asset")
  expect_identical(popgenVCF:::submission_package_role("assets/tables/abc123_table.tsv"), "asset")
  expect_identical(popgenVCF:::submission_package_role("assets/supplementary/abc123_extra.pdf"), "asset")
})

test_that("submission package plans are deterministic", {
  project <- new_popgenvcf_project("submission-plan")
  manuscript <- new_manuscript(project, title = "Submission plan")
  directory <- tempfile()
  write_manuscript(manuscript, directory)
  first <- submission_package_plan(directory)
  second <- submission_package_plan(directory)
  expect_identical(first, second)
  expect_true(all(c("role", "source", "destination", "size_bytes", "sha256") %in% names(first)))
  expect_true(any(first$destination == "submission/manuscript.md"))
})

test_that("submission packages are written and verified", {
  project <- new_popgenvcf_project("submission-write")
  manuscript <- new_manuscript(project, title = "Submission write")
  directory <- tempfile()
  write_manuscript(manuscript, directory)
  archive <- tempfile(fileext = ".tar.gz")
  record <- write_submission_package(directory, archive)
  expect_s3_class(record, "PopgenVCFSubmissionPackage")
  expect_true(file.exists(archive))
  expect_true(verify_submission_package(archive))
  expect_error(write_submission_package(directory, archive), "already exists")
})

test_that("submission package verification detects corruption", {
  project <- new_popgenvcf_project("submission-corrupt")
  manuscript <- new_manuscript(project, title = "Submission corruption")
  directory <- tempfile()
  write_manuscript(manuscript, directory)
  archive <- tempfile(fileext = ".tar.gz")
  write_submission_package(directory, archive)
  extracted <- tempfile()
  dir.create(extracted)
  utils::untar(archive, exdir = extracted)
  writeLines("modified", file.path(extracted, "submission", "manuscript.md"))
  old <- setwd(extracted); on.exit(setwd(old), add = TRUE)
  utils::tar(archive, files = "submission", compression = "gzip", tar = "internal")
  expect_error(verify_submission_package(archive), "checksum mismatch")
})
