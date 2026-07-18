release_manifest_script <- function() {
  installed <- system.file("scripts", "build_release_manifest.R", package = "popgenVCF")
  if (nzchar(installed)) return(installed)
  testthat::test_path("..", "..", "inst", "scripts", "build_release_manifest.R")
}

test_that("release manifests are deterministic and detect tampering", {
  environment <- new.env(parent = globalenv())
  sys.source(release_manifest_script(), envir = environment)

  asset_dir <- tempfile("release-assets-")
  dir.create(file.path(asset_dir, "nested"), recursive = TRUE)
  writeLines("alpha", file.path(asset_dir, "alpha.txt"))
  writeLines("beta", file.path(asset_dir, "nested", "beta.txt"))

  manifest <- environment$build_release_manifest(
    asset_dir = asset_dir,
    package_name = "popgenVCF",
    package_version = "0.8.3",
    release_id = "v0.8.3",
    git_tag = "v0.8.3",
    git_commit = paste(rep("a", 40L), collapse = ""),
    r_version = "4.5.0",
    workflow_name = "test",
    workflow_run_id = "1",
    workflow_run_attempt = "1",
    created_at = "1970-01-01T00:00:00Z"
  )
  environment$write_release_manifest(manifest, asset_dir)

  expect_true(environment$verify_release_manifest(asset_dir))
  expect_true(environment$run_tamper_test(asset_dir))

  first_manifest <- readLines(file.path(asset_dir, "release-manifest.json"))
  manifest_again <- environment$build_release_manifest(
    asset_dir = asset_dir,
    package_name = "popgenVCF",
    package_version = "0.8.3",
    release_id = "v0.8.3",
    git_tag = "v0.8.3",
    git_commit = paste(rep("a", 40L), collapse = ""),
    r_version = "4.5.0",
    workflow_name = "test",
    workflow_run_id = "1",
    workflow_run_attempt = "1",
    created_at = "1970-01-01T00:00:00Z"
  )
  environment$write_release_manifest(manifest_again, asset_dir)
  expect_identical(readLines(file.path(asset_dir, "release-manifest.json")), first_manifest)

  cat("\nchanged\n", file = file.path(asset_dir, "alpha.txt"), append = TRUE)
  expect_error(
    environment$verify_release_manifest(asset_dir),
    "checksum mismatch|size mismatch"
  )
})

test_that("release manifests reject missing and unexpected payload assets", {
  environment <- new.env(parent = globalenv())
  sys.source(release_manifest_script(), envir = environment)

  asset_dir <- tempfile("release-assets-")
  dir.create(asset_dir)
  writeLines("alpha", file.path(asset_dir, "alpha.txt"))

  manifest <- environment$build_release_manifest(
    asset_dir = asset_dir,
    package_name = "popgenVCF",
    package_version = "0.8.3",
    release_id = "v0.8.3",
    git_tag = "v0.8.3",
    git_commit = paste(rep("b", 40L), collapse = ""),
    r_version = "4.5.0",
    workflow_name = "test",
    workflow_run_id = "2",
    workflow_run_attempt = "1"
  )
  environment$write_release_manifest(manifest, asset_dir)

  unlink(file.path(asset_dir, "alpha.txt"))
  expect_error(environment$verify_release_manifest(asset_dir), "missing")

  writeLines("alpha", file.path(asset_dir, "alpha.txt"))
  writeLines("unexpected", file.path(asset_dir, "unexpected.txt"))
  expect_error(environment$verify_release_manifest(asset_dir), "differs from manifest")
})
