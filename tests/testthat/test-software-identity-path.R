write_fake_identity <- function(root, content = "source-tree") {
  dir.create(file.path(root, "inst", "metadata"), recursive = TRUE)
  writeLines(content, file.path(root, "inst", "metadata", "software-identity.json"))
}

test_that("a reachable source tree is preferred over system.file(), even when both exist", {
  root <- withr::local_tempdir()
  write_fake_identity(root, "from-source-tree")
  withr::local_dir(root)

  fake_installed <- withr::local_tempfile()
  writeLines("from-installed-copy", fake_installed)
  testthat::local_mocked_bindings(
    system.file = function(...) fake_installed,
    .package = "popgenVCF"
  )

  path <- popgenVCF:::popgenvcf_software_identity_path()
  expect_identical(readLines(path), "from-source-tree")
})

test_that("a real installed copy is used when no source tree is reachable (the normal end-user case)", {
  root <- withr::local_tempdir()
  withr::local_dir(root)

  fake_installed <- withr::local_tempfile()
  writeLines("from-installed-copy", fake_installed)
  testthat::local_mocked_bindings(
    system.file = function(...) fake_installed,
    .package = "popgenVCF"
  )

  path <- popgenVCF:::popgenvcf_software_identity_path()
  expect_identical(path, fake_installed)
  expect_identical(readLines(path), "from-installed-copy")
})

test_that("an unreachable source tree and a missing install fail closed with a clear error", {
  root <- withr::local_tempdir()
  withr::local_dir(root)

  testthat::local_mocked_bindings(
    system.file = function(...) "",
    .package = "popgenVCF"
  )

  expect_error(
    popgenVCF:::popgenvcf_software_identity_path(),
    "canonical software identity metadata is unavailable"
  )
})

test_that("POPGENVCF_SOURCE_ROOT overrides the working-directory-based candidates", {
  root <- withr::local_tempdir()
  write_fake_identity(root, "from-env-override")
  withr::local_envvar(POPGENVCF_SOURCE_ROOT = root)

  unrelated <- withr::local_tempdir()
  withr::local_dir(unrelated)

  fake_installed <- withr::local_tempfile()
  writeLines("from-installed-copy", fake_installed)
  testthat::local_mocked_bindings(
    system.file = function(...) fake_installed,
    .package = "popgenVCF"
  )

  path <- popgenVCF:::popgenvcf_software_identity_path()
  expect_identical(readLines(path), "from-env-override")
})
