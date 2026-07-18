test_that("release metadata is synchronized with DESCRIPTION", {
  package_root <- normalizePath(testthat::test_path("..", ".."), winslash = "/", mustWork = TRUE)
  script <- file.path(package_root, "scripts", "validate_release_metadata.R")

  skip_if_not(file.exists(script), "release metadata validator unavailable in built source package")
  skip_if_not_installed("jsonlite")

  output <- system2(
    file.path(R.home("bin"), "Rscript"),
    script,
    stdout = TRUE,
    stderr = TRUE,
    env = paste0("R_METADATA_ROOT=", package_root)
  )

  expect_equal(attr(output, "status") %||% 0L, 0L, info = paste(output, collapse = "\n"))
  expect_true(any(grepl("Release metadata is valid", output, fixed = TRUE)))
})
