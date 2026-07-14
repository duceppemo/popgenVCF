test_that("bundled hand-calculated core validation passes", {
  x <- run_scientific_validation(integration = FALSE)
  expect_true(x$passed)
  expect_true(all(x$checks$passed))
})

test_that("PCA comparison is invariant to sign reversal", {
  reference <- cbind(PC1 = c(-2, -1, 1, 2), PC2 = c(1, -1, -1, 1))
  target <- cbind(PC1 = -reference[, 1], PC2 = reference[, 2])
  expect_equal(align_pca_signs(target, reference), reference)
})

test_that("validation comparator reports mismatches", {
  x <- compare_validation_values(c(1, 2), c(1, 2.1), tolerance = 0.01)
  expect_false(x$passed)
  expect_equal(x$max_absolute_difference, 0.1, tolerance = 1e-12)
})

test_that("package tree has no case-insensitive filename collisions", {
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)
  paths <- list.files(root, recursive = TRUE, all.files = TRUE, include.dirs = FALSE)
  duplicate <- duplicated(tolower(paths)) | duplicated(tolower(paths), fromLast = TRUE)
  expect_length(paths[duplicate], 0L)
})
