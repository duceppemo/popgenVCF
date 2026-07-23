test_that("PCA eigenvalue normalization clips only numerical residue", {
  normalized <- popgenVCF:::normalize_pca_eigenvalues(c(4, 2, 1e-16, -1e-12))

  expect_equal(normalized$values, c(4, 2, 0, 0))
  expect_equal(normalized$adjusted_negative, 1L)
  expect_gt(normalized$tolerance, 1e-12)

  small_scale <- popgenVCF:::normalize_pca_eigenvalues(
    c(4e-12, 2e-12, -1e-24)
  )
  expect_equal(small_scale$values[1:2], c(4e-12, 2e-12))
  expect_equal(small_scale$values[[3L]], 0)

  expect_error(
    popgenVCF:::normalize_pca_eigenvalues(c(4, 2, -0.1)),
    "materially negative"
  )
  expect_error(
    popgenVCF:::normalize_pca_eigenvalues(c(0, 0)),
    "no positive genetic variance"
  )
  expect_error(
    popgenVCF:::normalize_pca_eigenvalues(c(4, NA_real_)),
    "non-finite eigenvalue"
  )
})

test_that("PCA registry publishes normalized runtime eigenvalues", {
  module_body <- paste(
    deparse(body(popgenVCF:::run_module_pca)),
    collapse = "\n"
  )

  expect_match(module_body, "eigenvalues = pca$eigenvalues", fixed = TRUE)
  expect_false(grepl("eigenvalues = pca$object$eigenval", module_body, fixed = TRUE))
})
