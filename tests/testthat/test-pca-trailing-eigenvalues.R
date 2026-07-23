test_that("PCA normalization discards only trailing undefined solver slots", {
  eigenvalues <- c(seq(10, 1), rep(NaN, 31L))
  normalized <- popgenVCF:::normalize_pca_eigenvalues(eigenvalues)

  expect_equal(normalized$values, seq(10, 1))
  expect_identical(normalized$discarded_nonfinite, 31L)
})

test_that("PCA normalization remains strict about malformed eigensystems", {
  expect_error(
    popgenVCF:::normalize_pca_eigenvalues(c(4, NA_real_)),
    "within the requested eigensystem"
  )
  expect_error(
    popgenVCF:::normalize_pca_eigenvalues(c(4, NA_real_, 2)),
    "within the requested eigensystem"
  )
  expect_error(
    popgenVCF:::normalize_pca_eigenvalues(c(4, 2, Inf)),
    "infinite eigenvalue"
  )
})

test_that("the compatibility definition is loaded after ordination", {
  body_text <- paste(
    deparse(body(popgenVCF:::normalize_pca_eigenvalues)),
    collapse = "\n"
  )

  expect_match(body_text, "discarded_nonfinite", fixed = TRUE)
})
