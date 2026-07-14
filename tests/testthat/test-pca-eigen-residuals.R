test_that("PCA eigen residual validation accepts exact eigenvectors", {
  g <- matrix(c(2, 1, 1, 2), 2, 2)
  e <- eigen(g, symmetric = TRUE)
  z <- pca_eigen_residuals(g, e$vectors, 2)
  expect_lt(z$maximum_residual, 1e-12)
  expect_lt(z$orthogonality_error, 1e-12)
})
