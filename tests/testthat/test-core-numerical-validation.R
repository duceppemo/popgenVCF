test_that("manual IBS has expected matrix properties", {
  x <- matrix(c(0, 0, 2, 2, 0, 2), nrow = 3, byrow = TRUE,
              dimnames = list(c("a", "b", "c"), NULL))
  ibs <- manual_ibs_matrix(x)
  expect_equal(diag(ibs), rep(1, 3), ignore_attr = TRUE)
  expect_equal(ibs, t(ibs))
  expect_equal(ibs[1, 2], 0)
})

test_that("PCA subspace comparison is sign and rotation invariant", {
  set.seed(42)
  x <- matrix(rnorm(40), 10, 4)
  q <- qr.Q(qr(x))[, 1:2, drop = FALSE]
  rotation <- matrix(c(0, -1, 1, 0), 2, 2)
  result <- compare_pca_subspaces(q, q %*% rotation, 2)
  expect_gte(result$minimum, 1 - 1e-12)
})

test_that("manual population diversity returns finite summaries", {
  x <- matrix(c(0, 1, 2, 0, 0, 2, 1, 2), nrow = 4, byrow = TRUE)
  ans <- manual_population_diversity(x, c("A", "A", "B", "B"))
  expect_equal(ans$population, c("A", "B"))
  expect_true(all(is.finite(ans$observed_heterozygosity)))
})
