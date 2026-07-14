test_that("haversine matrix is symmetric", {
  x <- popgenVCF:::haversine_matrix(c(45,46,47), c(-75,-76,-77), c("a","b","c"))
  expect_equal(x, t(x))
  expect_equal(unname(diag(x)), c(0, 0, 0))
})
