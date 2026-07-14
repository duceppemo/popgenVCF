test_that("LD-pruned SNP IDs remain plain vectors", {
  x <- c(1L, 2L, 3L)
  expect_true(is.vector(x))
  expect_null(attributes(x))

  # Regression guard: custom metadata attributes make is.vector() FALSE and
  # are rejected by SNPRelate's snp.id checks.
  y <- x
  attr(y, "threads") <- 4L
  expect_false(is.vector(y))
  expect_true(is.vector(as.vector(y)))
  expect_null(attributes(as.vector(y)))
})
