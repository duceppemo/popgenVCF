test_that("project UUID generation does not advance the analysis RNG", {
  set.seed(123L)
  expected <- stats::runif(3L)
  set.seed(123L)
  invisible(new_popgenvcf_project("rng-safe"))
  observed <- stats::runif(3L)
  expect_identical(observed, expected)
})
