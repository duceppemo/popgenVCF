test_that("ADMIXTURE CV output parses", {
  x <- popgenVCF:::parse_admixture_cv("CV error (K=4): 0.123456")
  expect_equal(x$K, 4L)
  expect_equal(x$cv_error, 0.123456)
})
