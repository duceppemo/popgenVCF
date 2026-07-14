test_that("call_supported drops unsupported optional arguments", {
  f <- function(x, y = 1) x + y
  result <- popgenVCF:::call_supported(
    f,
    list(x = 2, y = 3, verbose = FALSE),
    "f"
  )
  expect_identical(result, 5)
})

test_that("SNP rate-frequency API arguments are version tolerant", {
  formals_now <- names(formals(SNPRelate::snpgdsSNPRateFreq))
  expect_true(all(c("gdsobj", "sample.id") %in% formals_now) ||
                all(c("gds", "sample.id") %in% formals_now) ||
                "..." %in% formals_now)
})
