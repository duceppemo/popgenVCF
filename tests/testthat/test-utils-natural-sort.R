test_that("natural_sort_key orders embedded numbers numerically, not lexicographically", {
  x <- c("K10", "K2", "K1", "K9")
  expect_identical(x[order(popgenVCF:::natural_sort_key(x))], c("K1", "K2", "K9", "K10"))
})

test_that("natural_sort_key handles multiple embedded numbers and mixed alpha/numeric strings", {
  x <- c("chr2_window10", "chr10_window2", "chr2_window2")
  ordered <- x[order(popgenVCF:::natural_sort_key(x))]
  expect_identical(ordered, c("chr2_window2", "chr2_window10", "chr10_window2"))
})

test_that("natural_sort_key sorts digit-leading strings before non-numeric ones (1..22 before X, Y)", {
  x <- c("Y", "10", "2", "X", "1")
  ordered <- x[order(popgenVCF:::natural_sort_key(x))]
  expect_identical(ordered, c("1", "2", "10", "X", "Y"))
})

test_that("natural_sort_levels returns unique values in natural order", {
  x <- c("PC10", "PC2", "PC1", "PC2", "PC10")
  expect_identical(popgenVCF:::natural_sort_levels(x), c("PC1", "PC2", "PC10"))
})
