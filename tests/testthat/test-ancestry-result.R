test_that("ancestry replicate validates canonical Q matrices", {
  q <- matrix(c(0.9, 0.1, 0.2, 0.8), nrow = 2, byrow = TRUE)
  x <- new_ancestry_replicate(
    sample_ids = c("s1", "s2"), q = q, backend = "ADMIXTURE",
    k = 2, replicate = 1, seed = 11,
    metrics = c(cv_error = 0.42), converged = TRUE,
    runtime_seconds = 1.5, provenance = list(version = "1.3")
  )

  expect_s3_class(x, "PopgenVCFAncestryReplicate")
  expect_identical(x$backend, "admixture")
  expect_identical(x$k, 2L)
  expect_silent(validate_ancestry_replicate(x))
  expect_equal(ancestry_q_table(x)$ancestry, c(0.9, 0.2, 0.1, 0.8))
})

test_that("ancestry replicate rejects invalid identity and Q contracts", {
  q <- matrix(c(0.7, 0.3, 0.4, 0.6), nrow = 2, byrow = TRUE)
  expect_error(new_ancestry_replicate(c("s1", "s1"), q, "snmf"), "unique")
  expect_error(new_ancestry_replicate(c("s1", "s2"), q * 0.5, "snmf"), "sum to one")
  bad <- q; bad[1, 1] <- -0.1; bad[1, 2] <- 1.1
  expect_error(new_ancestry_replicate(c("s1", "s2"), bad, "snmf"), "lie in")
  expect_error(new_ancestry_replicate(c("s1", "s2"), q, "unknown"), "unsupported")
})

test_that("ancestry collections enforce replicate identity and sample order", {
  q <- matrix(c(0.8, 0.2, 0.3, 0.7), nrow = 2, byrow = TRUE)
  a <- new_ancestry_replicate(c("s1", "s2"), q, "admixture", replicate = 1, metrics = c(cv_error = 0.4))
  b <- new_ancestry_replicate(c("s1", "s2"), q[, 2:1], "admixture", replicate = 2, metrics = c(cv_error = 0.41))
  x <- new_ancestry_result(list(a, b))

  expect_s3_class(x, "PopgenVCFAncestryResult")
  tab <- ancestry_result_table(x)
  expect_equal(nrow(tab), 2)
  expect_identical(tab$replicate, c(1L, 2L))

  duplicate <- new_ancestry_replicate(c("s1", "s2"), q, "admixture", replicate = 1)
  expect_error(new_ancestry_result(list(a, duplicate)), "must be unique")
  reordered <- new_ancestry_replicate(c("s2", "s1"), q, "admixture", replicate = 3)
  expect_error(new_ancestry_result(list(a, reordered)), "identical sample IDs")
})

test_that("empty metrics are represented by a stable summary row", {
  q <- matrix(c(0.6, 0.4), nrow = 1)
  x <- new_ancestry_replicate("s1", q, "faststructure")
  tab <- ancestry_result_table(x)
  expect_equal(nrow(tab), 1)
  expect_true(is.na(tab$metric))
  expect_true(is.na(tab$value))
})
