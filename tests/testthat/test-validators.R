test_that("an unnamed membership collection is actually validated, not vacuously passed", {
  # names(collection) is NULL for an unnamed list, and `for (nm in NULL)`
  # never executes its body -- the pre-fix loop silently validated nothing
  # at all for an unnamed collection and returned zero errors regardless of
  # content. A matrix with a genuinely invalid membership value (negative
  # beyond tolerance) must be caught.
  invalid <- matrix(c(-0.5, 1.5, 0.5, 0.5), nrow = 2, byrow = TRUE,
                     dimnames = list(NULL, c("cluster_1", "cluster_2")))
  check <- validate_membership_collection(list(invalid))
  expect_true(length(check$errors) > 0L)
})

test_that("a plain data.frame membership matrix is validated, not crashed on", {
  # `q[, ..cols]` is data.table's own NSE escape and only resolves inside a
  # data.table `[` call -- a plain data.frame (explicitly accepted by this
  # function's own is.data.frame(q) check) previously crashed with "object
  # '..cols' not found" instead of being validated.
  good <- data.frame(cluster_1 = c(0.5, 0.5), cluster_2 = c(0.5, 0.5))
  check <- validate_membership_collection(list(`2` = good))
  expect_length(check$errors, 0L)
  expect_equal(check$metrics$K2_clusters, 2L)
})

test_that("a plain data.frame with an invalid membership value is caught, not crashed or vacuously passed", {
  bad_df <- data.frame(cluster_1 = c(-0.5, 0.5), cluster_2 = c(1.5, 0.5))
  check <- validate_membership_collection(list(`2` = bad_df))
  expect_true(any(grepl("K=2", check$errors)))
})
