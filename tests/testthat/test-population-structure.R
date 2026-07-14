test_that("cluster comparison is invariant to label switching", {
  q <- popgenVCF:::synthetic_structure_membership(n_per_cluster = 5, k = 3, seed = 7)
  target <- q[, c(3, 1, 2), drop = FALSE]
  x <- compare_q_matrices(target, q)
  expect_lt(x$maximum_absolute_difference, 1e-12)
  expect_equal(x$minimum_correlation, 1, tolerance = 1e-12)
})

test_that("replicate structure diagnostics identify stable results", {
  q <- popgenVCF:::synthetic_structure_membership(n_per_cluster = 5, k = 3, seed = 7)
  x <- structure_reproducibility(list(a = q, b = q[, c(2, 3, 1)]))
  expect_true(all(x$metrics$rmse < 1e-12))
  expect_equal(rowSums(x$consensus), rep(1, nrow(q)), tolerance = 1e-12)
})

test_that("K selection respects criterion direction", {
  x <- data.frame(K = 2:4, cv_error = c(.5, .3, .4), mean_success = c(.7, .9, .8))
  z <- select_structure_k(x)
  expect_true(all(z$best_by_method$K == 3L))
  expect_equal(z$consensus_k, 3L)
})

test_that("deterministic population structure validation passes", {
  x <- run_population_structure_validation(integration = FALSE)
  expect_true(x$passed)
  expect_true(all(x$checks$passed))
})

test_that("cluster assignment accepts negative similarities", {
  similarity <- matrix(
    c(1.0, -0.5, -0.5,
      -0.5, 1.0, -0.5,
      -0.5, -0.5, 1.0),
    nrow = 3,
    byrow = TRUE
  )
  assignment <- popgenVCF:::solve_cluster_assignment(similarity)
  expect_equal(assignment, 1:3)
})

test_that("shifting similarities does not change the optimal assignment", {
  similarity <- matrix(c(-0.2, 0.8, 0.9, -0.1), nrow = 2, byrow = TRUE)
  expected <- popgenVCF:::solve_cluster_assignment(similarity)
  shifted <- popgenVCF:::solve_cluster_assignment(similarity + 10)
  expect_equal(expected, shifted)
})

test_that("non-finite similarities are rejected", {
  similarity <- diag(2)
  similarity[1, 2] <- NA_real_
  expect_error(
    popgenVCF:::solve_cluster_assignment(similarity),
    "finite values"
  )
})
