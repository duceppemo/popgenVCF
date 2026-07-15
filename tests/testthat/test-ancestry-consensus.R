test_that("consensus ancestry removes label switching before averaging", {
  ids <- paste0("s", 1:4)
  q <- matrix(c(
    0.90, 0.08, 0.02,
    0.80, 0.15, 0.05,
    0.05, 0.90, 0.05,
    0.02, 0.08, 0.90
  ), ncol = 3, byrow = TRUE)
  reps <- list(
    new_ancestry_replicate(ids, q, "admixture", replicate = 1),
    new_ancestry_replicate(ids, q[, c(3, 1, 2)], "admixture", replicate = 2),
    new_ancestry_replicate(ids, q[, c(2, 3, 1)], "admixture", replicate = 3)
  )

  out <- consensus_ancestry(new_ancestry_result(reps))
  expect_s3_class(out, "PopgenVCFAncestryConsensus")
  expect_equal(out$mean_q, q, tolerance = 1e-12, ignore_attr = TRUE)
  expect_equal(out$median_q, q, tolerance = 1e-12, ignore_attr = TRUE)
  expect_equal(out$sd_q, matrix(0, 4, 3), tolerance = 1e-12, ignore_attr = TRUE)
  expect_equal(out$global_stability, 1, tolerance = 1e-12)
  expect_identical(out$reference_replicate, 1L)
  expect_true(all(out$alignment_table$rmsd < 1e-12))
})

test_that("consensus ancestry reports uncertainty for noisy replicates", {
  ids <- paste0("s", 1:4)
  q1 <- matrix(c(0.9, 0.1, 0.8, 0.2, 0.2, 0.8, 0.1, 0.9), ncol = 2, byrow = TRUE)
  q2 <- matrix(c(0.85, 0.15, 0.75, 0.25, 0.25, 0.75, 0.15, 0.85), ncol = 2, byrow = TRUE)
  q3 <- matrix(c(0.88, 0.12, 0.78, 0.22, 0.22, 0.78, 0.12, 0.88), ncol = 2, byrow = TRUE)
  reps <- list(
    new_ancestry_replicate(ids, q1, "snmf", replicate = 3),
    new_ancestry_replicate(ids, q2[, 2:1], "snmf", replicate = 1),
    new_ancestry_replicate(ids, q3, "snmf", replicate = 2)
  )

  out <- consensus_ancestry(reps, confidence = 0.9)
  expect_true(all(out$sd_q > 0))
  expect_true(all(out$lower_q <= out$mean_q))
  expect_true(all(out$upper_q >= out$mean_q))
  expect_true(out$global_stability < 1)
  expect_true(out$global_stability > 0)
  expect_equal(nrow(out$cluster_stability), 2)
  expect_equal(nrow(out$sample_uncertainty), 4)

  tab <- ancestry_consensus_table(out)
  expect_equal(nrow(tab), 8)
  expect_true(all(c("sample_id", "cluster", "mean", "median", "variance", "sd", "lower", "upper") %in% names(tab)))
})

test_that("single-replicate consensus has zero uncertainty", {
  q <- matrix(c(0.7, 0.3, 0.2, 0.8), ncol = 2, byrow = TRUE)
  rep <- new_ancestry_replicate(c("a", "b"), q, "faststructure")
  out <- consensus_ancestry(list(rep))
  expect_equal(out$mean_q, q, ignore_attr = TRUE)
  expect_equal(out$variance_q, matrix(0, 2, 2), ignore_attr = TRUE)
  expect_equal(out$lower_q, q, ignore_attr = TRUE)
  expect_equal(out$upper_q, q, ignore_attr = TRUE)
})

test_that("consensus ancestry rejects incompatible replicate collections", {
  ids <- c("a", "b")
  q2 <- matrix(c(0.8, 0.2, 0.1, 0.9), ncol = 2, byrow = TRUE)
  q3 <- matrix(c(0.7, 0.2, 0.1, 0.1, 0.2, 0.7), ncol = 3, byrow = TRUE)
  a <- new_ancestry_replicate(ids, q2, "admixture", replicate = 1)
  b <- new_ancestry_replicate(ids, q3, "admixture", replicate = 2)
  c <- new_ancestry_replicate(ids, q2, "snmf", replicate = 2)

  expect_error(consensus_ancestry(list(a, b)), "same K")
  expect_error(consensus_ancestry(list(a, c)), "same backend")
  expect_error(consensus_ancestry(list(a), confidence = 1), "strictly between")
  expect_error(consensus_ancestry(list(a), reference_replicate = 99), "not present")
})
