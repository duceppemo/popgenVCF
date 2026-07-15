test_that("ancestry alignment exactly recovers cluster permutations", {
  reference_q <- matrix(c(
    0.90, 0.08, 0.02,
    0.80, 0.15, 0.05,
    0.05, 0.90, 0.05,
    0.02, 0.08, 0.90
  ), ncol = 3, byrow = TRUE)
  target_q <- reference_q[, c(3, 1, 2)]

  out <- align_ancestry_replicate(target_q, reference_q)
  expect_s3_class(out, "PopgenVCFAncestryAlignment")
  expect_equal(out$aligned_q, reference_q, tolerance = 1e-12)
  expect_identical(out$permutation, c(2L, 3L, 1L))
  expect_equal(target_q %*% out$permutation_matrix, reference_q, tolerance = 1e-12)
  expect_equal(out$rmsd, 0, tolerance = 1e-12)
  expect_gt(out$alignment_score, 0.999999)
})

test_that("ancestry alignment preserves canonical replicate metadata", {
  ids <- paste0("s", 1:4)
  q <- matrix(c(0.9, 0.1, 0.8, 0.2, 0.2, 0.8, 0.1, 0.9), ncol = 2, byrow = TRUE)
  ref <- new_ancestry_replicate(ids, q, "admixture", replicate = 1)
  target <- new_ancestry_replicate(ids, q[, 2:1], "admixture", replicate = 2)

  out <- align_ancestry_replicate(target, ref)
  expect_s3_class(out$aligned_replicate, "PopgenVCFAncestryReplicate")
  expect_equal(out$aligned_replicate$q, q, tolerance = 1e-12)
  expect_identical(out$aligned_replicate$sample_ids, ids)
  expect_identical(out$aligned_replicate$replicate, 2L)
  expect_identical(out$aligned_replicate$provenance$alignment$permutation, c(2L, 1L))
})

test_that("ancestry alignment rejects incompatible inputs", {
  q <- matrix(c(0.8, 0.2, 0.1, 0.9), ncol = 2, byrow = TRUE)
  expect_error(align_ancestry_replicate(q[, 1, drop = FALSE], q), "identical dimensions")

  a <- new_ancestry_replicate(c("a", "b"), q, "snmf")
  b <- new_ancestry_replicate(c("b", "a"), q, "snmf", replicate = 2)
  expect_error(align_ancestry_replicate(b, a), "identical sample IDs")
})
