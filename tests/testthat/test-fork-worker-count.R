test_that("fork_worker_count is bounded by task count, thread budget, and platform support", {
  expect_identical(popgenVCF:::fork_worker_count(7L, 64L, fork_available = TRUE), 7L)
  expect_identical(popgenVCF:::fork_worker_count(7L, 3L, fork_available = TRUE), 3L)
  expect_identical(popgenVCF:::fork_worker_count(7L, 64L, fork_available = FALSE), 1L)
  expect_identical(popgenVCF:::fork_worker_count(7L, NA_integer_, fork_available = TRUE), 1L)
  expect_identical(popgenVCF:::fork_worker_count(0L, 64L, fork_available = TRUE), 1L)
  expect_identical(popgenVCF:::fork_worker_count(NA_integer_, 64L, fork_available = TRUE), 1L)
})
