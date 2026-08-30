# check_mclapply_results() is the single shared guard against a real,
# recurring bug class found in a pre-release audit: multiple mclapply() call
# sites (run_fst(), execute_dapc_k_tasks(), compute_diversity(),
# run_genome_scan_fst(), run_genome_scan_diversity()) checked only for a
# classed "try-error" result and let unlist()/rbindlist() silently drop a
# NULL result -- which is what a worker process killed outright by a signal
# (OOM, segfault) returns, not a "try-error". Dropping a NULL either shrinks
# the output (a population's rows vanish) or, worse, desynchronizes a
# positionally zipped result from its labels (run_fst()'s pairwise values
# silently shifting onto the wrong population pairs). These tests exercise
# the shared checker directly so every call site inherits the same,
# thoroughly-tested guarantee.

test_that("check_mclapply_results passes through a clean result set unchanged", {
  results <- list(1, 2, 3)
  expect_identical(
    popgenVCF:::check_mclapply_results(results, c("a", "b", "c"), "widget task"), results
  )
})

test_that("check_mclapply_results reports a killed worker's NULL slot, naming the right label(s)", {
  results <- list(1, NULL, 3)
  expect_error(
    popgenVCF:::check_mclapply_results(results, c("a", "b", "c"), "widget task"),
    "widget task.*terminated abnormally.*\\bb\\b"
  )
})

test_that("check_mclapply_results reports every killed worker, not just the first", {
  results <- list(NULL, 2, NULL)
  expect_error(
    popgenVCF:::check_mclapply_results(results, c("a", "b", "c"), "widget task"),
    "a, c"
  )
})

test_that("check_mclapply_results re-signals a try-error's real condition message", {
  bad <- structure("simulated real failure", class = "try-error",
                    condition = simpleError("simulated real failure"))
  results <- list(1, bad, 3)
  expect_error(
    popgenVCF:::check_mclapply_results(results, c("a", "b", "c"), "widget task"),
    "simulated real failure"
  )
})

test_that("check_mclapply_results checks try-error before NULL when both are present", {
  bad <- structure("boom", class = "try-error", condition = simpleError("boom"))
  results <- list(bad, NULL)
  expect_error(
    popgenVCF:::check_mclapply_results(results, c("a", "b"), "widget task"),
    "boom"
  )
})
