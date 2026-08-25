fst_parallel_fixture_gds <- function() {
  set.seed(21)
  n_pop <- 4L; n_per_pop <- 6L; n_snp <- 50L
  populations <- rep(LETTERS[1:n_pop], each = n_per_pop)
  # Distinct allele frequencies per population, so pairwise FST is genuinely
  # non-degenerate, not just formula self-consistency.
  base_freqs <- c(A = 0.2, B = 0.4, C = 0.6, D = 0.8)
  sample_id <- paste0("S", seq_len(length(populations)))
  genmat <- matrix(NA_integer_, nrow = length(populations), ncol = n_snp)
  for (j in seq_len(n_snp)) {
    for (i in seq_along(populations)) genmat[i, j] <- rbinom(1, 2, base_freqs[[populations[i]]])
  }
  gds_path <- tempfile(fileext = ".gds")
  SNPRelate::snpgdsCreateGeno(
    gds_path, genmat = genmat, sample.id = sample_id, snp.id = seq_len(n_snp),
    snp.chromosome = rep(1L, n_snp), snp.position = seq_len(n_snp) * 100L,
    snp.allele = rep("A/G", n_snp), snpfirstdim = FALSE
  )
  metadata <- data.table::data.table(sample = sample_id, population = populations)
  list(path = gds_path, snp_ids = seq_len(n_snp), metadata = metadata)
}

test_that("parallel run_fst (per-pair fork, independent GDS connections) matches the sequential result exactly", {
  fx <- fst_parallel_fixture_gds()
  gds <- SNPRelate::snpgdsOpen(fx$path)
  on.exit(SNPRelate::snpgdsClose(gds), add = TRUE)

  serial <- popgenVCF:::run_fst(gds, fx$snp_ids, fx$metadata, gds_path = NULL, threads = 1L)
  parallel_result <- popgenVCF:::run_fst(gds, fx$snp_ids, fx$metadata, gds_path = fx$path, threads = 3L)

  expect_identical(nrow(parallel_result$long), nrow(serial$long))
  expect_identical(nrow(parallel_result$long), 6L) # choose(4, 2)
  # Row order can legitimately differ (mc.preschedule = FALSE dispatches
  # pairs to whichever worker is free first), so compare sorted by the same
  # key rather than assuming identical row order.
  key <- c("population_1", "population_2")
  data.table::setorderv(serial$long, key)
  data.table::setorderv(parallel_result$long, key)
  expect_equal(parallel_result$long, serial$long, tolerance = 1e-12)
  expect_equal(parallel_result$matrix, serial$matrix, tolerance = 1e-12)
  expect_identical(parallel_result$global, serial$global)
})

test_that("run_fst falls back to sequential when gds_path is NULL, regardless of threads", {
  fx <- fst_parallel_fixture_gds()
  gds <- SNPRelate::snpgdsOpen(fx$path)
  on.exit(SNPRelate::snpgdsClose(gds), add = TRUE)

  # threads = 8 but no gds_path -- must not attempt to fork (would try to
  # reuse the shared, already-open `gds` handle across workers, exactly the
  # unsafe pattern this is guarding against).
  result <- popgenVCF:::run_fst(gds, fx$snp_ids, fx$metadata, gds_path = NULL, threads = 8L)
  expect_identical(nrow(result$long), 6L)
})

test_that("a real per-pair failure surfaces as a clear, non-silent error rather than a corrupted result", {
  fx <- fst_parallel_fixture_gds()
  gds <- SNPRelate::snpgdsOpen(fx$path)
  on.exit(SNPRelate::snpgdsClose(gds), add = TRUE)

  # A gds_path pointing at a file that does not exist makes every forked
  # worker's own SNPRelate::snpgdsOpen() call fail -- a real, guaranteed
  # failure mode, proving mclapply's per-worker errors are actually caught
  # and surfaced, not silently dropped or left as corrupted rows.
  expect_error(
    popgenVCF:::run_fst(gds, fx$snp_ids, fx$metadata, gds_path = tempfile(fileext = ".gds"), threads = 3L),
    "Parallel FST computation failed"
  )
})
