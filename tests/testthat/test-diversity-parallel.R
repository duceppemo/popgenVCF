diversity_parallel_fixture_gds <- function() {
  set.seed(11)
  n <- 30L; l <- 40L
  sample_id <- paste0("s", seq_len(n))
  snp_id <- seq_len(l)
  population <- rep(c("PopA", "PopB", "PopC"), each = 10L)
  # A real, non-degenerate allele-frequency structure per population, so
  # snpgdsHWE()/private-allele detection genuinely exercise real data, not a
  # trivially uniform fixture.
  genmat <- matrix(NA_integer_, nrow = n, ncol = l)
  for (j in seq_len(l)) {
    for (pop in unique(population)) {
      idx <- population == pop
      p <- stats::runif(1, 0.1, 0.9)
      genmat[idx, j] <- stats::rbinom(sum(idx), 2L, p)
    }
  }
  # A few missing calls, matching real data.
  genmat[sample(seq_along(genmat), 15L)] <- NA_integer_

  gds_path <- tempfile(fileext = ".gds")
  SNPRelate::snpgdsCreateGeno(
    gds_path, genmat = genmat, sample.id = sample_id, snp.id = snp_id,
    snp.chromosome = rep(c(1L, 2L), each = l / 2L), snp.position = (seq_len(l) * 100L),
    snp.allele = rep("A/G", l), snpfirstdim = FALSE
  )
  metadata <- popgenVCF:::normalize_sample_aliases(data.table::data.table(
    sample = sample_id, population = population
  ))
  list(path = gds_path, sample_id = sample_id, metadata = metadata)
}

test_that("parallel diversity (per-population fork, independent GDS connections) matches the sequential result exactly", {
  fx <- diversity_parallel_fixture_gds()
  gds <- SNPRelate::snpgdsOpen(fx$path)
  on.exit(SNPRelate::snpgdsClose(gds), add = TRUE)
  ids <- popgenVCF:::get_gds_ids(gds)

  serial <- popgenVCF:::compute_diversity(
    gds, fx$sample_id, ids$snp, fx$metadata, ids,
    gds_path = NULL, threads = 1L
  )
  parallel_result <- popgenVCF:::compute_diversity(
    gds, fx$sample_id, ids$snp, fx$metadata, ids,
    gds_path = fx$path, threads = 3L
  )

  expect_identical(nrow(parallel_result$locus), nrow(serial$locus))
  # Row order can legitimately differ (mc.preschedule = FALSE dispatches
  # populations to whichever worker is free first), so compare sorted by
  # the same key rather than assuming identical row order.
  key <- c("population", "snp_id")
  data.table::setorderv(serial$locus, key)
  data.table::setorderv(parallel_result$locus, key)
  expect_equal(parallel_result$locus, serial$locus, tolerance = 1e-12)
  expect_equal(
    parallel_result$population[order(population)],
    serial$population[order(population)],
    tolerance = 1e-12
  )
})

test_that("compute_diversity falls back to sequential when gds_path is NULL, regardless of threads", {
  fx <- diversity_parallel_fixture_gds()
  gds <- SNPRelate::snpgdsOpen(fx$path)
  on.exit(SNPRelate::snpgdsClose(gds), add = TRUE)
  ids <- popgenVCF:::get_gds_ids(gds)

  # threads = 8 but no gds_path -- must not attempt to fork (would error
  # immediately, since mclapply would try to reuse the shared, already-open
  # `gds` handle across workers, exactly the unsafe pattern this is guarding
  # against).
  result <- popgenVCF:::compute_diversity(
    gds, fx$sample_id, ids$snp, fx$metadata, ids, gds_path = NULL, threads = 8L
  )
  expect_identical(nrow(result$locus), 3L * length(ids$snp))
})

test_that("a real per-worker failure surfaces as a clear, non-silent error rather than a corrupted result", {
  fx <- diversity_parallel_fixture_gds()
  gds <- SNPRelate::snpgdsOpen(fx$path)
  on.exit(SNPRelate::snpgdsClose(gds), add = TRUE)
  ids <- popgenVCF:::get_gds_ids(gds)

  # A gds_path pointing at a file that does not exist makes every forked
  # worker's own SNPRelate::snpgdsOpen() call fail -- a real, guaranteed
  # failure mode (unlike guessing at SNPRelate's exact behavior for a bogus
  # snp.id), proving mclapply's per-worker errors are actually caught and
  # surfaced, not silently dropped or left as corrupted rows.
  expect_error(
    popgenVCF:::compute_diversity(
      gds, fx$sample_id, ids$snp, fx$metadata, ids,
      gds_path = tempfile(fileext = ".gds"), threads = 3L
    ),
    "Parallel diversity computation failed"
  )
})
