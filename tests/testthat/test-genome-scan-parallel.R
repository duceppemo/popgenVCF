genome_scan_fst_parallel_fixture_gds <- function() {
  set.seed(31)
  n_pop <- 4L; n_per_pop <- 6L
  populations <- rep(LETTERS[1:n_pop], each = n_per_pop)
  sample_id <- paste0("S", seq_len(length(populations)))
  base_freqs <- c(A = 0.2, B = 0.4, C = 0.6, D = 0.8)
  n_snp_per_chr <- 100L
  chromosome <- rep(c(1L, 2L), each = n_snp_per_chr)
  position <- rep(seq(100L, by = 100L, length.out = n_snp_per_chr), 2L)
  n_snp <- length(chromosome)
  genmat <- matrix(NA_integer_, nrow = length(populations), ncol = n_snp)
  for (j in seq_len(n_snp)) {
    for (i in seq_along(populations)) genmat[i, j] <- rbinom(1, 2, base_freqs[[populations[i]]])
  }
  gds_path <- tempfile(fileext = ".gds")
  SNPRelate::snpgdsCreateGeno(
    gds_path, genmat = genmat, sample.id = sample_id, snp.id = seq_len(n_snp),
    snp.chromosome = chromosome, snp.position = position,
    snp.allele = rep("A/G", n_snp), snpfirstdim = FALSE
  )
  metadata <- data.table::data.table(sample = sample_id, population = populations)
  # window_bp = step_bp = 1000 over a 100-1000bp*2-chromosome span gives ~20
  # non-overlapping windows -- enough to exercise real multi-chunk dispatch
  # (fork_worker_count(20, 3) = 3 workers, each getting a contiguous slice).
  list(path = gds_path, snp_ids = seq_len(n_snp), metadata = metadata)
}

test_that("parallel run_genome_scan_fst (chunked fork, independent GDS connections) matches the sequential result exactly", {
  fx <- genome_scan_fst_parallel_fixture_gds()
  gds <- SNPRelate::snpgdsOpen(fx$path)
  on.exit(SNPRelate::snpgdsClose(gds), add = TRUE)
  ids <- popgenVCF:::get_gds_ids(gds)

  serial <- popgenVCF:::run_genome_scan_fst(
    gds, fx$snp_ids, ids, fx$metadata, window_bp = 1000, step_bp = 1000, min_snps = 2L,
    gds_path = NULL, threads = 1L
  )
  parallel_result <- popgenVCF:::run_genome_scan_fst(
    gds, fx$snp_ids, ids, fx$metadata, window_bp = 1000, step_bp = 1000, min_snps = 2L,
    gds_path = fx$path, threads = 3L
  )

  expect_gt(nrow(serial), 10L)
  # Chunked dispatch (parallel::splitIndices()) preserves window order
  # exactly, unlike diversity's per-population dispatch -- no re-sort needed.
  expect_equal(parallel_result, serial, tolerance = 1e-12)
})

test_that("run_genome_scan_fst falls back to sequential when gds_path is NULL, regardless of threads", {
  fx <- genome_scan_fst_parallel_fixture_gds()
  gds <- SNPRelate::snpgdsOpen(fx$path)
  on.exit(SNPRelate::snpgdsClose(gds), add = TRUE)
  ids <- popgenVCF:::get_gds_ids(gds)

  result <- popgenVCF:::run_genome_scan_fst(
    gds, fx$snp_ids, ids, fx$metadata, window_bp = 1000, step_bp = 1000, min_snps = 2L,
    gds_path = NULL, threads = 8L
  )
  expect_gt(nrow(result), 0L)
})

test_that("a real per-chunk failure surfaces as a clear, non-silent error rather than a corrupted result", {
  fx <- genome_scan_fst_parallel_fixture_gds()
  gds <- SNPRelate::snpgdsOpen(fx$path)
  on.exit(SNPRelate::snpgdsClose(gds), add = TRUE)
  ids <- popgenVCF:::get_gds_ids(gds)

  expect_error(
    popgenVCF:::run_genome_scan_fst(
      gds, fx$snp_ids, ids, fx$metadata, window_bp = 1000, step_bp = 1000, min_snps = 2L,
      gds_path = tempfile(fileext = ".gds"), threads = 3L
    ),
    "Parallel genome-scan FST computation failed"
  )
})

genome_scan_diversity_parallel_fixture <- function() {
  set.seed(41)
  populations <- rep(c("PopA", "PopB", "PopC", "PopD"), each = 10L)
  chromosome <- rep("1", 40L)
  position <- rep(seq(100L, by = 100L, length.out = 10L), 4L)
  data.table::data.table(
    population = populations, chromosome = chromosome, position = position,
    observed_heterozygosity = stats::runif(40, 0.1, 0.5),
    unbiased_expected_heterozygosity = stats::runif(40, 0.1, 0.5),
    polymorphic = sample(c(TRUE, FALSE), 40, replace = TRUE, prob = c(0.7, 0.3))
  )
}

test_that("parallel run_genome_scan_diversity (per-population fork, no GDS involved) matches the sequential result exactly", {
  locus <- genome_scan_diversity_parallel_fixture()
  population_n <- c(PopA = 10L, PopB = 10L, PopC = 10L, PopD = 10L)

  serial <- popgenVCF:::run_genome_scan_diversity(
    locus, window_bp = 1000, step_bp = 1000, min_snps = 1L, population_n = population_n, threads = 1L
  )
  parallel_result <- popgenVCF:::run_genome_scan_diversity(
    locus, window_bp = 1000, step_bp = 1000, min_snps = 1L, population_n = population_n, threads = 3L
  )

  expect_identical(nrow(parallel_result), nrow(serial))
  expect_equal(parallel_result, serial, tolerance = 1e-12)
})

test_that("run_genome_scan_diversity with threads = 1 stays sequential and still covers every population", {
  locus <- genome_scan_diversity_parallel_fixture()
  result <- popgenVCF:::run_genome_scan_diversity(locus, window_bp = 1000, step_bp = 1000, min_snps = 1L, threads = 1L)
  expect_identical(sort(unique(result$population)), c("PopA", "PopB", "PopC", "PopD"))
})

test_that("a real per-population failure surfaces as a clear, non-silent error rather than a corrupted result", {
  locus <- genome_scan_diversity_parallel_fixture()
  # No GDS handle is involved in this function at all, so a nonexistent-path
  # trick doesn't apply -- a population_n entry that is genuinely the wrong
  # type (a real caller-input-validation bug, not a contrived one) makes
  # `2 * population_n[[pop]]` error for every population, proving mclapply's
  # per-worker errors are caught and surfaced here too.
  bad_population_n <- c(PopA = "bad", PopB = "bad", PopC = "bad", PopD = "bad")
  expect_error(
    popgenVCF:::run_genome_scan_diversity(
      locus, window_bp = 1000, step_bp = 1000, min_snps = 1L,
      population_n = bad_population_n, threads = 3L
    ),
    "Parallel genome-scan diversity computation failed"
  )
})
