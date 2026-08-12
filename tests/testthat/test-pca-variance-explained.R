pca_variance_fixture <- function(n_samples = 20L, n_snps = 300L, seed = 1L) {
  set.seed(seed)
  sample_id <- paste0("s", seq_len(n_samples))
  snp_id <- seq_len(n_snps)
  genmat <- matrix(
    sample(0:2, n_samples * n_snps, replace = TRUE, prob = c(0.25, 0.5, 0.25)),
    nrow = n_samples, ncol = n_snps
  )
  gds_path <- tempfile(fileext = ".gds")
  SNPRelate::snpgdsCreateGeno(
    gds_path, genmat = genmat, sample.id = sample_id, snp.id = snp_id,
    snp.chromosome = rep(1L, n_snps), snp.position = seq_len(n_snps),
    snp.allele = rep("A/G", n_snps), snpfirstdim = FALSE
  )
  gds <- SNPRelate::snpgdsOpen(gds_path)
  ids <- popgenVCF:::get_gds_ids(gds)
  metadata <- popgenVCF:::normalize_sample_aliases(data.table::data.table(
    sample = sample_id, population = rep("PopA", n_samples)
  ))
  list(gds = gds, sample_id = sample_id, snp_id = snp_id, ids = ids, metadata = metadata)
}

test_that("run_pca()'s reported percent variance matches SNPRelate's own varprop, not a truncated-sum re-normalization", {
  fx <- pca_variance_fixture()
  on.exit(SNPRelate::snpgdsClose(fx$gds), add = TRUE)

  n_pcs <- 5L
  result <- popgenVCF:::run_pca(
    fx$gds, fx$sample_id, fx$snp_id, fx$metadata, n_pcs, 1L, ids = fx$ids
  )

  # Independent ground truth: SNPRelate's own eigen.cnt-scoped varprop from a
  # direct call with the same requested component count.
  reference <- SNPRelate::snpgdsPCA(
    fx$gds, sample.id = fx$sample_id, snp.id = fx$snp_id,
    autosome.only = FALSE, remove.monosnp = TRUE, maf = NaN, missing.rate = NaN,
    eigen.cnt = n_pcs, num.thread = 1L, verbose = FALSE
  )
  expected_percent <- 100 * reference$varprop[seq_len(n_pcs)]

  expect_equal(result$variance$percent, expected_percent, tolerance = 1e-8)
  expect_equal(sum(result$variance$proportion), sum(reference$varprop[seq_len(n_pcs)]), tolerance = 1e-8)

  # The old, buggy formula (eigenvalues normalized against only the retained
  # subset's own sum) inflates every percentage well above the true value --
  # assert the fix is not accidentally still computing that.
  buggy_percent <- 100 * result$eigenvalues[seq_len(n_pcs)] / sum(result$eigenvalues[seq_len(n_pcs)])
  expect_true(all(result$variance$percent < buggy_percent - 1e-6))

  # Percentages must be real, bounded proportions of total variance, not an
  # artifact that happens to always sum close to 100% regardless of how many
  # components were requested away from the full spectrum.
  expect_true(all(result$variance$percent > 0))
  expect_true(sum(result$variance$percent) < 100)
})

test_that("run_pca()'s percent variance is materially different when requesting fewer components", {
  fx <- pca_variance_fixture(n_samples = 20L, n_snps = 300L, seed = 2L)
  on.exit(SNPRelate::snpgdsClose(fx$gds), add = TRUE)

  few <- popgenVCF:::run_pca(fx$gds, fx$sample_id, fx$snp_id, fx$metadata, 2L, 1L)
  many <- popgenVCF:::run_pca(fx$gds, fx$sample_id, fx$snp_id, fx$metadata, 10L, 1L)

  # A correct PC1 percentage does not depend on how many total components
  # were requested (SNPRelate computes varprop against the true total
  # regardless of eigen.cnt) -- the pre-fix formula would make PC1's
  # reported percentage shrink as more components are requested, since the
  # truncated-sum denominator grows with n_pcs.
  expect_equal(few$variance$percent[1], many$variance$percent[1], tolerance = 1e-6)
})
