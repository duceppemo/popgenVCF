ld_decay_block_fixture <- function() {
  # Three haplotype blocks, each 10 SNPs sharing one dosage value per sample
  # (perfect within-block LD, r = 1), separated by large physical gaps and
  # drawn independently across samples (near-zero cross-block LD) -- a
  # deterministic near/far contrast, not a claim about realistic decay shape.
  set.seed(11)
  n_samp <- 100L
  n_blocks <- 3L; block_size <- 10L
  block_dosage <- matrix(sample(0:2, n_samp * n_blocks, replace = TRUE), nrow = n_samp, ncol = n_blocks)
  genmat <- matrix(NA_integer_, nrow = n_samp, ncol = n_blocks * block_size)
  snp_position <- integer(n_blocks * block_size)
  col <- 1L
  for (b in seq_len(n_blocks)) {
    block_start <- (b - 1L) * 200000L + 1000L
    for (s in seq_len(block_size)) {
      genmat[, col] <- block_dosage[, b]
      snp_position[col] <- block_start + (s - 1L) * 1000L
      col <- col + 1L
    }
  }
  sample_id <- paste0("S", seq_len(n_samp))
  snp_id <- seq_len(n_blocks * block_size)
  gds_path <- tempfile(fileext = ".gds")
  SNPRelate::snpgdsCreateGeno(
    gds_path, genmat = genmat, sample.id = sample_id, snp.id = snp_id,
    snp.chromosome = rep(1L, length(snp_id)), snp.position = snp_position,
    snp.allele = rep("A/G", length(snp_id)), snpfirstdim = FALSE
  )
  gds <- SNPRelate::snpgdsOpen(gds_path)
  ids <- popgenVCF:::get_gds_ids(gds)
  list(gds = gds, ids = ids, sample_id = sample_id, snp_id = snp_id)
}

test_that("compute_ld_decay shows near-perfect LD within a block and near-zero LD across blocks", {
  fx <- ld_decay_block_fixture()
  on.exit(SNPRelate::snpgdsClose(fx$gds), add = TRUE)
  res <- popgenVCF:::compute_ld_decay(
    fx$gds, fx$sample_id, fx$snp_id, fx$ids,
    max_distance_bp = 500000L, bin_bp = 1000L, slide = 29L
  )
  expect_identical(res$n_snps, 30L)
  expect_gt(res$n_pairs, 0L)

  near <- res$binned[distance_bin_start < 10000L]
  far <- res$binned[distance_bin_start >= 190000L]
  expect_true(nrow(near) > 0L)
  expect_true(nrow(far) > 0L)
  expect_true(all(abs(near$mean_r2 - 1) < 1e-6))
  expect_true(all(far$mean_r2 < 0.05))
})

test_that("compute_ld_decay returns an empty result for fewer than two SNPs", {
  fx <- ld_decay_block_fixture()
  on.exit(SNPRelate::snpgdsClose(fx$gds), add = TRUE)
  res <- popgenVCF:::compute_ld_decay(
    fx$gds, fx$sample_id, fx$snp_id[1L], fx$ids,
    max_distance_bp = 500000L, bin_bp = 1000L, slide = 29L
  )
  expect_identical(res$n_snps, 1L)
  expect_identical(res$n_pairs, 0L)
  expect_identical(nrow(res$binned), 0L)
})

test_that("compute_ld_decay's max_distance_bp excludes pairs beyond the cutoff", {
  fx <- ld_decay_block_fixture()
  on.exit(SNPRelate::snpgdsClose(fx$gds), add = TRUE)
  res <- popgenVCF:::compute_ld_decay(
    fx$gds, fx$sample_id, fx$snp_id, fx$ids,
    max_distance_bp = 15000L, bin_bp = 1000L, slide = 29L
  )
  expect_true(all(res$binned$distance_bin_start < 15000L))
})

test_that("validate_ld_decay_result accepts a well-formed result and flags a malformed one", {
  fx <- ld_decay_block_fixture()
  on.exit(SNPRelate::snpgdsClose(fx$gds), add = TRUE)
  res <- popgenVCF:::compute_ld_decay(
    fx$gds, fx$sample_id, fx$snp_id, fx$ids,
    max_distance_bp = 500000L, bin_bp = 1000L, slide = 29L
  )
  v <- popgenVCF:::validate_ld_decay_result(res, NULL, NULL)
  expect_true(v$valid)
  expect_identical(v$metrics$n_pairs, res$n_pairs)

  bad <- list(binned = list(not = "tabular"), n_snps = 30L, n_pairs = 10L)
  v_bad <- popgenVCF:::validate_ld_decay_result(bad, NULL, NULL)
  expect_false(v_bad$valid)

  incomplete <- list(binned = res$binned)
  v_incomplete <- popgenVCF:::validate_ld_decay_result(incomplete, NULL, NULL)
  expect_false(v_incomplete$valid)
})

test_that("ld_decay_module_spec is registered and enabled by default", {
  registry <- popgenVCF::default_analysis_registry()
  expect_true("ld_decay" %in% names(registry$modules))
  module <- registry$modules$ld_decay
  cfg <- popgenVCF::default_config()
  expect_true(popgenVCF:::module_is_enabled(module, cfg))
  cfg$analyses$ld_decay <- FALSE
  expect_false(popgenVCF:::module_is_enabled(module, cfg))
})

test_that("plot_ld_decay writes a figure only when the binned table is non-empty", {
  fx <- ld_decay_block_fixture()
  on.exit(SNPRelate::snpgdsClose(fx$gds), add = TRUE)
  res <- popgenVCF:::compute_ld_decay(
    fx$gds, fx$sample_id, fx$snp_id, fx$ids,
    max_distance_bp = 500000L, bin_bp = 1000L, slide = 29L
  )
  out <- tempfile("ld-decay-plot-")
  dirs <- list(figures = file.path(out, "figures"))
  dir.create(dirs$figures, recursive = TRUE)
  cfg <- popgenVCF::default_config()
  cfg$output$figure_formats <- "png"
  popgenVCF:::plot_ld_decay(res, cfg, dirs)
  expect_true(file.exists(file.path(dirs$figures, "43_LD_decay.png")))

  empty <- list(binned = data.table::data.table(
    distance_bin_start = integer(), distance_bin_end = integer(),
    n_pairs = integer(), mean_r2 = numeric()
  ), n_snps = 0L, n_pairs = 0L)
  out2 <- tempfile("ld-decay-plot-empty-")
  dirs2 <- list(figures = file.path(out2, "figures"))
  dir.create(dirs2$figures, recursive = TRUE)
  popgenVCF:::plot_ld_decay(empty, cfg, dirs2)
  expect_false(file.exists(file.path(dirs2$figures, "43_LD_decay.png")))
})
