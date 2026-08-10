# A real Wright-Fisher forward simulation with a known true Ne (not just a
# formula self-consistency check): N diploid individuals, L unlinked loci
# (one per "chromosome"), random mating with independent parental-allele
# inheritance per locus (recombination = 0.5, matching "unlinked") for
# several generations. This is the only way to generate genotypes with real
# drift-induced multilocus LD -- sampling each locus's genotypes
# independently from its own drifted allele frequency (tried first while
# developing this) produces zero real cross-locus correlation and silently
# validates nothing.
simulate_wright_fisher_ne <- function(n, n_loci, generations, seed) {
  set.seed(seed)
  haplotypes <- array(rbinom(2 * n * n_loci, 1, 0.5), dim = c(2, n, n_loci))
  for (g in seq_len(generations)) {
    parent1 <- sample.int(n, n, replace = TRUE)
    parent2 <- sample.int(n, n, replace = TRUE)
    which1 <- matrix(sample.int(2, n * n_loci, replace = TRUE), nrow = n, ncol = n_loci)
    which2 <- matrix(sample.int(2, n * n_loci, replace = TRUE), nrow = n, ncol = n_loci)
    new_hap <- array(NA_integer_, dim = c(2, n, n_loci))
    for (l in seq_len(n_loci)) {
      new_hap[1, , l] <- haplotypes[cbind(which1[, l], parent1, l)]
      new_hap[2, , l] <- haplotypes[cbind(which2[, l], parent2, l)]
    }
    haplotypes <- new_hap
  }
  genmat <- matrix(NA_integer_, n, n_loci)
  for (l in seq_len(n_loci)) genmat[, l] <- haplotypes[1, , l] + haplotypes[2, , l]

  sample_id <- paste0("S", seq_len(n))
  snp_id <- seq_len(n_loci)
  gds_path <- tempfile(fileext = ".gds")
  SNPRelate::snpgdsCreateGeno(
    gds_path, genmat = genmat, sample.id = sample_id, snp.id = snp_id,
    snp.chromosome = snp_id, snp.position = rep(1000L, n_loci),
    snp.allele = rep("A/G", n_loci), snpfirstdim = FALSE
  )
  gds <- SNPRelate::snpgdsOpen(gds_path)
  ids <- popgenVCF:::get_gds_ids(gds)
  list(gds = gds, ids = ids, sample_id = sample_id, snp_id = snp_id)
}

test_that("compute_ne_ld recovers a known true Ne from simulated drift, within a generous band", {
  fx <- simulate_wright_fisher_ne(n = 30L, n_loci = 80L, generations = 6L, seed = 5L)
  on.exit(SNPRelate::snpgdsClose(fx$gds), add = TRUE)
  metadata <- data.table::data.table(sample = fx$sample_id, population = "SIM")
  res <- popgenVCF:::compute_ne_ld(fx$gds, fx$sample_id, fx$snp_id, fx$ids, metadata, max_snps = 2000L, seed = 42L)

  expect_identical(nrow(res), 1L)
  expect_identical(res$ne_status, "ok")
  expect_true(is.finite(res$ne))
  # True N = 30; LD-based Ne from a single realization has real sampling
  # variance, so this checks order-of-magnitude recovery, not precision.
  expect_gt(res$ne, 30 / 4)
  expect_lt(res$ne, 30 * 4)
  expect_identical(res$n_chromosomes, 80L)
  expect_gt(res$n_pairs, 0L)
})

test_that("ne_ld_bias_correction matches the documented Waples (2006) piecewise formula", {
  expect_equal(popgenVCF:::ne_ld_bias_correction(20), 0.0018 + 0.907 / 20 + 4.44 / 20^2)
  expect_equal(popgenVCF:::ne_ld_bias_correction(50), 1 / 50)
  expect_equal(popgenVCF:::ne_ld_bias_correction(30), 0.0018 + 0.907 / 30 + 4.44 / 30^2)
})

test_that("ne_ld_from_r2_drift handles the non-positive and out-of-domain cases", {
  ok <- popgenVCF:::ne_ld_from_r2_drift(0.01)
  expect_identical(ok$status, "ok")
  expect_equal(ok$ne, (1 / 3 + sqrt(1 / 9 - 2.76 * 0.01)) / (2 * 0.01))

  zero_or_negative <- popgenVCF:::ne_ld_from_r2_drift(-0.001)
  expect_identical(zero_or_negative$ne, Inf)
  expect_identical(zero_or_negative$status, "ok")

  out_of_domain <- popgenVCF:::ne_ld_from_r2_drift(0.1)
  expect_true(is.na(out_of_domain$ne))
  expect_identical(out_of_domain$status, "below_formula_domain")
})

test_that("ne_ld_one_population reports insufficient-data statuses transparently", {
  fx <- simulate_wright_fisher_ne(n = 10L, n_loci = 5L, generations = 2L, seed = 9L)
  on.exit(SNPRelate::snpgdsClose(fx$gds), add = TRUE)

  too_few_samples <- popgenVCF:::ne_ld_one_population(
    fx$gds, fx$sample_id[1L], fx$snp_id, fx$ids, max_snps = 2000L, seed = 42L
  )
  expect_identical(too_few_samples$ne_status, "fewer_than_two_samples")
  expect_true(is.na(too_few_samples$ne))

  single_chromosome_fx <- simulate_wright_fisher_ne(n = 10L, n_loci = 5L, generations = 2L, seed = 9L)
  on.exit(SNPRelate::snpgdsClose(single_chromosome_fx$gds), add = TRUE)
  one_chr_ids <- single_chromosome_fx$ids
  one_chr_ids$chromosome[] <- 1L
  single_chr <- popgenVCF:::ne_ld_one_population(
    single_chromosome_fx$gds, single_chromosome_fx$sample_id, single_chromosome_fx$snp_id,
    one_chr_ids, max_snps = 2000L, seed = 42L
  )
  expect_identical(single_chr$ne_status, "fewer_than_two_chromosomes")
  expect_true(is.na(single_chr$ne))
})

test_that("compute_ne_ld returns one row per population, ordered by population", {
  fx <- simulate_wright_fisher_ne(n = 20L, n_loci = 20L, generations = 3L, seed = 11L)
  on.exit(SNPRelate::snpgdsClose(fx$gds), add = TRUE)
  metadata <- data.table::data.table(
    sample = fx$sample_id, population = rep(c("B", "A"), each = 10L)
  )
  res <- popgenVCF:::compute_ne_ld(fx$gds, fx$sample_id, fx$snp_id, fx$ids, metadata, max_snps = 2000L, seed = 42L)
  expect_identical(res$population, c("A", "B"))
  expect_identical(res$n_samples, c(10L, 10L))
})

test_that("validate_ne_ld_result accepts a well-formed result and flags negative Ne/n_pairs", {
  fx <- simulate_wright_fisher_ne(n = 20L, n_loci = 20L, generations = 3L, seed = 11L)
  on.exit(SNPRelate::snpgdsClose(fx$gds), add = TRUE)
  metadata <- data.table::data.table(sample = fx$sample_id, population = "SIM")
  res <- popgenVCF:::compute_ne_ld(fx$gds, fx$sample_id, fx$snp_id, fx$ids, metadata, max_snps = 2000L, seed = 42L)
  ok <- popgenVCF:::validate_ne_ld_result(res, NULL, NULL)
  expect_true(ok$valid)

  bad_ne <- data.table::copy(res)
  bad_ne[1L, ne := -5]
  expect_false(popgenVCF:::validate_ne_ld_result(bad_ne, NULL, NULL)$valid)

  bad_pairs <- data.table::copy(res)
  bad_pairs[1L, n_pairs := -1L]
  expect_false(popgenVCF:::validate_ne_ld_result(bad_pairs, NULL, NULL)$valid)

  incomplete <- data.frame(population = "SIM")
  expect_false(popgenVCF:::validate_ne_ld_result(incomplete, NULL, NULL)$valid)
})

test_that("ne_ld_module_spec is registered and enabled by default", {
  registry <- popgenVCF::default_analysis_registry()
  expect_true("ne_ld" %in% names(registry$modules))
  module <- registry$modules$ne_ld
  cfg <- popgenVCF::default_config()
  expect_true(popgenVCF:::module_is_enabled(module, cfg))
  cfg$analyses$ne_ld <- FALSE
  expect_false(popgenVCF:::module_is_enabled(module, cfg))
})

test_that("plot_ne_ld writes a figure only when at least one finite Ne exists", {
  # Same seed/parameters as the recovery test above -- already confirmed to
  # yield a finite "ok" estimate, unlike an arbitrary small fixture where
  # r2_drift can legitimately land at or below the sampling-noise floor
  # (reported as Ne = Inf, a correct result, not a bug) by chance.
  fx <- simulate_wright_fisher_ne(n = 30L, n_loci = 80L, generations = 6L, seed = 5L)
  on.exit(SNPRelate::snpgdsClose(fx$gds), add = TRUE)
  metadata <- data.table::data.table(sample = fx$sample_id, population = "SIM")
  res <- popgenVCF:::compute_ne_ld(fx$gds, fx$sample_id, fx$snp_id, fx$ids, metadata, max_snps = 2000L, seed = 42L)

  out <- tempfile("ne-ld-plot-")
  dirs <- list(figures = file.path(out, "figures"))
  dir.create(dirs$figures, recursive = TRUE)
  cfg <- popgenVCF::default_config()
  cfg$output$figure_formats <- "png"
  popgenVCF:::plot_ne_ld(res, cfg, dirs)
  expect_true(file.exists(file.path(dirs$figures, "45_Ne_LD.png")))

  empty <- data.table::data.table(
    population = "SIM", n_samples = 1L, n_snps = 0L, n_chromosomes = 0L, n_pairs = 0L,
    harmonic_mean_n = NA_real_, mean_r2 = NA_real_, mean_r2_drift = NA_real_,
    ne = NA_real_, ne_status = "fewer_than_two_samples"
  )
  out2 <- tempfile("ne-ld-plot-empty-")
  dirs2 <- list(figures = file.path(out2, "figures"))
  dir.create(dirs2$figures, recursive = TRUE)
  popgenVCF:::plot_ne_ld(empty, cfg, dirs2)
  expect_false(file.exists(file.path(dirs2$figures, "45_Ne_LD.png")))
})
