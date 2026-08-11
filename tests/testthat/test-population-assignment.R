test_that("genotype_log_likelihood implements the standard biallelic HWE genotype-likelihood formula", {
  gll <- popgenVCF:::genotype_log_likelihood
  freq <- 0.3
  expect_equal(gll(0, freq), 2 * log(1 - freq))
  expect_equal(gll(1, freq), log(2) + log(freq) + log(1 - freq))
  expect_equal(gll(2, freq), 2 * log(freq))
  expect_true(is.na(gll(NA_integer_, freq)))
  expect_true(is.na(gll(1, NA_real_)))
})

test_that("run_population_assignment matches an exact hand calculation, including leave-one-out and the mismatch flag", {
  # Population A (S1-S4): typical dosage 0, except S4 (deliberately dosage
  # 2 -- a B-like genotype mislabeled as A). Population B (S5-S8): typical
  # dosage 2, except S8 (deliberately dosage 0 -- an A-like genotype
  # mislabeled as B). One locus, so the log-likelihood is a single
  # genotype-likelihood term and every value below is hand-computable.
  locus_table <- data.table::data.table(
    population = c("A", "B"),
    snp_id = c("s1", "s1"),
    n_called = c(4L, 4L),
    alternate_allele_count = c(2L, 6L)
  )
  sample_table <- data.table::data.table(
    sample = c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8"),
    population = c("A", "A", "A", "A", "B", "B", "B", "B")
  )
  genotype <- matrix(c(0, 0, 0, 2, 2, 2, 2, 0), ncol = 1L)

  res <- popgenVCF:::run_population_assignment(genotype, sample_table, locus_table, "s1")
  a <- data.table::copy(res$assignment)
  data.table::setkey(a, sample)

  expect_identical(res$populations, c("A", "B"))
  expect_identical(nrow(a), 8L)

  # S1-S3 (dosage 0): leave-one-out within A gives freq = 2/6 = 1/3;
  # against B (freq = 6/8, no leave-one-out) they fit far worse.
  for (s in c("S1", "S2", "S3")) {
    expect_identical(a[s, assigned_population], "A")
    expect_false(a[s, mismatch])
    expect_equal(a[s, log_likelihood], 2 * log(1 - 1 / 3), tolerance = 1e-8)
  }

  # S4 (dosage 2, recorded A): leave-one-out within A gives alt_count = 0,
  # zero-frequency corrected to the 1/(2n) floor (n = 3 -> 1/6); against B
  # (freq = 6/8, no leave-one-out) it fits far better -- a real mismatch.
  expect_identical(a["S4", assigned_population], "B")
  expect_true(a["S4", mismatch])
  expect_equal(a["S4", log_likelihood], 2 * log(6 / 8), tolerance = 1e-8)

  # S5-S7 (dosage 2): leave-one-out within B gives freq = 4/6 = 2/3;
  # against A (freq = 2/8, no leave-one-out) they fit far worse.
  for (s in c("S5", "S6", "S7")) {
    expect_identical(a[s, assigned_population], "B")
    expect_false(a[s, mismatch])
    expect_equal(a[s, log_likelihood], 2 * log(2 / 3), tolerance = 1e-8)
  }

  # S8 (dosage 0, recorded B): leave-one-out within B gives alt_count = 6
  # of 6 possible copies, a fixed-frequency correction to 1 - 1/6 = 5/6;
  # against A (freq = 2/8, no leave-one-out) it fits far better.
  expect_identical(a["S8", assigned_population], "A")
  expect_true(a["S8", mismatch])
  expect_equal(a["S8", log_likelihood], 2 * log(1 - 2 / 8), tolerance = 1e-8)

  expect_true(all(a$n_loci_used == 1L))
  expect_true(all(a$posterior_probability >= 0 & a$posterior_probability <= 1, na.rm = TRUE))
  expect_true(all(a$likelihood_ratio > 1, na.rm = TRUE))
})

test_that("run_population_assignment excludes loci with zero calls in a candidate population, like Nei's distance does", {
  locus_table <- data.table::data.table(
    population = c("A", "A", "B", "B"),
    snp_id = c("s1", "s2", "s1", "s2"),
    n_called = c(4L, 0L, 4L, 4L),
    alternate_allele_count = c(2L, 0L, 6L, 2L)
  )
  sample_table <- data.table::data.table(sample = "S1", population = "A")
  genotype <- matrix(c(0, 1), nrow = 1L)

  res <- popgenVCF:::run_population_assignment(genotype, sample_table, locus_table, c("s1", "s2"))
  # s2 contributes nothing to population A's score (zero calls there), so
  # only s1 is used even though two loci were requested.
  expect_identical(res$assignment$n_loci_used, 1L)
})

test_that("run_population_assignment returns an empty result with fewer than two populations", {
  locus_table <- data.table::data.table(
    population = "A", snp_id = "s1", n_called = 4L, alternate_allele_count = 2L
  )
  sample_table <- data.table::data.table(sample = "S1", population = "A")
  genotype <- matrix(0, nrow = 1L, ncol = 1L)

  res <- popgenVCF:::run_population_assignment(genotype, sample_table, locus_table, "s1")
  expect_identical(nrow(res$assignment), 0L)
  expect_identical(res$populations, "A")
})

test_that("run_population_assignment recovers realistic self-assignment on a larger synthetic fixture", {
  set.seed(11)
  n_per_pop <- 25L; n_snp <- 80L
  pops <- rep(c("A", "B", "C"), each = n_per_pop)
  base_freqs <- c(A = 0.15, B = 0.5, C = 0.85)
  sample_id <- paste0("S", seq_along(pops))
  genotype <- matrix(NA_integer_, nrow = length(pops), ncol = n_snp)
  for (j in seq_len(n_snp)) {
    for (i in seq_along(pops)) genotype[i, j] <- stats::rbinom(1, 2, base_freqs[[pops[i]]])
  }
  snp_ids <- paste0("snp", seq_len(n_snp))
  locus_table <- data.table::rbindlist(lapply(c("A", "B", "C"), function(p) {
    idx <- which(pops == p)
    data.table::data.table(
      population = p, snp_id = snp_ids,
      n_called = colSums(!is.na(genotype[idx, , drop = FALSE])),
      alternate_allele_count = colSums(genotype[idx, , drop = FALSE], na.rm = TRUE)
    )
  }))
  sample_table <- data.table::data.table(sample = sample_id, population = pops)

  res <- popgenVCF:::run_population_assignment(genotype, sample_table, locus_table, snp_ids)
  match_rate <- mean(!res$assignment$mismatch)
  expect_gt(match_rate, 0.9)
})

test_that("plot_population_assignment writes a figure file", {
  a <- data.table::data.table(
    sample = c("S1", "S2", "S3", "S4"),
    recorded_population = c("A", "A", "B", "B"),
    assigned_population = c("A", "B", "B", "B"),
    mismatch = c(FALSE, TRUE, FALSE, FALSE),
    log_likelihood = c(-1, -2, -1.5, -1.1),
    likelihood_ratio = c(5, 2, 8, 6),
    posterior_probability = c(0.9, 0.6, 0.85, 0.8),
    n_loci_used = c(50L, 50L, 50L, 50L)
  )
  result <- list(assignment = a, populations = c("A", "B"))
  cfg <- popgenVCF::default_config(); cfg$output$figure_formats <- "png"
  out <- tempfile("assignment-plot-"); dirs <- list(figures = file.path(out, "figures"))
  dir.create(dirs$figures, recursive = TRUE)

  popgenVCF:::plot_population_assignment(result, cfg, dirs)
  expect_true(file.exists(file.path(dirs$figures, "47_population_assignment.png")))
})

test_that("validate_population_assignment_result accepts a well-formed result and flags real defects", {
  a <- data.table::data.table(
    sample = c("S1", "S2"), recorded_population = c("A", "B"),
    assigned_population = c("A", "A"), mismatch = c(FALSE, TRUE),
    log_likelihood = c(-1, -2), likelihood_ratio = c(3, 1.5),
    posterior_probability = c(0.9, 0.55), n_loci_used = c(40L, 40L)
  )
  ok <- popgenVCF:::validate_population_assignment_result(
    list(assignment = a, populations = c("A", "B")), NULL, NULL
  )
  expect_true(ok$valid)
  expect_match(ok$warnings, "1 sample")

  bad_posterior <- data.table::copy(a); bad_posterior[1L, posterior_probability := 1.5]
  expect_false(popgenVCF:::validate_population_assignment_result(
    list(assignment = bad_posterior, populations = c("A", "B")), NULL, NULL
  )$valid)

  bad_n_loci <- data.table::copy(a); bad_n_loci[1L, n_loci_used := -1L]
  expect_false(popgenVCF:::validate_population_assignment_result(
    list(assignment = bad_n_loci, populations = c("A", "B")), NULL, NULL
  )$valid)

  incomplete <- list(assignment = data.table::data.table(sample = "S1"), populations = "A")
  expect_false(popgenVCF:::validate_population_assignment_result(incomplete, NULL, NULL)$valid)
})

test_that("population_assignment_module_spec is registered and enabled by default", {
  registry <- popgenVCF::default_analysis_registry()
  expect_true("population_assignment" %in% names(registry$modules))
  module <- registry$modules$population_assignment

  spec <- popgenVCF::population_assignment_module_spec()
  expect_identical(module$run, spec$run)
  expect_identical(module$validate, spec$validate)
  expect_identical(module$outputs, spec$outputs)
  expect_identical(module$references, spec$references)
  expect_identical(module$resource_class, spec$resource_class)
  expect_identical(module$contract_version, spec$contract_version)

  cfg <- popgenVCF::default_config()
  expect_true(spec$enabled(cfg))
  cfg$analyses$population_assignment <- FALSE
  expect_false(spec$enabled(cfg))
})
