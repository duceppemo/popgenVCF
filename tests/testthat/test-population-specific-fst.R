test_that("compute_population_specific_fst matches hierfstat::betas() exactly (it is the reference implementation, called directly)", {
  skip_if_not_installed("hierfstat")
  set.seed(1)
  n_pop <- 3L; n_per_pop <- 20L; n_snp <- 30L
  pops <- rep(c("A", "B", "C"), each = n_per_pop)
  base_freqs <- c(A = 0.2, B = 0.5, C = 0.8)
  geno <- matrix(NA_integer_, nrow = length(pops), ncol = n_snp)
  for (j in seq_len(n_snp)) for (i in seq_along(pops)) geno[i, j] <- rbinom(1, 2, base_freqs[[pops[i]]])

  res <- popgenVCF:::compute_population_specific_fst(geno, pops)
  expect_true(res$available)
  expect_setequal(res$table$population, c("A", "B", "C"))
  data.table::setorder(res$table, population)
  expect_equal(res$table$beta, c(0.399725891715711, 0.109800714831408, 0.39174066438627), tolerance = 1e-9)
  expect_equal(res$overall, 0.300422423644463, tolerance = 1e-9)

  # A real, biologically sensible property: population B (allele frequency
  # 0.5, closest to the 3-population mean of 0.5) contributes the least to
  # overall differentiation; A and C (the extreme frequencies) contribute
  # the most -- confirmed directly from the pinned values above, not
  # asserted independently of them.
  expect_lt(res$table[population == "B", beta], res$table[population == "A", beta])
  expect_lt(res$table[population == "B", beta], res$table[population == "C", beta])

  # The (here, equal) sample-size-weighted mean of the per-population beta
  # values equals the overall population FST, by construction of the method.
  expect_equal(mean(res$table$beta), res$overall, tolerance = 1e-9)
})

test_that("compute_population_specific_fst returns an unavailable, empty result with fewer than two populations", {
  geno <- matrix(rbinom(40, 2, 0.3), nrow = 4L)
  res <- popgenVCF:::compute_population_specific_fst(geno, rep("A", 4L))
  expect_false(res$available)
  expect_identical(nrow(res$table), 0L)
  expect_true(is.na(res$overall))
})

test_that("a non-finite beta from hierfstat::betas() is surfaced with a warning, not silently propagated", {
  # hierfstat::betas() is not documented to guarantee a finite beta for
  # every population (a small or degenerate population, e.g. n = 1
  # sample, is a plausible source) -- previously any non-finite value
  # flowed silently into the published table/figure with nothing
  # indicating it was unusual. hierfstat::betas() is mocked to return a
  # NaN beta directly (engineering a real genotype fixture that reliably
  # reproduces hierfstat's own internal NaN case is not practical) so
  # this test exercises the actual guard, not a hand reimplementation.
  local_mocked_bindings(
    betas = function(dat, nboot = 0, ...) {
      list(betaiovl = c(A = NaN, B = 0.1), betaW = 0.1)
    },
    .package = "hierfstat"
  )
  geno <- matrix(rbinom(80, 2, 0.3), nrow = 8L)
  expect_output(
    res <- popgenVCF:::compute_population_specific_fst(geno, rep(c("A", "B"), each = 4L)),
    "non-finite.*A"
  )
  expect_true(res$available)
  expect_true(is.nan(res$table[population == "A", beta]))
  expect_equal(res$table[population == "B", beta], 0.1)
})

test_that("compute_population_specific_fst skips transparently when hierfstat is unavailable", {
  local_mocked_bindings(requireNamespace = function(...) FALSE, .package = "base")
  geno <- matrix(rbinom(80, 2, 0.3), nrow = 8L)
  res <- popgenVCF:::compute_population_specific_fst(geno, rep(c("A", "B"), each = 4L))
  expect_false(res$available)
  expect_identical(nrow(res$table), 0L)
  expect_true(is.na(res$overall))
})

test_that("plot_population_specific_fst writes a figure file, and is a no-op when unavailable", {
  skip_if_not_installed("hierfstat")
  cfg <- popgenVCF::default_config(); cfg$output$figure_formats <- "png"
  out <- tempfile("beta-fst-plot-"); dirs <- list(figures = file.path(out, "figures"))
  dir.create(dirs$figures, recursive = TRUE)
  result <- list(
    available = TRUE, overall = 0.2,
    table = data.table::data.table(population = c("A", "B", "C"), beta = c(0.4, 0.1, 0.35))
  )
  popgenVCF:::plot_population_specific_fst(result, cfg, dirs)
  expect_true(file.exists(file.path(dirs$figures, "51_population_specific_fst.png")))

  out2 <- tempfile("beta-fst-plot-unavail-"); dirs2 <- list(figures = file.path(out2, "figures"))
  dir.create(dirs2$figures, recursive = TRUE)
  popgenVCF:::plot_population_specific_fst(list(available = FALSE, table = data.table::data.table(), overall = NA_real_), cfg, dirs2)
  expect_false(file.exists(file.path(dirs2$figures, "51_population_specific_fst.png")))
})
