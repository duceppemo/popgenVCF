test_that("run_bottleneck_analysis bins the folded SFS and recovers the expected 'no bottleneck' mode", {
  # 2 bins spanning [0, 0.5]: bin 1 = (0, 0.25], bin 2 = (0.25, 0.5].
  # Population A: 3 rare loci (bin 1), 1 common locus (bin 2) -- the
  # expected L-shaped equilibrium pattern, mode at the lowest class.
  locus <- data.table::data.table(
    population = "A", snp_id = paste0("s", 1:4),
    polymorphic = TRUE, maf = c(0.1, 0.15, 0.2, 0.4)
  )
  res <- popgenVCF:::run_bottleneck_analysis(locus, n_bins = 2L)

  spec_a <- res$spectrum[population == "A"]
  expect_identical(spec_a$n_loci, c(3L, 1L))
  expect_equal(spec_a$bin_lower, c(0, 0.25))
  expect_equal(spec_a$bin_upper, c(0.25, 0.5))

  summary_a <- res$summary[population == "A"]
  expect_identical(summary_a$n_polymorphic_loci, 4L)
  expect_identical(summary_a$mode_bin, 1L)
  expect_false(summary_a$mode_shifted)
})

test_that("run_bottleneck_analysis flags a mode shift when the highest-frequency class dominates", {
  locus <- data.table::data.table(
    population = "B", snp_id = paste0("s", 1:5),
    polymorphic = TRUE, maf = c(0.1, 0.35, 0.4, 0.45, 0.48)
  )
  res <- popgenVCF:::run_bottleneck_analysis(locus, n_bins = 2L)

  summary_b <- res$summary[population == "B"]
  expect_identical(summary_b$mode_bin, 2L)
  expect_true(summary_b$mode_shifted)
})

test_that("run_bottleneck_analysis excludes monomorphic loci, matching standard SFS convention", {
  locus <- data.table::data.table(
    population = "A", snp_id = c("s1", "s2", "s3"),
    polymorphic = c(TRUE, FALSE, TRUE), maf = c(0.1, 0, 0.2)
  )
  res <- popgenVCF:::run_bottleneck_analysis(locus, n_bins = 2L)
  expect_identical(sum(res$spectrum[population == "A", n_loci]), 2L)
})

test_that("run_bottleneck_analysis reports NA/no shift for a population with no polymorphic loci", {
  locus <- data.table::data.table(
    population = "A", snp_id = "s1", polymorphic = FALSE, maf = 0
  )
  res <- popgenVCF:::run_bottleneck_analysis(locus, n_bins = 10L)
  summary_a <- res$summary[population == "A"]
  expect_identical(summary_a$n_polymorphic_loci, 0L)
  expect_true(is.na(summary_a$mode_bin))
  expect_true(is.na(summary_a$mode_shifted))
  expect_identical(sum(res$spectrum[population == "A", n_loci]), 0L)
  expect_identical(nrow(res$spectrum[population == "A"]), 10L)
})

test_that("run_bottleneck_analysis handles a class-boundary allele frequency correctly (right-closed bins)", {
  # bin_width = 0.05 for n_bins = 10; maf exactly on a boundary belongs to
  # the lower (right-closed) class, matching how the figure/labels present it.
  locus <- data.table::data.table(
    population = "A", snp_id = c("s1", "s2"),
    polymorphic = TRUE, maf = c(0.05, 0.5)
  )
  res <- popgenVCF:::run_bottleneck_analysis(locus, n_bins = 10L)
  spec_a <- res$spectrum[population == "A"]
  expect_identical(spec_a[bin == 1L, n_loci], 1L)
  expect_identical(spec_a[bin == 10L, n_loci], 1L)
})

test_that("plot_bottleneck writes a figure file", {
  locus <- data.table::data.table(
    population = rep(c("A", "B"), each = 4L),
    snp_id = paste0("s", 1:8),
    polymorphic = TRUE,
    maf = c(0.1, 0.15, 0.2, 0.4, 0.1, 0.35, 0.4, 0.45)
  )
  res <- popgenVCF:::run_bottleneck_analysis(locus, n_bins = 4L)
  cfg <- popgenVCF::default_config(); cfg$output$figure_formats <- "png"
  out <- tempfile("bottleneck-plot-"); dirs <- list(figures = file.path(out, "figures"))
  dir.create(dirs$figures, recursive = TRUE)

  popgenVCF:::plot_bottleneck(res, cfg, dirs)
  expect_true(file.exists(file.path(dirs$figures, "48_site_frequency_spectrum.png")))
})

test_that("plot_bottleneck is a no-op when no population has any segregating loci", {
  locus <- data.table::data.table(population = "A", snp_id = "s1", polymorphic = FALSE, maf = 0)
  res <- popgenVCF:::run_bottleneck_analysis(locus, n_bins = 4L)
  cfg <- popgenVCF::default_config(); cfg$output$figure_formats <- "png"
  out <- tempfile("bottleneck-empty-"); dirs <- list(figures = file.path(out, "figures"))
  dir.create(dirs$figures, recursive = TRUE)

  popgenVCF:::plot_bottleneck(res, cfg, dirs)
  expect_false(file.exists(file.path(dirs$figures, "48_site_frequency_spectrum.png")))
})

test_that("validate_bottleneck_result accepts a well-formed result and flags real defects", {
  locus <- data.table::data.table(
    population = "A", snp_id = paste0("s", 1:4),
    polymorphic = TRUE, maf = c(0.1, 0.15, 0.2, 0.4)
  )
  res <- popgenVCF:::run_bottleneck_analysis(locus, n_bins = 2L)
  ok <- popgenVCF:::validate_bottleneck_result(res, NULL, NULL)
  expect_true(ok$valid)

  bad_n_loci <- res
  bad_n_loci$spectrum <- data.table::copy(res$spectrum)
  bad_n_loci$spectrum[1L, n_loci := -1L]
  expect_false(popgenVCF:::validate_bottleneck_result(bad_n_loci, NULL, NULL)$valid)

  bad_bounds <- res
  bad_bounds$spectrum <- data.table::copy(res$spectrum)
  bad_bounds$spectrum[1L, bin_upper := bin_lower - 0.01]
  expect_false(popgenVCF:::validate_bottleneck_result(bad_bounds, NULL, NULL)$valid)

  incomplete <- list(spectrum = data.table::data.table(), summary = data.table::data.table())
  expect_false(popgenVCF:::validate_bottleneck_result(incomplete, NULL, NULL)$valid)
})

test_that("validate_bottleneck_result surfaces a mode-shift warning", {
  locus <- data.table::data.table(
    population = "B", snp_id = paste0("s", 1:5),
    polymorphic = TRUE, maf = c(0.1, 0.35, 0.4, 0.45, 0.48)
  )
  res <- popgenVCF:::run_bottleneck_analysis(locus, n_bins = 2L)
  out <- popgenVCF:::validate_bottleneck_result(res, NULL, NULL)
  expect_true(out$valid)
  expect_match(out$warnings, "mode-shifted")
})

test_that("bottleneck_module_spec is registered, requires diversity, and is enabled by default", {
  registry <- popgenVCF::default_analysis_registry()
  expect_true("bottleneck" %in% names(registry$modules))
  module <- registry$modules$bottleneck

  spec <- popgenVCF::bottleneck_module_spec()
  expect_identical(module$run, spec$run)
  expect_identical(module$requires, spec$requires)
  expect_identical(module$requires, "diversity")
  expect_identical(module$validate, spec$validate)
  expect_identical(module$outputs, spec$outputs)
  expect_identical(module$references, spec$references)
  expect_identical(module$resource_class, spec$resource_class)
  expect_identical(module$contract_version, spec$contract_version)

  cfg <- popgenVCF::default_config()
  expect_true(spec$enabled(cfg))
  cfg$analyses$bottleneck <- FALSE
  expect_false(spec$enabled(cfg))
})
