test_that("resolve_pcadapt_k applies the population-count heuristic and bounds", {
  expect_identical(popgenVCF:::resolve_pcadapt_k(NULL, 5L, 100L, 500L), 4L) # n_pop - 1
  expect_identical(popgenVCF:::resolve_pcadapt_k(NULL, NA_integer_, 100L, 500L), 2L) # pcadapt's own default
  expect_identical(popgenVCF:::resolve_pcadapt_k(3L, 5L, 100L, 500L), 3L) # explicit config wins
  expect_identical(popgenVCF:::resolve_pcadapt_k(NULL, 20L, 5L, 500L), 4L) # bounded by n_samples - 1
  expect_identical(popgenVCF:::resolve_pcadapt_k(NULL, 20L, 500L, 5L), 4L) # bounded by n_snps - 1
  expect_identical(popgenVCF:::resolve_pcadapt_k(NULL, 50L, 500L, 500L), 10L) # capped at 10
  expect_identical(popgenVCF:::resolve_pcadapt_k(0L, 5L, 100L, 500L), 1L) # floor of 1
})

pcadapt_structured_fixture <- function(seed = 42L, n = 60L, l = 300L, outlier_idx = c(50L, 150L, 250L)) {
  set.seed(seed)
  group <- rep(c(1, 2), each = n / 2)
  geno <- matrix(NA_integer_, n, l)
  for (j in seq_len(l)) {
    p1 <- runif(1, 0.3, 0.7)
    p2 <- min(max(p1 + rnorm(1, 0, 0.03), 0.05), 0.95)
    geno[group == 1, j] <- rbinom(n / 2, 2, p1)
    geno[group == 2, j] <- rbinom(n / 2, 2, p2)
  }
  for (j in outlier_idx) {
    geno[group == 1, j] <- rbinom(n / 2, 2, 0.02)
    geno[group == 2, j] <- rbinom(n / 2, 2, 0.98)
  }
  list(
    genotype = geno, snp_ids = paste0("snp", seq_len(l)),
    chromosome = rep("22", l), position = seq_len(l) * 1000L,
    outlier_idx = outlier_idx
  )
}

test_that("run_pcadapt_scan recovers real, deliberately-injected outlier loci with no false positives", {
  skip_if_not_installed("pcadapt")
  fx <- pcadapt_structured_fixture()
  res <- popgenVCF:::run_pcadapt_scan(
    fx$genotype, fx$snp_ids, fx$chromosome, fx$position,
    k = NULL, n_populations = 2L, min_maf = 0.05, fdr_alpha = 0.05
  )
  expect_identical(res$k, 1L) # n_populations - 1
  expect_identical(res$n_tested, 300L)
  detected <- res$table[outlier == TRUE, snp_id]
  expect_setequal(detected, fx$snp_ids[fx$outlier_idx])
  expect_true(is.finite(res$gif) && res$gif > 0)
})

test_that("run_pcadapt_scan finds no outliers in purely random, unstructured genotypes", {
  skip_if_not_installed("pcadapt")
  set.seed(7)
  n <- 30L; l <- 200L
  geno <- matrix(sample(0:2, n * l, replace = TRUE, prob = c(0.4, 0.2, 0.4)), n, l)
  res <- popgenVCF:::run_pcadapt_scan(
    geno, paste0("snp", seq_len(l)), rep("22", l), sort(sample(1:1e6, l)),
    k = 2L, n_populations = 3L, min_maf = 0.05, fdr_alpha = 0.05
  )
  expect_identical(res$n_outliers, 0L)
})

test_that("run_pcadapt_scan's p_value/maf/stat vectors align positionally with the input locus order, including min.maf-excluded loci", {
  skip_if_not_installed("pcadapt")
  set.seed(11)
  n <- 40L; l <- 20L
  geno <- matrix(sample(0:2, n * l, replace = TRUE), n, l)
  geno[, 1] <- 0L # force monomorphic -> excluded by min.maf
  # More samples than loci here is a deliberately small fixture; pcadapt's
  # own orientation sanity-check warning is expected and harmless.
  res <- suppressWarnings(popgenVCF:::run_pcadapt_scan(
    geno, paste0("snp", seq_len(l)), rep("22", l), seq_len(l) * 100L,
    k = 2L, min_maf = 0.05, fdr_alpha = 0.05
  ))
  expect_identical(nrow(res$table), 20L)
  expect_identical(res$table$snp_id[1], "snp1")
  expect_true(is.na(res$table$p_value[1]))
  expect_identical(res$table$maf[1], 0)
  expect_false(res$table$outlier[1])
})

test_that("run_pcadapt_scan errors clearly with fewer than two loci", {
  skip_if_not_installed("pcadapt")
  geno <- matrix(sample(0:2, 10, replace = TRUE), 10, 1)
  expect_error(
    popgenVCF:::run_pcadapt_scan(geno, "s1", "22", 100L),
    "at least two loci"
  )
})

test_that("validate_pcadapt_result accepts a well-formed result and flags real defects", {
  skip_if_not_installed("pcadapt")
  fx <- pcadapt_structured_fixture()
  res <- popgenVCF:::run_pcadapt_scan(
    fx$genotype, fx$snp_ids, fx$chromosome, fx$position,
    k = NULL, n_populations = 2L, min_maf = 0.05, fdr_alpha = 0.05
  )
  ok <- popgenVCF:::validate_pcadapt_result(res, NULL, NULL)
  expect_true(ok$valid)

  bad_q <- res
  bad_q$table <- data.table::copy(res$table)
  bad_q$table[1L, q_value := p_value - 0.5]
  expect_false(popgenVCF:::validate_pcadapt_result(bad_q, NULL, NULL)$valid)

  bad_outlier <- res
  bad_outlier$table <- data.table::copy(res$table)
  bad_outlier$table[1L, `:=`(outlier = TRUE, q_value = NA_real_)]
  expect_false(popgenVCF:::validate_pcadapt_result(bad_outlier, NULL, NULL)$valid)

  incomplete <- list(table = data.table::data.table())
  expect_false(popgenVCF:::validate_pcadapt_result(incomplete, NULL, NULL)$valid)
})

test_that("pcadapt_module_spec is registered, declares no dependency, and is enabled by default", {
  registry <- popgenVCF::default_analysis_registry()
  expect_true("pcadapt" %in% names(registry$modules))
  module <- registry$modules$pcadapt

  spec <- popgenVCF::pcadapt_module_spec()
  expect_identical(module$run, spec$run)
  expect_identical(module$requires, spec$requires)
  # Deliberately no `requires = "diversity"`: diversity is population-gated
  # and pcadapt must not be (see pcadapt_module_spec()'s own docs) -- a real
  # dependency-graph bug this project caught via a no-metadata pipeline run.
  expect_identical(module$requires, character())
  expect_identical(module$validate, spec$validate)
  expect_identical(module$outputs, spec$outputs)
  expect_identical(module$references, spec$references)
  expect_identical(module$resource_class, spec$resource_class)
  expect_identical(module$contract_version, spec$contract_version)

  cfg <- popgenVCF::default_config()
  expect_true(spec$enabled(cfg))
  cfg$analyses$pcadapt <- FALSE
  expect_false(spec$enabled(cfg))
})

test_that("pcadapt is not gated behind population metadata, unlike amova/bottleneck/clonality", {
  registry <- popgenVCF::default_analysis_registry()
  capabilities <- list(metadata_supplied = TRUE, population = FALSE, population_levels = 0L, coordinates = FALSE)
  table <- popgenVCF:::analysis_capability_table(registry, capabilities)
  pcadapt_row <- table[module == "pcadapt"]
  expect_true(pcadapt_row$available)
})

test_that("plot_pcadapt writes a figure file when at least one locus was tested", {
  skip_if_not_installed("pcadapt")
  fx <- pcadapt_structured_fixture()
  res <- popgenVCF:::run_pcadapt_scan(
    fx$genotype, fx$snp_ids, fx$chromosome, fx$position,
    k = NULL, n_populations = 2L, min_maf = 0.05, fdr_alpha = 0.05
  )
  cfg <- popgenVCF::default_config(); cfg$output$figure_formats <- "png"
  out <- tempfile("pcadapt-plot-"); dirs <- list(figures = file.path(out, "figures"))
  dir.create(dirs$figures, recursive = TRUE)

  popgenVCF:::plot_pcadapt(res, cfg, dirs)
  expect_true(file.exists(file.path(dirs$figures, "59_pcadapt_manhattan.png")))
})

test_that("run_pcadapt_scan degrades gracefully on a real, directly-observed numerical-stability failure (too little data relative to K)", {
  skip_if_not_installed("pcadapt")
  # This package's own tiny CI validation fixture, restricted to its real
  # QC-passing marker set (7 loci for 8 samples -- exactly what
  # run_module_pcadapt() itself operates on), reliably triggers "system is
  # computationally singular" inside pcadapt's internal per-locus
  # regression -- caught by a real end-to-end no-metadata pipeline run, not
  # manufactured for this test. (The full, unfiltered marker set does NOT
  # reproduce this -- QC filtering, not the fixture per se, is what
  # triggers it, so this test derives qc_snps via the real variant_qc()
  # rather than hardcoding indices.)
  paths <- popgenVCF:::validation_fixture_paths()
  gds_path <- tempfile(fileext = ".gds")
  gds <- popgenVCF:::prepare_gds(paths$vcf, gds_path, force = TRUE)
  on.exit({
    try(SNPRelate::snpgdsClose(gds), silent = TRUE)
    unlink(gds_path, force = TRUE)
  }, add = TRUE)
  ids <- popgenVCF:::get_gds_ids(gds)
  sample_ids <- as.character(ids$sample)
  vq <- popgenVCF:::variant_qc(gds, sample_ids, ids, 0.05, 0.2)
  snp_ids <- vq[pass_combined == TRUE, snp_id]
  expect_true(length(snp_ids) >= 2L) # sanity: the fixture still has usable loci
  geno <- SNPRelate::snpgdsGetGeno(gds, sample.id = sample_ids, snp.id = snp_ids, snpfirstdim = FALSE, verbose = FALSE)

  res <- suppressWarnings(popgenVCF:::run_pcadapt_scan(
    geno, snp_ids, ids$chromosome[match(snp_ids, ids$snp)], ids$position[match(snp_ids, ids$snp)],
    k = NULL, n_populations = NA_integer_, min_maf = 0.05, fdr_alpha = 0.05
  ))
  expect_true(res$failed)
  expect_identical(res$reason, "numerical_instability")
  expect_identical(res$n_tested, 0L)
  expect_identical(res$n_outliers, 0L)
  expect_identical(nrow(res$table), length(snp_ids))
  expect_true(all(is.na(res$table$p_value)))
  expect_false(any(res$table$outlier))

  ok <- popgenVCF:::validate_pcadapt_result(res, NULL, NULL)
  expect_true(ok$valid)
})

test_that("run_pcadapt_scan degrades gracefully, not fatally, when the optional pcadapt package is not installed", {
  # A real production incident: a v1.0.0 container image that omitted the
  # optional pcadapt conda package turned this into a hard stop() that
  # killed an entire real run's ~45 minutes of completed upstream results.
  # pcadapt defaults on for every user (unlike opt-in ml_tree), so a
  # missing package must degrade the same way the numerical-instability
  # failure above does. requireNamespace() is a base function called
  # unqualified from inside popgenVCF's own namespace, which
  # testthat::local_mocked_bindings() cannot shadow (it only mocks bindings
  # a package defines or explicitly imports) -- so this exercises the real
  # absence directly instead: it runs for real wherever pcadapt genuinely
  # is not installed (as in a container image missing it, or this
  # environment) and is skipped only where it happens to be present.
  testthat::skip_if(
    requireNamespace("pcadapt", quietly = TRUE),
    "pcadapt is installed in this environment; cannot exercise the missing-package path"
  )
  set.seed(1)
  n <- 10L; l <- 5L
  geno <- matrix(sample(0:2, n * l, replace = TRUE), n, l)
  snp_ids <- paste0("snp", seq_len(l))
  res <- popgenVCF:::run_pcadapt_scan(
    geno, snp_ids, rep("22", l), seq_len(l) * 1000L,
    k = NULL, n_populations = 2L, min_maf = 0.05, fdr_alpha = 0.05
  )
  expect_true(res$failed)
  expect_identical(res$reason, "package_missing")
  expect_identical(res$n_tested, 0L)
  expect_identical(res$n_outliers, 0L)
  expect_identical(nrow(res$table), length(snp_ids))
  expect_true(all(is.na(res$table$p_value)))
  expect_false(any(res$table$outlier))

  ok <- popgenVCF:::validate_pcadapt_result(res, NULL, NULL)
  expect_true(ok$valid)
})

test_that("run_module_pcadapt does not depend on the population-gated diversity module (real no-metadata pipeline run)", {
  skip_if_not_installed("pcadapt")
  skip_if(Sys.which("bcftools") == "", "bcftools is not available")
  pg_env <- popgenVCF:::.pg_env
  on.exit(pg_env$log_file <- NULL, add = TRUE)
  paths <- popgenVCF:::validation_fixture_paths()
  root <- withr::local_tempdir()
  cfg <- popgenVCF::default_config()
  cfg$input$vcf <- paths$vcf
  cfg$output$directory <- root
  cfg$compute$threads <- 1L
  cfg$analyses$n_pcs <- 3L
  cfg$report$enabled <- FALSE

  analysis <- suppressWarnings(popgenVCF::run_pipeline(cfg))
  expect_identical(analysis$status, "complete")
  pcadapt_result <- popgenVCF::get_analysis_result(analysis, "pcadapt")
  expect_true(!is.null(pcadapt_result))
})
