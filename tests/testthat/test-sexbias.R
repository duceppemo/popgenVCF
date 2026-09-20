sexbias_fixture_genotype <- function(n = 20L, l = 40L, seed = 1L) {
  set.seed(seed)
  geno <- matrix(sample(0:2, n * l, replace = TRUE), n, l)
  dimnames(geno) <- list(paste0("VCF_S", seq_len(n)), paste0("snp", seq_len(l)))
  geno
}

test_that("run_sexbias returns NULL when the metadata has no sex column", {
  skip_if_not_installed("hierfstat")
  geno <- sexbias_fixture_genotype()
  metadata <- data.table::data.table(
    sample = rownames(geno), population = rep(c("A", "B"), each = 10)
  )
  expect_null(popgenVCF:::run_sexbias(geno, rownames(geno), metadata))
})

test_that("run_sexbias returns NULL when fewer than two samples share a recorded sex", {
  skip_if_not_installed("hierfstat")
  geno <- sexbias_fixture_genotype()
  metadata <- data.table::data.table(
    sample = rownames(geno), population = rep(c("A", "B"), each = 10),
    sex = c("male", rep("female", 19L))
  )
  expect_null(popgenVCF:::run_sexbias(geno, rownames(geno), metadata))
})

test_that("run_sexbias returns NULL when only one recorded sex is usable", {
  skip_if_not_installed("hierfstat")
  geno <- sexbias_fixture_genotype()
  metadata <- data.table::data.table(
    sample = rownames(geno), population = rep(c("A", "B"), each = 10),
    sex = rep("female", 20L)
  )
  expect_null(popgenVCF:::run_sexbias(geno, rownames(geno), metadata))
})

test_that("run_sexbias excludes samples with missing or unrecognized sex labels", {
  skip_if_not_installed("hierfstat")
  geno <- sexbias_fixture_genotype()
  sex <- rep(c("male", "female"), each = 10)
  sex[c(1, 2)] <- c(NA, "unknown")
  metadata <- data.table::data.table(
    sample = rownames(geno), population = rep(c("A", "B"), each = 10), sex = sex
  )
  res <- popgenVCF:::run_sexbias(geno, rownames(geno), metadata)
  expect_false(is.null(res))
  expect_identical(nrow(res$table), 18L)
  expect_false(any(res$table$sample %in% c("VCF_S1", "VCF_S2")))
})

test_that("run_sexbias's mAIc t-test result has sane structure and bounds", {
  skip_if_not_installed("hierfstat")
  geno <- sexbias_fixture_genotype(n = 30L, l = 50L, seed = 2L)
  metadata <- data.table::data.table(
    sample = rownames(geno), population = rep(c("A", "B"), each = 15),
    sex = rep(c("male", "female"), 15)
  )
  res <- popgenVCF:::run_sexbias(geno, rownames(geno), metadata, test = "mAIc", seed = 42)

  expect_identical(res$test, "mAIc")
  expect_identical(res$permutations, 0L)
  expect_true(is.finite(res$statistic))
  expect_true(is.finite(res$p_value) && res$p_value >= 0 && res$p_value <= 1)
  expect_identical(res$n_female + res$n_male, 30L)
  expect_setequal(names(res$table), c("sample", "population", "sex", "aic"))
  expect_setequal(unique(res$table$sex), c("female", "male"))
  expect_identical(nrow(res$table), 30L)
})

test_that("run_sexbias's permutation test is deterministic given the same seed", {
  skip_if_not_installed("hierfstat")
  geno <- sexbias_fixture_genotype(n = 24L, l = 30L, seed = 3L)
  metadata <- data.table::data.table(
    sample = rownames(geno), population = rep(c("A", "B"), each = 12),
    sex = rep(c("male", "female"), 12)
  )
  res1 <- popgenVCF:::run_sexbias(geno, rownames(geno), metadata, test = "mAIc", permutations = 50L, seed = 7)
  res2 <- popgenVCF:::run_sexbias(geno, rownames(geno), metadata, test = "mAIc", permutations = 50L, seed = 7)
  expect_identical(res1$statistic, res2$statistic)
  expect_identical(res1$p_value, res2$p_value)
  expect_identical(res1$permutations, 50L)
})

test_that("run_sexbias's FST test requires permutations, matching hierfstat's own behavior", {
  skip_if_not_installed("hierfstat")
  geno <- sexbias_fixture_genotype(n = 20L, l = 30L, seed = 4L)
  metadata <- data.table::data.table(
    sample = rownames(geno), population = rep(c("A", "B"), each = 10),
    sex = rep(c("male", "female"), 10)
  )
  expect_error(popgenVCF:::run_sexbias(geno, rownames(geno), metadata, test = "FST", permutations = 0L))
  res <- popgenVCF:::run_sexbias(geno, rownames(geno), metadata, test = "FST", permutations = 20L, seed = 1)
  expect_identical(res$test, "FST")
  expect_true(is.finite(res$p_value))
})

test_that("plot_sexbias writes a figure file when a result is available", {
  skip_if_not_installed("hierfstat")
  geno <- sexbias_fixture_genotype()
  metadata <- data.table::data.table(
    sample = rownames(geno), population = rep(c("A", "B"), each = 10),
    sex = rep(c("male", "female"), 10)
  )
  res <- popgenVCF:::run_sexbias(geno, rownames(geno), metadata)
  cfg <- popgenVCF::default_config(); cfg$output$figure_formats <- "png"
  out <- tempfile("sexbias-plot-"); dirs <- list(figures = file.path(out, "figures"))
  dir.create(dirs$figures, recursive = TRUE)

  popgenVCF:::plot_sexbias(res, cfg, dirs)
  expect_true(file.exists(file.path(dirs$figures, "60_sexbias_AIc_by_sex.png")))
})

test_that("plot_sexbias is a no-op when the result is NULL", {
  cfg <- popgenVCF::default_config(); cfg$output$figure_formats <- "png"
  out <- tempfile("sexbias-empty-"); dirs <- list(figures = file.path(out, "figures"))
  dir.create(dirs$figures, recursive = TRUE)

  popgenVCF:::plot_sexbias(NULL, cfg, dirs)
  expect_false(file.exists(file.path(dirs$figures, "60_sexbias_AIc_by_sex.png")))
})

test_that("validate_sexbias_result accepts a well-formed result, a skipped NULL result, and flags real defects", {
  skip_if_not_installed("hierfstat")
  geno <- sexbias_fixture_genotype()
  metadata <- data.table::data.table(
    sample = rownames(geno), population = rep(c("A", "B"), each = 10),
    sex = rep(c("male", "female"), 10)
  )
  res <- popgenVCF:::run_sexbias(geno, rownames(geno), metadata)

  ok <- popgenVCF:::validate_sexbias_result(res, NULL, NULL)
  expect_true(ok$valid)

  skipped <- popgenVCF:::validate_sexbias_result(NULL, NULL, NULL)
  expect_true(skipped$valid)
  expect_match(skipped$warnings, "skipped")

  bad_p <- res; bad_p$p_value <- 1.5
  expect_false(popgenVCF:::validate_sexbias_result(bad_p, NULL, NULL)$valid)

  incomplete <- list(table = data.table::data.table())
  expect_false(popgenVCF:::validate_sexbias_result(incomplete, NULL, NULL)$valid)
})

test_that("run_sexbias does not perturb the caller's global RNG state", {
  # set.seed(seed) here previously left the global RNG state permanently
  # re-seeded for the rest of the R session, silently coupling every
  # subsequently executed randomized analysis (bootstrap trees, other
  # permutation tests) to whether/when sexbias happened to run.
  skip_if_not_installed("hierfstat")
  geno <- sexbias_fixture_genotype(n = 24L, l = 30L, seed = 3L)
  metadata <- data.table::data.table(
    sample = rownames(geno), population = rep(c("A", "B"), each = 12),
    sex = rep(c("male", "female"), 12)
  )
  set.seed(123L)
  before <- runif(5L)

  set.seed(123L)
  invisible(popgenVCF:::run_sexbias(
    geno, rownames(geno), metadata, test = "mAIc", permutations = 20L, seed = 999L
  ))
  after <- runif(5L)

  expect_identical(before, after)
})

test_that("sexbias_module_spec is registered, requires diversity, and is enabled by default", {
  registry <- popgenVCF::default_analysis_registry()
  expect_true("sexbias" %in% names(registry$modules))
  module <- registry$modules$sexbias

  spec <- popgenVCF::sexbias_module_spec()
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
  cfg$analyses$sexbias <- FALSE
  expect_false(spec$enabled(cfg))
})

test_that("sexbias is gated behind population metadata, like clonality/amova/bottleneck", {
  registry <- popgenVCF::default_analysis_registry()
  capabilities <- list(metadata_supplied = TRUE, population = FALSE, population_levels = 0L, coordinates = FALSE)
  table <- popgenVCF:::analysis_capability_table(registry, capabilities)
  row <- table[module == "sexbias"]
  expect_false(row$available)
})

test_that("sexbias is enabled by default in default_config()", {
  # sexbias correctly skips itself at runtime, via the capability gate
  # above, whenever the metadata `sex` column it actually needs is absent.
  cfg <- popgenVCF::default_config()
  expect_true(cfg$analyses$sexbias)
})

test_that("run_module_sexbias gracefully skips against this package's real, sex-column-free CI validation fixture", {
  skip_if_not_installed("hierfstat")
  paths <- popgenVCF:::validation_fixture_paths()
  metadata <- data.table::fread(paths$metadata)
  expect_false("sex" %in% names(metadata))

  gds_path <- tempfile(fileext = ".gds")
  gds <- popgenVCF:::prepare_gds(paths$vcf, gds_path, force = TRUE)
  on.exit({
    try(SNPRelate::snpgdsClose(gds), silent = TRUE)
    unlink(gds_path, force = TRUE)
  }, add = TRUE)
  ids <- popgenVCF:::get_gds_ids(gds)
  sample_ids <- as.character(ids$sample)
  geno <- SNPRelate::snpgdsGetGeno(gds, sample.id = sample_ids, snp.id = ids$snp, snpfirstdim = FALSE, verbose = FALSE)

  res <- popgenVCF:::run_sexbias(geno, sample_ids, metadata)
  expect_null(res)
})

test_that("run_sexbias gives the same result whether or not the samples arrive grouped by population", {
  # hierfstat::samp.within() returns indices concatenated population by
  # population -- a within-population shuffle only for population-sorted
  # rows. In VCF order it moved assignment-index values across populations,
  # so the permutation p-value came from the wrong null.
  skip_if_not_installed("hierfstat")
  geno <- sexbias_fixture_genotype(n = 24L, l = 30L, seed = 3L)
  population <- rep(c("A", "B", "C"), 8L)
  sex <- rep(c("male", "female"), each = 12L)
  unsorted <- data.table::data.table(sample = rownames(geno), population = population, sex = sex)
  ord <- order(population)
  sorted_geno <- geno[ord, , drop = FALSE]
  sorted <- data.table::data.table(sample = rownames(sorted_geno), population = population[ord], sex = sex[ord])

  a <- popgenVCF:::run_sexbias(geno, rownames(geno), unsorted, test = "mAIc", permutations = 99L, seed = 5)
  b <- popgenVCF:::run_sexbias(sorted_geno, rownames(sorted_geno), sorted, test = "mAIc", permutations = 99L, seed = 5)
  expect_equal(a$statistic, b$statistic)
  expect_identical(a$p_value, b$p_value)
  expect_equal(a$table[order(sample), aic], b$table[order(sample), aic])
})

test_that("run_sexbias's FST and FIS tests run on samples that are not grouped by population", {
  # Both failed with "invalid 'times' argument" for any input in VCF order.
  skip_if_not_installed("hierfstat")
  geno <- sexbias_fixture_genotype(n = 20L, l = 30L, seed = 4L)
  metadata <- data.table::data.table(
    sample = rownames(geno), population = rep(c("A", "B"), 10L), sex = rep(c("female", "male"), each = 10L)
  )
  for (test in c("FST", "FIS")) {
    res <- popgenVCF:::run_sexbias(geno, rownames(geno), metadata, test = test, permutations = 20L, seed = 1)
    expect_true(is.finite(res$p_value))
    expect_true(res$p_value >= 0 && res$p_value <= 1)
  }
})

test_that("an undefined sex-bias p-value is a validation warning, not a pipeline-aborting error", {
  skip_if_not_installed("hierfstat")
  geno <- sexbias_fixture_genotype(n = 6L, l = 30L)
  geno[] <- 1L
  metadata <- data.table::data.table(
    sample = rownames(geno), population = rep(c("A", "B"), 3L), sex = rep(c("female", "male"), each = 3L)
  )
  res <- popgenVCF:::run_sexbias(geno, rownames(geno), metadata)
  expect_false(is.finite(res$p_value))
  validation <- popgenVCF:::validate_sexbias_result(res, NULL, NULL)
  expect_true(validation$valid)
  expect_match(validation$warnings, "undefined")

  out_of_range <- res; out_of_range$p_value <- 1.5
  expect_false(popgenVCF:::validate_sexbias_result(out_of_range, NULL, NULL)$valid)
})
