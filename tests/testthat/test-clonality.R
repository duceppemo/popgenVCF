clonality_fixture_genotype <- function(n = 20L, l = 40L, seed = 1L) {
  set.seed(seed)
  geno <- matrix(sample(0:2, n * l, replace = TRUE), n, l)
  dimnames(geno) <- list(paste0("VCF_S", seq_len(n)), paste0("snp", seq_len(l)))
  geno
}

test_that("clonality_encode_genind maps dosage to codominant genotype codes and handles missing data", {
  geno <- matrix(c(0, 1, 2, NA, 0, 2), nrow = 2, dimnames = list(c("A", "B"), c("s1", "s2", "s3")))
  gid <- popgenVCF:::clonality_encode_genind(geno, c("A", "B"), c("pop1", "pop1"))
  expect_s4_class(gid, "genind")
  expect_identical(adegenet::nInd(gid), 2L)
  expect_identical(adegenet::nLoc(gid), 3L)
  # matrix is filled column-major: s1 = (A=0, B=1), s2 = (A=2, B=NA), s3 = (A=0, B=2)
  tab <- adegenet::tab(gid)
  expect_true(all(is.na(tab["B", grepl("^s2\\.", colnames(tab))])))
  expect_equal(unname(tab["B", grepl("^s1\\.", colnames(tab))]), c(1, 1))
})

test_that("run_clonality detects an exact duplicate pair as a shared multilocus genotype", {
  geno <- clonality_fixture_genotype()
  geno["VCF_S2", ] <- geno["VCF_S1", ]
  metadata <- data.table::data.table(
    sample = rownames(geno), population = rep(c("A", "B"), each = 10)
  )
  res <- popgenVCF:::run_clonality(geno, rownames(geno), metadata, seed = 42, curve_replicates = 10)

  expect_true(nrow(res$groups) >= 1L)
  dup_group <- res$groups[grepl("VCF_S1", samples) & grepl("VCF_S2", samples)]
  expect_identical(nrow(dup_group), 1L)
  expect_identical(dup_group$n_members, 2L)
  expect_false(dup_group$cross_population)
})

test_that("run_clonality flags a cross-population shared genotype", {
  geno <- clonality_fixture_genotype()
  geno["VCF_S11", ] <- geno["VCF_S1", ] # S1 is population A (first 10), S11 is population B
  metadata <- data.table::data.table(
    sample = rownames(geno), population = rep(c("A", "B"), each = 10)
  )
  res <- popgenVCF:::run_clonality(geno, rownames(geno), metadata, seed = 42, curve_replicates = 0)
  dup_group <- res$groups[grepl("VCF_S1(,|$)", samples) & grepl("VCF_S11", samples)]
  expect_identical(nrow(dup_group), 1L)
  expect_true(dup_group$cross_population)
})

test_that("run_clonality's summary table has one row per population plus Total, with sane bounds", {
  geno <- clonality_fixture_genotype()
  metadata <- data.table::data.table(
    sample = rownames(geno), population = rep(c("A", "B"), each = 10)
  )
  res <- popgenVCF:::run_clonality(geno, rownames(geno), metadata, seed = 42, curve_replicates = 0)
  expect_setequal(res$summary$population, c("A", "B", "Total"))
  expect_true(all(res$summary$mlg <= res$summary$n))
  expect_true(all(res$summary$mlg >= 1L))
  total_row <- res$summary[population == "Total"]
  expect_equal(res$n_mlg_total, as.numeric(total_row$mlg))
})

test_that("run_clonality's genotype accumulation curve is non-decreasing on average and bounded by the total MLG count", {
  geno <- clonality_fixture_genotype(n = 20L, l = 30L, seed = 3L)
  metadata <- data.table::data.table(
    sample = rownames(geno), population = rep(c("A", "B"), each = 10)
  )
  res <- popgenVCF:::run_clonality(geno, rownames(geno), metadata, seed = 42, curve_replicates = 30)
  expect_true(nrow(res$curve) > 0L)
  expect_true(all(diff(res$curve$mean_mlg) >= -1e-8))
  expect_true(all(res$curve$mean_mlg <= res$n_mlg_total + 1e-8))
})

test_that("run_clonality errors clearly with a non-missing-population requirement", {
  geno <- clonality_fixture_genotype(n = 6L, l = 10L)
  metadata <- data.table::data.table(sample = rownames(geno), population = c(rep("A", 5L), NA))
  expect_error(
    popgenVCF:::run_clonality(geno, rownames(geno), metadata, seed = 42),
    "non-missing population"
  )
})

test_that("run_clonality errors clearly with fewer than two loci", {
  geno <- clonality_fixture_genotype(n = 6L, l = 1L)
  metadata <- data.table::data.table(sample = rownames(geno), population = "A")
  expect_error(
    popgenVCF:::run_clonality(geno, rownames(geno), metadata, seed = 42),
    "at least two polymorphic loci"
  )
})

test_that("run_clonality is deterministic given the same seed", {
  geno <- clonality_fixture_genotype(n = 16L, l = 25L, seed = 5L)
  metadata <- data.table::data.table(
    sample = rownames(geno), population = rep(c("A", "B"), each = 8)
  )
  res1 <- popgenVCF:::run_clonality(geno, rownames(geno), metadata, seed = 7, curve_replicates = 15)
  res2 <- popgenVCF:::run_clonality(geno, rownames(geno), metadata, seed = 7, curve_replicates = 15)
  expect_identical(res1$summary, res2$summary)
  expect_identical(res1$curve, res2$curve)
})

test_that("plot_clonality writes a figure file when a curve was computed", {
  geno <- clonality_fixture_genotype()
  metadata <- data.table::data.table(
    sample = rownames(geno), population = rep(c("A", "B"), each = 10)
  )
  res <- popgenVCF:::run_clonality(geno, rownames(geno), metadata, seed = 42, curve_replicates = 10)
  cfg <- popgenVCF::default_config(); cfg$output$figure_formats <- "png"
  out <- tempfile("clonality-plot-"); dirs <- list(figures = file.path(out, "figures"))
  dir.create(dirs$figures, recursive = TRUE)

  popgenVCF:::plot_clonality(res, cfg, dirs)
  expect_true(file.exists(file.path(dirs$figures, "58_genotype_accumulation_curve.png")))
})

test_that("plot_clonality is a no-op when the curve is empty (curve_replicates = 0)", {
  geno <- clonality_fixture_genotype()
  metadata <- data.table::data.table(
    sample = rownames(geno), population = rep(c("A", "B"), each = 10)
  )
  res <- popgenVCF:::run_clonality(geno, rownames(geno), metadata, seed = 42, curve_replicates = 0)
  cfg <- popgenVCF::default_config(); cfg$output$figure_formats <- "png"
  out <- tempfile("clonality-empty-"); dirs <- list(figures = file.path(out, "figures"))
  dir.create(dirs$figures, recursive = TRUE)

  popgenVCF:::plot_clonality(res, cfg, dirs)
  expect_false(file.exists(file.path(dirs$figures, "58_genotype_accumulation_curve.png")))
})

test_that("validate_clonality_result accepts a well-formed result and flags real defects", {
  geno <- clonality_fixture_genotype()
  geno["VCF_S2", ] <- geno["VCF_S1", ]
  metadata <- data.table::data.table(
    sample = rownames(geno), population = rep(c("A", "B"), each = 10)
  )
  res <- popgenVCF:::run_clonality(geno, rownames(geno), metadata, seed = 42, curve_replicates = 10)

  ok <- popgenVCF:::validate_clonality_result(res, NULL, NULL)
  expect_true(ok$valid)
  expect_match(ok$warnings, "share an identical multilocus genotype")

  bad_mlg <- res
  bad_mlg$summary <- data.table::copy(res$summary)
  bad_mlg$summary[1L, mlg := n + 1L]
  expect_false(popgenVCF:::validate_clonality_result(bad_mlg, NULL, NULL)$valid)

  bad_group <- res
  bad_group$groups <- data.table::copy(res$groups)
  bad_group$groups[1L, n_members := 1L]
  expect_false(popgenVCF:::validate_clonality_result(bad_group, NULL, NULL)$valid)

  incomplete <- list(summary = data.table::data.table())
  expect_false(popgenVCF:::validate_clonality_result(incomplete, NULL, NULL)$valid)
})

test_that("clonality_empty_summary has the same columns clonality_rename_summary produces, so downstream code sees a consistent schema", {
  empty <- popgenVCF:::clonality_empty_summary()
  expect_identical(nrow(empty), 0L)
  expect_true(all(c(
    "population", "n", "mlg", "emlg", "emlg_se", "shannon_h", "stoddart_taylor_g",
    "simpson_lambda", "evenness_e5", "expected_heterozygosity", "ia", "rbard",
    "ia_p_value", "rbard_p_value"
  ) %in% names(empty)))
})

test_that("clonality_run_poppr_isolated returns the real result on ordinary success", {
  geno <- clonality_fixture_genotype()
  metadata <- data.table::data.table(
    sample = rownames(geno), population = rep(c("A", "B"), each = 10)
  )
  gid <- popgenVCF:::clonality_encode_genind(geno, rownames(geno), metadata$population)
  gc <- poppr::as.genclone(gid)
  out <- popgenVCF:::clonality_run_poppr_isolated(gc, 0L)
  expect_s3_class(out, "data.frame")
  expect_true("Total" %in% out$Pop)
})

test_that("clonality_run_poppr_isolated returns NULL, not an error, when the wrapped call itself errors", {
  # A crash (segfault) can't be simulated safely inside the test suite (see
  # R/clonality.R's header comment on the real reproduction), so this exercises
  # the same NULL-on-failure contract via an ordinary R-level error instead --
  # the isolation wrapper must degrade gracefully either way.
  fake_gc <- structure(list(), class = "not_a_genclone")
  out <- popgenVCF:::clonality_run_poppr_isolated(fake_gc, 0L)
  expect_null(out)
})

test_that("run_clonality falls back to an empty, well-typed summary and sets poppr_failed when poppr::poppr() cannot be run", {
  geno <- clonality_fixture_genotype()
  metadata <- data.table::data.table(
    sample = rownames(geno), population = rep(c("A", "B"), each = 10)
  )
  testthat::local_mocked_bindings(
    clonality_run_poppr_isolated = function(gc, ia_permutations) NULL
  )
  res <- popgenVCF:::run_clonality(geno, rownames(geno), metadata, seed = 42, curve_replicates = 5)
  expect_true(res$poppr_failed)
  expect_identical(nrow(res$summary), 0L)
  expect_identical(names(res$summary), names(popgenVCF:::clonality_empty_summary()))
  expect_true(is.na(res$n_mlg_total))
  # groups/curve don't depend on poppr::poppr() and should be unaffected
  expect_true(is.data.frame(res$groups))
})

test_that("validate_clonality_result accepts the poppr-failed empty-summary fallback without error", {
  geno <- clonality_fixture_genotype()
  metadata <- data.table::data.table(
    sample = rownames(geno), population = rep(c("A", "B"), each = 10)
  )
  testthat::local_mocked_bindings(
    clonality_run_poppr_isolated = function(gc, ia_permutations) NULL
  )
  res <- popgenVCF:::run_clonality(geno, rownames(geno), metadata, seed = 42, curve_replicates = 5)
  ok <- popgenVCF:::validate_clonality_result(res, NULL, NULL)
  expect_true(ok$valid)
})

test_that("clonality_module_spec is registered, requires diversity, and is enabled by default", {
  registry <- popgenVCF::default_analysis_registry()
  expect_true("clonality" %in% names(registry$modules))
  module <- registry$modules$clonality

  spec <- popgenVCF::clonality_module_spec()
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
  cfg$analyses$clonality <- FALSE
  expect_false(spec$enabled(cfg))
})
