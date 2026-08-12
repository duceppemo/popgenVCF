test_that("jost_hs_ht/jost_d_from_hs_ht match an exact hand calculation", {
  # 2 populations, 10 individuals each, 1 locus: p_A = 0.2, p_B = 0.8.
  # harm_n = 10 (equal sample sizes); hand-derived through Hs_est, Ht_est, D.
  freq_mat <- matrix(c(0.2, 0.8), nrow = 2L)
  n_mat <- matrix(c(10, 10), nrow = 2L)
  hh <- popgenVCF:::jost_hs_ht(freq_mat, n_mat)

  expect_equal(hh$hs_est, 0.3368421053, tolerance = 1e-9)
  expect_equal(hh$ht_est, 0.5084210526, tolerance = 1e-9)

  d <- popgenVCF:::jost_d_from_hs_ht(hh$hs_est, hh$ht_est, 2L)
  expect_equal(d, 0.5174603175, tolerance = 1e-9)
})

test_that("compute_jost_d matches mmod::D_Jost() on a synthetic multi-population fixture (cross-validated externally, see NEWS.md)", {
  # Allele frequencies below are the exact result of a real integer
  # genotype-dosage matrix (4 populations, unequal sample sizes 15/22/9/18,
  # 3 loci) run through the actual mmod::D_Jost() reference implementation
  # (Winter 2012) during development: it returned global.het D = 0.2257385,
  # matching this package's own implementation to floating-point precision
  # (exact, diff = 0) before shipping. mmod is not a dependency of this
  # package, so the value is pinned here as a literal rather than
  # re-installing mmod at test time.
  locus_table <- data.table::data.table(
    population = rep(c("A", "B", "C", "D"), each = 3L),
    snp_id = rep(c("s1", "s2", "s3"), 4L),
    n_called = c(15L, 15L, 15L, 22L, 22L, 22L, 9L, 9L, 9L, 18L, 18L, 18L),
    alternate_allele_frequency = c(
      0.3000000, 0.6000000, 0.1000000, # A
      0.5454545, 0.2045455, 0.7954545, # B
      0.1666667, 0.8888889, 0.3888889, # C
      0.6944444, 0.4444444, 0.2500000  # D
    )
  )
  res <- popgenVCF:::compute_jost_d(locus_table)

  expect_equal(res$global, 0.2257385, tolerance = 1e-6)
  expect_identical(nrow(res$long), 6L) # choose(4, 2)
  expect_true(all(is.finite(res$long$jost_d)))
  expect_identical(dim(res$matrix), c(4L, 4L))
  expect_identical(diag(res$matrix), c(A = 0, B = 0, C = 0, D = 0))
  expect_equal(res$matrix["A", "B"], res$long[population_1 == "A" & population_2 == "B", jost_d])
})

test_that("compute_jost_d excludes loci with a zero-call population, matching Nei's-distance convention", {
  locus_table <- data.table::data.table(
    population = c("A", "A", "B", "B"),
    snp_id = c("s1", "s2", "s1", "s2"),
    n_called = c(10L, 10L, 10L, 0L),
    alternate_allele_frequency = c(0.3, 0.5, 0.6, NA_real_)
  )
  res <- popgenVCF:::compute_jost_d(locus_table)
  expect_identical(res$long$jost_d_n_snps, 1L)
})

test_that("compute_jost_d returns an empty/NA result with fewer than two populations", {
  locus_table <- data.table::data.table(
    population = "A", snp_id = "s1", n_called = 10L, alternate_allele_frequency = 0.3
  )
  res <- popgenVCF:::compute_jost_d(locus_table)
  expect_true(is.na(res$global))
  expect_identical(nrow(res$long), 0L)
  expect_identical(dim(res$matrix), c(0L, 0L))
})

test_that("run_module_fst merges Jost's D and population-specific FST into the same tables as FST/Nm", {
  paths <- popgenVCF:::validation_fixture_paths()
  gds_path <- tempfile(fileext = ".gds")
  gds <- popgenVCF:::prepare_gds(paths$vcf, gds_path, force = TRUE)
  on.exit({
    try(SNPRelate::snpgdsClose(gds), silent = TRUE)
    unlink(gds_path, force = TRUE)
  }, add = TRUE)

  ids <- popgenVCF:::get_gds_ids(gds)
  metadata <- popgenVCF:::read_metadata(paths$metadata, "yes")
  snp_ids <- as.character(ids$snp)
  sample_ids <- as.character(ids$sample)

  cfg <- popgenVCF::default_config()
  cfg$analyses$bootstrap$enabled <- FALSE
  dirs <- list(tables = tempfile("tables-"), figures = tempfile("figures-"))
  dir.create(dirs$tables); dir.create(dirs$figures)
  cfg$output$figure_formats <- "png"

  div <- popgenVCF:::compute_diversity(gds, sample_ids, snp_ids, metadata, ids)
  analysis <- popgenVCF:::new_popgen_vcf_analysis(cfg)
  context <- list(
    cfg = cfg, dirs = dirs, gds = gds, sample_ids = sample_ids,
    qc_snps = snp_ids, metadata = metadata, ids = ids, diversity_full = div
  )
  out <- popgenVCF:::run_module_fst(analysis, context)
  fst <- popgenVCF::get_analysis_result(out$analysis, "fst")

  expect_true("global_jost_d" %in% names(fst))
  expect_true(all(c("jost_d", "jost_d_n_snps") %in% names(fst$long)))
  expect_true(file.exists(file.path(dirs$tables, "19b_pairwise_jost_d_matrix.tsv")))
  global_table <- data.table::fread(file.path(dirs$tables, "17_global_FST.tsv"))
  expect_true("global_jost_d" %in% names(global_table))

  expect_true("global_beta_fst" %in% names(fst))
  expect_true("global_beta_fst" %in% names(global_table))
  expect_true("population_specific_fst" %in% names(fst))
  if (requireNamespace("hierfstat", quietly = TRUE)) {
    expect_true(nrow(fst$population_specific_fst) > 0L)
    expect_true(file.exists(file.path(dirs$tables, "51_population_specific_fst.tsv")))
  }
})
