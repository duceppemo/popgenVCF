test_that("dxy_pair matches an exact hand calculation", {
  # 1 locus: p_A = 0.2, p_B = 0.8. dxy = p_A*(1-p_B) + p_B*(1-p_A)
  #        = 0.2*0.2 + 0.8*0.8 = 0.04 + 0.64 = 0.68.
  locus_table <- data.table::data.table(
    population = c("A", "B"), snp_id = c("s1", "s1"),
    n_called = c(10L, 10L), alternate_allele_frequency = c(0.2, 0.8)
  )
  res <- popgenVCF:::dxy_pair(locus_table, "A", "B")
  expect_equal(res$dxy, 0.68, tolerance = 1e-9)
  expect_identical(res$n_snps, 1L)
})

test_that("dxy_pair recovers known bounds: identical frequencies give 2p(1-p); fixed opposite alleles give 1", {
  same_freq <- data.table::data.table(
    population = c("A", "B"), snp_id = c("s1", "s1"),
    n_called = c(10L, 10L), alternate_allele_frequency = c(0.3, 0.3)
  )
  res_same <- popgenVCF:::dxy_pair(same_freq, "A", "B")
  expect_equal(res_same$dxy, 2 * 0.3 * 0.7, tolerance = 1e-9)

  fixed_opposite <- data.table::data.table(
    population = c("A", "B"), snp_id = c("s1", "s1"),
    n_called = c(10L, 10L), alternate_allele_frequency = c(1, 0)
  )
  res_fixed <- popgenVCF:::dxy_pair(fixed_opposite, "A", "B")
  expect_equal(res_fixed$dxy, 1, tolerance = 1e-9)
})

test_that("compute_dxy averages multiple loci and reports the global value as the mean across pairs", {
  # 3 populations, 2 loci each, unequal but fully-called samples.
  locus_table <- data.table::data.table(
    population = rep(c("A", "B", "C"), each = 2L),
    snp_id = rep(c("s1", "s2"), 3L),
    n_called = 10L,
    alternate_allele_frequency = c(
      0.2, 0.6, # A
      0.8, 0.4, # B
      0.5, 0.1  # C
    )
  )
  res <- popgenVCF:::compute_dxy(locus_table)

  ab <- mean(c(0.2 * (1 - 0.8) + 0.8 * (1 - 0.2), 0.6 * (1 - 0.4) + 0.4 * (1 - 0.6)))
  ac <- mean(c(0.2 * (1 - 0.5) + 0.5 * (1 - 0.2), 0.6 * (1 - 0.1) + 0.1 * (1 - 0.6)))
  bc <- mean(c(0.8 * (1 - 0.5) + 0.5 * (1 - 0.8), 0.4 * (1 - 0.1) + 0.1 * (1 - 0.4)))

  expect_identical(nrow(res$long), 3L) # choose(3, 2)
  expect_equal(res$long[population_1 == "A" & population_2 == "B", dxy], ab, tolerance = 1e-9)
  expect_equal(res$long[population_1 == "A" & population_2 == "C", dxy], ac, tolerance = 1e-9)
  expect_equal(res$long[population_1 == "B" & population_2 == "C", dxy], bc, tolerance = 1e-9)
  expect_equal(res$global, mean(c(ab, ac, bc)), tolerance = 1e-9)

  expect_identical(dim(res$matrix), c(3L, 3L))
  expect_identical(diag(res$matrix), c(A = 0, B = 0, C = 0))
  expect_equal(res$matrix["A", "B"], ab, tolerance = 1e-9)
  expect_equal(res$matrix["B", "A"], ab, tolerance = 1e-9)
})

test_that("compute_dxy excludes loci with a zero-call population, matching Jost's D/Nei's-distance convention", {
  locus_table <- data.table::data.table(
    population = c("A", "A", "B", "B"),
    snp_id = c("s1", "s2", "s1", "s2"),
    n_called = c(10L, 10L, 10L, 0L),
    alternate_allele_frequency = c(0.3, 0.5, 0.6, NA_real_)
  )
  res <- popgenVCF:::compute_dxy(locus_table)
  expect_identical(res$long$dxy_n_snps, 1L)
  expect_equal(res$long$dxy, 0.3 * (1 - 0.6) + 0.6 * (1 - 0.3), tolerance = 1e-9)
})

test_that("compute_dxy returns an empty/NA result with fewer than two populations", {
  locus_table <- data.table::data.table(
    population = "A", snp_id = "s1", n_called = 10L, alternate_allele_frequency = 0.3
  )
  res <- popgenVCF:::compute_dxy(locus_table)
  expect_true(is.na(res$global))
  expect_identical(nrow(res$long), 0L)
  expect_identical(dim(res$matrix), c(0L, 0L))
})

test_that("run_module_fst merges Dxy into the same tables as FST/Nm/Jost's D", {
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

  expect_true("global_dxy" %in% names(fst))
  expect_true(all(c("dxy", "dxy_n_snps") %in% names(fst$long)))
  expect_true(all(fst$long$dxy >= 0 & fst$long$dxy <= 1, na.rm = TRUE))
  expect_true(file.exists(file.path(dirs$tables, "19c_pairwise_dxy_matrix.tsv")))
  global_table <- data.table::fread(file.path(dirs$tables, "17_global_FST.tsv"))
  expect_true("global_dxy" %in% names(global_table))

  ok <- popgenVCF:::validate_fst_result(fst, out$analysis, context)
  expect_true(ok$valid)
})

test_that("validate_fst_result flags Dxy values outside [0, 1]", {
  bad <- list(
    global = 0.1, matrix = matrix(0, 2, 2, dimnames = list(c("A", "B"), c("A", "B"))),
    long = data.table::data.table(
      population_1 = "A", population_2 = "B", fst = 0.1, dxy = 1.5
    )
  )
  out <- popgenVCF:::validate_fst_result(bad, NULL, NULL)
  expect_false(out$valid)
  expect_match(paste(out$errors, collapse = " "), "Dxy")
})
