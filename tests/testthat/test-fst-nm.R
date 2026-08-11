test_that("fst_to_nm implements Wright's (1931) island-model formula", {
  nm <- popgenVCF:::fst_to_nm
  expect_equal(nm(0.05), (1 - 0.05) / (4 * 0.05))
  expect_equal(nm(0.05), 4.75)
  expect_equal(nm(0.25), 0.75)
  expect_equal(nm(1), 0)
})

test_that("fst_to_nm reports Inf for zero or negative FST, not a fabricated finite number", {
  nm <- popgenVCF:::fst_to_nm
  expect_identical(nm(0), Inf)
  expect_identical(nm(-0.01), Inf)
  expect_identical(nm(-5), Inf)
})

test_that("fst_to_nm propagates NA", {
  nm <- popgenVCF:::fst_to_nm
  expect_identical(nm(NA_real_), NA_real_)
  expect_equal(nm(c(0.05, NA_real_, 0)), c(4.75, NA_real_, Inf))
})

test_that("run_fst attaches Nm alongside FST at both the global and pairwise level", {
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

  result <- popgenVCF:::run_fst(gds, snp_ids, metadata)

  expect_true("global_nm" %in% names(result))
  expect_equal(result$global_nm, popgenVCF:::fst_to_nm(result$global))
  expect_true("nm" %in% names(result$long))
  expect_equal(result$long$nm, popgenVCF:::fst_to_nm(result$long$fst))
  expect_true(nrow(result$long) > 0L)
})

test_that("run_fst reports NA global Nm with fewer than two populations", {
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
  one_population <- data.table::copy(metadata)
  one_population[, population := "only"]

  result <- popgenVCF:::run_fst(gds, snp_ids, one_population)

  expect_true(is.na(result$global))
  expect_true(is.na(result$global_nm))
})
