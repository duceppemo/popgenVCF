hwe_private_allele_fixture_gds <- function() {
  sample_id <- c("A1", "A2", "A3", "A4", "B1", "B2", "B3", "B4")
  snp_id <- 1:5
  genmat <- matrix(
    c(
      1, 1, 1, 1, 0, 0, 0, 0, # L1: PopA all het, PopB all hom-ref -> alt private to PopA
      0, 0, 0, 0, 2, 2, 2, 2, # L2: PopA all hom-ref, PopB all hom-alt -> symmetric private
      0, 0, 1, 1, NA, NA, NA, NA, # L3: PopA mixed, PopB fully missing -> "both" private to PopA
      0, 1, 1, 2, 0, 0, 1, 2, # L4: shared between both populations, not private
      0, 0, 0, 0, 0, 0, 0, 0 # L5: monomorphic everywhere
    ),
    nrow = 8, ncol = 5
  )
  gds_path <- tempfile(fileext = ".gds")
  SNPRelate::snpgdsCreateGeno(
    gds_path, genmat = genmat, sample.id = sample_id, snp.id = snp_id,
    snp.chromosome = rep(1L, 5), snp.position = (1:5) * 100L,
    snp.allele = rep("A/G", 5), snpfirstdim = FALSE
  )
  list(path = gds_path, sample_id = sample_id)
}

hwe_private_allele_fixture <- function() {
  built <- hwe_private_allele_fixture_gds()
  gds <- SNPRelate::snpgdsOpen(built$path)
  ids <- popgenVCF:::get_gds_ids(gds)
  metadata <- popgenVCF:::normalize_sample_aliases(data.table::data.table(
    sample = built$sample_id, population = rep(c("PopA", "PopB"), each = 4L)
  ))
  list(gds = gds, ids = ids, sample_id = built$sample_id, metadata = metadata)
}

test_that("compute_diversity detects private alleles, including the all-missing-elsewhere 'both' case", {
  fx <- hwe_private_allele_fixture()
  on.exit(SNPRelate::snpgdsClose(fx$gds), add = TRUE)
  div <- popgenVCF:::compute_diversity(fx$gds, fx$sample_id, fx$ids$snp, fx$metadata, fx$ids)

  by_locus <- function(pop, snp) div$locus[population == pop & snp_id == snp]

  expect_identical(by_locus("PopA", 1L)$private_allele, "alt")
  expect_identical(by_locus("PopB", 1L)$private_allele, "none")
  expect_identical(by_locus("PopA", 2L)$private_allele, "ref")
  expect_identical(by_locus("PopB", 2L)$private_allele, "alt")
  expect_identical(by_locus("PopA", 3L)$private_allele, "both")
  expect_identical(by_locus("PopB", 3L)$private_allele, "none")
  expect_identical(by_locus("PopA", 4L)$private_allele, "none")
  expect_identical(by_locus("PopB", 4L)$private_allele, "none")
  expect_identical(by_locus("PopA", 5L)$private_allele, "none")
  expect_identical(by_locus("PopB", 5L)$private_allele, "none")

  expect_identical(div$population[population == "PopA", private_allele_loci], 3L)
  expect_identical(div$population[population == "PopB", private_allele_loci], 1L)
})

test_that("compute_diversity reports NA HWE p-values for loci monomorphic within a population", {
  fx <- hwe_private_allele_fixture()
  on.exit(SNPRelate::snpgdsClose(fx$gds), add = TRUE)
  div <- popgenVCF:::compute_diversity(fx$gds, fx$sample_id, fx$ids$snp, fx$metadata, fx$ids)

  by_locus <- function(pop, snp) div$locus[population == pop & snp_id == snp]

  expect_true(is.na(by_locus("PopB", 1L)$hwe_pvalue))
  expect_true(is.na(by_locus("PopA", 2L)$hwe_pvalue))
  expect_true(is.na(by_locus("PopA", 5L)$hwe_pvalue))
  expect_true(is.na(by_locus("PopB", 5L)$hwe_pvalue))
  expect_true(is.na(by_locus("PopB", 3L)$hwe_pvalue))

  expect_equal(by_locus("PopA", 1L)$hwe_pvalue, 0.3142857, tolerance = 1e-6)
  expect_equal(by_locus("PopA", 3L)$hwe_pvalue, 1)
  expect_equal(by_locus("PopA", 4L)$hwe_pvalue, 1)
  expect_equal(by_locus("PopB", 4L)$hwe_pvalue, 0.4285714, tolerance = 1e-6)

  expect_identical(div$population[population == "PopA", hwe_tested_loci], 3L)
  expect_identical(div$population[population == "PopB", hwe_tested_loci], 1L)
  expect_identical(div$population[population == "PopA", hwe_significant_loci], 0L)
  expect_identical(div$population[population == "PopA", hwe_significant_loci_fdr], 0L)
})

test_that("compute_diversity respects a custom hwe_alpha threshold", {
  fx <- hwe_private_allele_fixture()
  on.exit(SNPRelate::snpgdsClose(fx$gds), add = TRUE)
  div <- popgenVCF:::compute_diversity(fx$gds, fx$sample_id, fx$ids$snp, fx$metadata, fx$ids, hwe_alpha = 0.5)

  expect_identical(div$population[population == "PopA", hwe_significant_loci], 1L)
})

test_that("compute_diversity leaves private_allele as NA with fewer than two populations", {
  fx <- hwe_private_allele_fixture()
  on.exit(SNPRelate::snpgdsClose(fx$gds), add = TRUE)
  one_pop_metadata <- data.table::copy(fx$metadata)
  one_pop_metadata[, population := "PopA"]
  div <- popgenVCF:::compute_diversity(fx$gds, fx$sample_id, fx$ids$snp, one_pop_metadata, fx$ids)

  expect_true(all(is.na(div$locus$private_allele)))
  expect_identical(div$population$private_allele_loci, 0L)
})
