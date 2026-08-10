allelic_richness_fixture <- function() {
  sample_id <- c("A1", "A2", "A3", "A4", "B1", "B2", "B3", "B4")
  snp_id <- 1:5
  genmat <- matrix(
    c(
      1, 1, 1, 1, 0, 0, 0, 0, # L1: PopA het/het, PopB hom-ref -> PopA has both alleles, PopB one
      0, 0, 0, 0, 2, 2, 2, 2, # L2: PopA hom-ref, PopB hom-alt -> each population has one allele
      0, 0, 1, 1, NA, NA, NA, NA, # L3: PopA mixed, PopB fully missing
      0, 1, 1, 2, 0, 0, 1, 2, # L4: both alleles present in both populations
      0, 0, 0, 0, 0, 0, 0, 0 # L5: monomorphic everywhere -> one allele
    ),
    nrow = 8, ncol = 5
  )
  gds_path <- tempfile(fileext = ".gds")
  SNPRelate::snpgdsCreateGeno(
    gds_path, genmat = genmat, sample.id = sample_id, snp.id = snp_id,
    snp.chromosome = rep(1L, 5), snp.position = (1:5) * 100L,
    snp.allele = rep("A/G", 5), snpfirstdim = FALSE
  )
  gds <- SNPRelate::snpgdsOpen(gds_path)
  ids <- popgenVCF:::get_gds_ids(gds)
  metadata <- popgenVCF:::normalize_sample_aliases(data.table::data.table(
    sample = sample_id, population = rep(c("PopA", "PopB"), each = 4L)
  ))
  list(gds = gds, ids = ids, sample_id = sample_id, metadata = metadata)
}

test_that("compute_diversity computes rarefied allelic richness when hierfstat is installed", {
  skip_if_not_installed("hierfstat")
  fx <- allelic_richness_fixture()
  on.exit(SNPRelate::snpgdsClose(fx$gds), add = TRUE)
  div <- popgenVCF:::compute_diversity(fx$gds, fx$sample_id, fx$ids$snp, fx$metadata, fx$ids)

  expect_true(div$allelic_richness_available)
  expect_identical(div$allelic_richness_min_alleles, 8)

  by_locus <- function(pop, snp) div$locus[population == pop & snp_id == snp, allelic_richness]
  # Biallelic SNPs: 2 when both alleles are present in the population, 1 when
  # monomorphic within it, NA when the population has no calls at all.
  expect_identical(by_locus("PopA", 1L), 2)
  expect_identical(by_locus("PopB", 1L), 1)
  expect_identical(by_locus("PopA", 2L), 1)
  expect_identical(by_locus("PopB", 2L), 1)
  expect_identical(by_locus("PopA", 3L), 2)
  expect_true(is.na(by_locus("PopB", 3L)))
  expect_identical(by_locus("PopA", 4L), 2)
  expect_identical(by_locus("PopB", 4L), 2)
  expect_identical(by_locus("PopA", 5L), 1)
  expect_identical(by_locus("PopB", 5L), 1)

  expect_equal(div$population[population == "PopA", mean_allelic_richness], 1.6)
  expect_equal(div$population[population == "PopB", mean_allelic_richness], 1.25)
})

test_that("compute_diversity skips allelic richness transparently when hierfstat is unavailable", {
  fx <- allelic_richness_fixture()
  on.exit(SNPRelate::snpgdsClose(fx$gds), add = TRUE)
  local_mocked_bindings(requireNamespace = function(...) FALSE, .package = "base")
  div <- popgenVCF:::compute_diversity(fx$gds, fx$sample_id, fx$ids$snp, fx$metadata, fx$ids)

  expect_false(div$allelic_richness_available)
  expect_true(is.na(div$allelic_richness_min_alleles))
  expect_true(all(is.na(div$locus$allelic_richness)))
  expect_true(all(is.na(div$population$mean_allelic_richness)))
})

test_that("validate_diversity_result flags allelic richness above the biallelic maximum", {
  fx <- allelic_richness_fixture()
  on.exit(SNPRelate::snpgdsClose(fx$gds), add = TRUE)
  div <- popgenVCF:::compute_diversity(fx$gds, fx$sample_id, fx$ids$snp, fx$metadata, fx$ids)
  ok <- popgenVCF:::validate_diversity_result(div, NULL, NULL)
  expect_true(ok$valid)

  bad <- div
  bad$locus <- data.table::copy(div$locus)
  bad$locus[1L, allelic_richness := 2.5]
  broken <- popgenVCF:::validate_diversity_result(bad, NULL, NULL)
  expect_false(broken$valid)
  expect_true(any(grepl("allelic_richness", broken$errors)))
})

test_that("run_module_diversity records a WARNING message when allelic richness is unavailable", {
  fx <- allelic_richness_fixture()
  on.exit(SNPRelate::snpgdsClose(fx$gds), add = TRUE)
  local_mocked_bindings(requireNamespace = function(...) FALSE, .package = "base")

  cfg <- popgenVCF::default_config()
  cfg$analyses$bootstrap$enabled <- FALSE
  dirs <- list(tables = tempfile("tables-"), figures = tempfile("figures-"))
  dir.create(dirs$tables); dir.create(dirs$figures)
  cfg$output$figure_formats <- "png"
  analysis <- popgenVCF:::new_popgen_vcf_analysis(cfg)
  context <- list(
    cfg = cfg, dirs = dirs, gds = fx$gds, sample_ids = fx$sample_id,
    qc_snps = fx$ids$snp, metadata = fx$metadata, ids = fx$ids
  )
  out <- popgenVCF:::run_module_diversity(analysis, context)
  messages <- out$analysis$messages
  expect_true(any(
    messages$level == "WARNING" & messages$stage == "diversity" &
      grepl("hierfstat", messages$message, fixed = TRUE)
  ))
})
