# Real, empirically-confirmed motivation (not hypothetical): concatenating a
# bounded chromosome X region into the quickstart dataset's kinship/PCA/DAPC
# marker pool collapsed a known real duplicate/MZ-twin pair's kinship
# (NA19331/NA19334, 0.4459) and a real high-kinship pair's (HG03873/HG03998,
# 0.4525) down to near zero -- KING-robust's denominator depends on each
# sample's own heterozygosity count, and a hemizygous male has zero
# heterozygosity on X by construction, so pooling autosomal and
# male-hemizygous X markers in the same estimate is not just imprecise, it
# is wrong. Standard tools (PLINK, KING, GCTA) exclude sex chromosomes from
# genome-wide kinship/PCA/GRM by default for exactly this reason.

test_that("ld_prune_exact restricts pruning to a supplied candidate SNP set", {
  n_samples <- 20L
  n_snps <- 40L
  base <- popgenVCF:::synthetic_genotypes(samples = n_samples, snps = n_snps, seed = 11L)
  snp_id <- seq_len(n_snps)
  gds_path <- tempfile(fileext = ".gds")
  SNPRelate::snpgdsCreateGeno(
    gds_path, genmat = base, sample.id = rownames(base), snp.id = snp_id,
    snp.chromosome = rep(c("1", "X"), each = n_snps / 2L),
    snp.position = rep(seq_len(n_snps / 2L) * 100L, 2L),
    snp.allele = rep("A/G", n_snps), snpfirstdim = FALSE
  )
  gds <- SNPRelate::snpgdsOpen(gds_path)
  on.exit(SNPRelate::snpgdsClose(gds), add = TRUE)

  autosomal_candidates <- snp_id[seq_len(n_snps / 2L)]
  pruned <- popgenVCF:::ld_prune_exact(
    gds, rownames(base), maf_threshold = 0.05, threads = 1L, seed = 42L,
    snp_ids = autosomal_candidates
  )
  expect_true(all(pruned %in% autosomal_candidates))
  expect_true(length(pruned) > 0L)

  pruned_all <- popgenVCF:::ld_prune_exact(
    gds, rownames(base), maf_threshold = 0.05, threads = 1L, seed = 42L
  )
  expect_true(any(!pruned_all %in% autosomal_candidates))
})

# A real bug caught while writing this config: YAML 1.1 (what the `yaml`
# package parses) treats an unquoted "Y" as a boolean literal, not the
# string "Y" -- inst/example_config.yml's non_autosomal_chromosome_names
# list silently corrupted "Y" into `TRUE` (round-tripping back to the string
# "TRUE") until "Y" was explicitly quoted. Pinned here so it cannot recur.
test_that("inst/example_config.yml parses non_autosomal_chromosome_names without YAML boolean corruption", {
  cfg <- popgenVCF::read_config(
    system.file("example_config.yml", package = "popgenVCF", mustWork = TRUE)
  )
  expect_setequal(
    cfg$qc$non_autosomal_chromosome_names,
    c("X", "Y", "MT", "M", "chrX", "chrY", "chrM", "chrMT")
  )
})

test_that("default_config enables autosome_only with the documented non-autosomal names", {
  cfg <- popgenVCF::default_config()
  expect_true(cfg$qc$autosome_only)
  expect_setequal(
    cfg$qc$non_autosomal_chromosome_names,
    c("X", "Y", "MT", "M", "chrX", "chrY", "chrM", "chrMT")
  )
})

test_that("validate_config coerces non_autosomal_chromosome_names to character", {
  cfg <- popgenVCF::default_config()
  cfg$input$vcf <- tempfile(fileext = ".vcf")
  cfg$output$directory <- tempfile("popgenvcf-output-")
  file.create(cfg$input$vcf)
  cfg$qc$non_autosomal_chromosome_names <- c("X", "Y")
  validated <- popgenVCF::validate_config(cfg)
  expect_identical(validated$qc$non_autosomal_chromosome_names, c("X", "Y"))
})

test_that("run_pipeline excludes non-autosomal SNPs from qc_ids/final_snps by default, and sex_check still sees them", {
  skip_if(Sys.which("bcftools") == "", "bcftools is not available")
  pg_env <- popgenVCF:::.pg_env
  on.exit(pg_env$log_file <- NULL, add = TRUE)
  paths <- popgenVCF::quickstart_dataset_paths()
  root <- withr::local_tempdir()

  cfg <- popgenVCF::default_config()
  cfg$input$vcf <- paths$vcf
  cfg$input$metadata <- paths$metadata
  cfg$output$directory <- root
  cfg$compute$threads <- max(1L, parallel::detectCores() - 1L)
  cfg$analyses$dapc_cross_validation <- FALSE
  cfg$analyses$dapc_k <- "2:2"
  cfg$analyses$bootstrap$enabled <- FALSE
  cfg$analyses$structure$replicates <- 1L
  cfg$report$enabled <- FALSE

  analysis <- popgenVCF::run_pipeline(cfg)
  expect_identical(analysis$status, "complete")

  ids <- popgenVCF:::get_gds_ids(popgenVCF:::prepare_gds(paths$vcf, tempfile(fileext = ".gds")))
  qc_ids <- analysis$variants$qc_ids
  qc_ids_all <- analysis$variants$qc_ids_all
  expect_true(length(qc_ids_all) > length(qc_ids))
  expect_false(any(ids$chromosome[match(qc_ids, ids$snp)] == "X"))
  expect_true(any(ids$chromosome[match(qc_ids_all, ids$snp)] == "X"))
  expect_false(any(ids$chromosome[match(analysis$variants$ld_ids, ids$snp)] == "X"))

  # The real regression this fix addresses: with chromosome X pooled out of
  # the kinship marker set, both real high-kinship pairs are correctly
  # detected again (they collapse to ~0 kinship if X markers are pooled in).
  # NA19331/NA19334 is a confirmed real duplicate/MZ twin; HG03873/HG03998
  # is a real high-kinship pair that is NOT actually a duplicate/twin
  # (chromosome X later proves they are genuinely different sexes) -- kept
  # here purely as the real fixture that demonstrates the bug/fix, not as a
  # claim about their biological relationship.
  kinship <- popgenVCF:::get_analysis_result(analysis, "kinship")
  pairs <- kinship$close_relatives
  ho_pair <- pairs[(sample_1 == "HG03873" & sample_2 == "HG03998") |
                      (sample_1 == "HG03998" & sample_2 == "HG03873")]
  lwk_pair <- pairs[(sample_1 == "NA19331" & sample_2 == "NA19334") |
                       (sample_1 == "NA19334" & sample_2 == "NA19331")]
  expect_identical(nrow(ho_pair), 1L)
  expect_identical(nrow(lwk_pair), 1L)
  expect_gt(ho_pair$kinship, 0.35)
  expect_gt(lwk_pair$kinship, 0.35)

  sex_check <- popgenVCF:::get_analysis_result(analysis, "sex_check")
  expect_false(is.null(sex_check))
  expect_gt(sex_check$n_x_snps, 0L)
})
