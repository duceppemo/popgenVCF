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

test_that("ld_prune_exact's ld_r2/slide_max_bp/slide_max_n/start_pos arguments really reach snpgdsLDpruning (not silently ignored)", {
  captured <- NULL
  testthat::local_mocked_bindings(
    snpgdsLDpruning = function(gdsobj, ..., ld.threshold, slide.max.bp, slide.max.n, start.pos) {
      captured <<- list(
        ld.threshold = ld.threshold, slide.max.bp = slide.max.bp,
        slide.max.n = slide.max.n, start.pos = start.pos
      )
      list(chr1 = 1L)
    },
    .package = "SNPRelate"
  )
  gds_placeholder <- structure(list(), class = "gds.class")
  popgenVCF:::ld_prune_exact(
    gds_placeholder, c("s1", "s2"), maf_threshold = 0.05, threads = 1L, seed = 42L,
    ld_r2 = 0.7, slide_max_bp = 250000, slide_max_n = 200L, start_pos = "random"
  )
  expect_equal(captured$ld.threshold, sqrt(0.7))
  expect_equal(captured$slide.max.n, 200L)
  expect_identical(captured$start.pos, "random")
  expect_true(is.finite(captured$slide.max.bp) && captured$slide.max.bp > 0)
})

# A second, more severe real regression chromosome Y exposed (found while
# adding a real chromosome Y demo dataset): a chromosome only one sex has
# by biology (e.g. human chromosome Y) is ~100% "missing" in the other sex.
# Both harmonize_samples()'s per-sample missingness QC and variant_qc()'s
# per-SNP missingness QC treat that as a data-quality problem unless told
# otherwise -- the former silently drops every sample of the unaffected sex
# out of the entire pipeline, the latter fails every chromosome Y SNP out
# of QC entirely, leaving sex_check with zero chromosome Y markers.
sex_limited_fixture_gds <- function(n_female = 10L, n_male = 10L, n_autosomal = 20L, n_ylike = 15L, seed = 3L) {
  set.seed(seed)
  n_samples <- n_female + n_male
  p_auto <- stats::runif(n_autosomal, 0.2, 0.8)
  geno_auto <- vapply(p_auto, function(pi) stats::rbinom(n_samples, 2L, pi), integer(n_samples))

  p_y <- stats::runif(n_ylike, 0.2, 0.8)
  geno_y_male <- vapply(p_y, function(pi) stats::rbinom(n_male, 2L, pi), integer(n_male))
  geno_y_female <- matrix(3L, nrow = n_female, ncol = n_ylike) # 3 = missing, not 0/1/2
  geno_y <- rbind(geno_y_female, geno_y_male) # females first, matching sample_id order below

  genmat <- t(cbind(geno_auto, geno_y))
  sample_id <- c(paste0("F", seq_len(n_female)), paste0("M", seq_len(n_male)))
  n_snps <- n_autosomal + n_ylike
  snp_id <- seq_len(n_snps)
  chromosome <- c(rep("1", n_autosomal), rep("Y", n_ylike))
  position <- c(seq_len(n_autosomal) * 100L, seq_len(n_ylike) * 100L)

  gds_path <- tempfile(fileext = ".gds")
  SNPRelate::snpgdsCreateGeno(
    gds_path, genmat = genmat, sample.id = sample_id, snp.id = snp_id,
    snp.chromosome = chromosome, snp.position = position,
    snp.allele = rep("A/G", n_snps), snpfirstdim = TRUE
  )
  list(path = gds_path, sample_id = sample_id, n_autosomal = n_autosomal, n_ylike = n_ylike)
}

test_that("variant_qc exempts sex-limited chromosomes from the missingness threshold, MAF unaffected", {
  fx <- sex_limited_fixture_gds()
  gds <- SNPRelate::snpgdsOpen(fx$path)
  on.exit(SNPRelate::snpgdsClose(gds), add = TRUE)
  ids <- popgenVCF:::get_gds_ids(gds)

  without_exemption <- popgenVCF:::variant_qc(gds, fx$sample_id, ids, maf_threshold = 0.05, max_missing = 0.2)
  y_rows <- without_exemption[ids$chromosome[match(without_exemption$snp_id, ids$snp)] == "Y"]
  expect_true(all(!y_rows$pass_missing))
  expect_true(all(!y_rows$pass_combined))

  with_exemption <- popgenVCF:::variant_qc(
    gds, fx$sample_id, ids, maf_threshold = 0.05, max_missing = 0.2,
    sex_limited_chromosome_names = c("Y", "chrY")
  )
  y_rows2 <- with_exemption[ids$chromosome[match(with_exemption$snp_id, ids$snp)] == "Y"]
  expect_true(all(y_rows2$pass_missing))
  # MAF is unaffected by the exemption; it is still computed and can still
  # fail a Y SNP on its own terms.
  expect_identical(y_rows2$pass_combined, y_rows2$pass_maf)

  auto_rows <- with_exemption[ids$chromosome[match(with_exemption$snp_id, ids$snp)] == "1"]
  auto_rows_noex <- without_exemption[ids$chromosome[match(without_exemption$snp_id, ids$snp)] == "1"]
  expect_identical(auto_rows$pass_combined, auto_rows_noex$pass_combined)
})

test_that("harmonize_samples restricts per-sample missingness to a supplied SNP set", {
  fx <- sex_limited_fixture_gds()
  gds <- SNPRelate::snpgdsOpen(fx$path)
  on.exit(SNPRelate::snpgdsClose(gds), add = TRUE)
  ids <- popgenVCF:::get_gds_ids(gds)
  metadata <- popgenVCF:::metadata_from_samples(fx$sample_id)

  without_restriction <- popgenVCF:::harmonize_samples(gds, ids, metadata, max_missing = 0.2)
  # Females are ~100% missing across the Y-like SNPs, which are half of
  # every sample's total markers here -- enough to fail the default 0.2
  # missingness threshold and be dropped entirely, the real regression.
  expect_lt(length(without_restriction$sample_ids), length(fx$sample_id))
  expect_true(all(grepl("^M", without_restriction$sample_ids)))

  autosomal_only <- ids$snp[ids$chromosome == "1"]
  with_restriction <- popgenVCF:::harmonize_samples(
    gds, ids, metadata, max_missing = 0.2, snp_ids = autosomal_only
  )
  expect_identical(length(with_restriction$sample_ids), length(fx$sample_id))
  expect_setequal(with_restriction$sample_ids, fx$sample_id)
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
  # The real, more severe regression chromosome Y exposed: chromosome Y has
  # zero calls at all for one whole sex by biology, not by data-quality
  # problem. Before this fix, that pooled into (a) per-sample missingness
  # QC (harmonize_samples()), silently dropping every sample of the
  # unaffected sex out of the entire pipeline, and (b) per-SNP missingness
  # QC (variant_qc()), failing every chromosome Y SNP out of qc_snps_all
  # and leaving sex_check with zero chromosome Y markers.
  expect_identical(length(analysis$samples$ids), 160L)
  expect_gt(sex_check$n_y_snps, 0L)
  expect_true(all(!is.na(sex_check$table$y_call_rate)))
  # Real signal: combining X and Y resolves most of X-alone's "ambiguous"
  # calls, and flags any confident X/Y disagreement as "discordant" rather
  # than silently picking one -- both real, expected outcomes on this data.
  expect_true("discordant" %in% sex_check$table$status)
  expect_false("ambiguous" %in% sex_check$table$status)
})
