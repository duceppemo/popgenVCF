# Real hemizygous-vs-diploid ground truth, not a hand-picked F value: males
# are simulated as homozygous-only X calls (2*rbinom(n,1,p), i.e. never
# genotype 1, matching how a diploid VCF caller represents a single X copy),
# females as standard HWE diploid calls (rbinom(n,2,p)). Verified once
# empirically before writing this fixture: 15 simulated males gave F in
# [0.84, 1.13], 15 simulated females gave F in [-0.06, 0.10] -- a clean
# separation either side of PLINK's own --check-sex defaults (male > 0.8,
# female < 0.2), confirming SNPRelate::snpgdsIndInb(method="mom.visscher")
# restricted to X SNPs is the right tool, not hand-rolled math.
sex_check_fixture_gds <- function(n_female = 15L, n_male = 15L, n_snps = 200L, seed = 1L) {
  set.seed(seed)
  p <- stats::runif(n_snps, 0.1, 0.9)
  geno_female <- vapply(p, function(pi) stats::rbinom(n_female, 2L, pi), integer(n_female))
  geno_male <- vapply(p, function(pi) 2L * stats::rbinom(n_male, 1L, pi), integer(n_male))
  genmat <- t(rbind(geno_female, geno_male))
  sample_id <- c(paste0("F", seq_len(n_female)), paste0("M", seq_len(n_male)))
  snp_id <- seq_len(n_snps)

  gds_path <- tempfile(fileext = ".gds")
  SNPRelate::snpgdsCreateGeno(
    gds_path, genmat = genmat, sample.id = sample_id, snp.id = snp_id,
    snp.chromosome = rep("X", n_snps), snp.position = seq_len(n_snps) * 100L,
    snp.allele = rep("A/G", n_snps), snpfirstdim = TRUE
  )
  list(path = gds_path, sample_id = sample_id, true_sex = ifelse(grepl("^M", sample_id), "male", "female"))
}

sex_check_fixture <- function(metadata_sex = NULL, ...) {
  built <- sex_check_fixture_gds(...)
  gds <- SNPRelate::snpgdsOpen(built$path)
  metadata <- data.table::data.table(sample = built$sample_id)
  if (!is.null(metadata_sex)) metadata$sex <- metadata_sex
  metadata <- popgenVCF:::normalize_sample_aliases(metadata)
  list(gds = gds, sample_id = built$sample_id, metadata = metadata, true_sex = built$true_sex)
}

test_that("normalize_sex_label recognizes standard synonyms and rejects unknown values", {
  out <- popgenVCF:::normalize_sex_label(c("Male", "M", "1", "FEMALE", "f", "2", "unknown", "", NA))
  expect_identical(out, c("male", "male", "male", "female", "female", "female", NA, NA, NA))
})

test_that("run_sex_check recovers known simulated male/female genotypes from X heterozygosity", {
  fx <- sex_check_fixture(metadata_sex = NULL)
  on.exit(SNPRelate::snpgdsClose(fx$gds), add = TRUE)
  ids <- popgenVCF:::get_gds_ids(fx$gds)

  result <- popgenVCF:::run_sex_check(
    fx$gds, fx$sample_id, ids$snp, ids, fx$metadata,
    x_chromosome_names = c("X", "chrX"), male_threshold = 0.8, female_threshold = 0.2
  )
  expect_false(is.null(result))
  expect_identical(result$n_x_snps, 200L)
  table <- result$table
  expect_identical(nrow(table), length(fx$sample_id))
  expect_setequal(names(table), c(
    "sample", "vcf_sample", "n_x_snps", "x_heterozygosity_F",
    "inferred_sex", "reported_sex", "status"
  ))
  ordered <- table[match(fx$sample_id, table$vcf_sample), ]
  expect_identical(ordered$inferred_sex, fx$true_sex)
  # No metadata sex column supplied: comparison columns are present but inert.
  expect_true(all(is.na(table$reported_sex)))
  expect_true(all(is.na(table$status)))
})

test_that("run_sex_check flags mismatches against a supplied sex metadata column", {
  fx <- sex_check_fixture(metadata_sex = NULL)
  on.exit(SNPRelate::snpgdsClose(fx$gds), add = TRUE)
  ids <- popgenVCF:::get_gds_ids(fx$gds)

  # Deliberately mislabel one female as male and one male as female in the
  # metadata; everyone else is reported correctly.
  reported <- fx$true_sex
  reported[fx$sample_id == "F1"] <- "male"
  reported[fx$sample_id == "M1"] <- "female"
  metadata <- data.table::data.table(sample = fx$sample_id, sex = reported)
  metadata <- popgenVCF:::normalize_sample_aliases(metadata)

  result <- popgenVCF:::run_sex_check(
    fx$gds, fx$sample_id, ids$snp, ids, metadata,
    x_chromosome_names = c("X", "chrX"), male_threshold = 0.8, female_threshold = 0.2
  )
  table <- result$table
  by_sample <- stats::setNames(table$status, table$vcf_sample)
  expect_identical(unname(by_sample["F1"]), "mismatch")
  expect_identical(unname(by_sample["M1"]), "mismatch")
  expect_identical(unname(by_sample["F2"]), "match")
  expect_identical(unname(by_sample["M2"]), "match")
  expect_identical(sum(table$status == "mismatch"), 2L)
})

test_that("run_sex_check skips transparently when too few X-chromosome SNPs are available", {
  fx <- sex_check_fixture(metadata_sex = NULL, n_snps = 5L)
  on.exit(SNPRelate::snpgdsClose(fx$gds), add = TRUE)
  ids <- popgenVCF:::get_gds_ids(fx$gds)

  result <- popgenVCF:::run_sex_check(
    fx$gds, fx$sample_id, ids$snp, ids, fx$metadata,
    x_chromosome_names = c("X", "chrX"), male_threshold = 0.8, female_threshold = 0.2
  )
  expect_null(result)
})

test_that("run_sex_check skips transparently when the VCF has no X-named chromosome", {
  fx <- sex_check_fixture(metadata_sex = NULL)
  on.exit(SNPRelate::snpgdsClose(fx$gds), add = TRUE)
  ids <- popgenVCF:::get_gds_ids(fx$gds)

  result <- popgenVCF:::run_sex_check(
    fx$gds, fx$sample_id, ids$snp, ids, fx$metadata,
    x_chromosome_names = c("chr23_not_present"), male_threshold = 0.8, female_threshold = 0.2
  )
  expect_null(result)
})

test_that("plot_sex_check draws a per-sample F-statistic figure and is silent with nothing to plot", {
  plots <- list()
  local_mocked_bindings(
    save_plot = function(p, stem, ...) {
      plots[[stem]] <<- p
      invisible(TRUE)
    },
    .package = "popgenVCF"
  )
  fx <- sex_check_fixture(metadata_sex = NULL)
  on.exit(SNPRelate::snpgdsClose(fx$gds), add = TRUE)
  ids <- popgenVCF:::get_gds_ids(fx$gds)
  result <- popgenVCF:::run_sex_check(
    fx$gds, fx$sample_id, ids$snp, ids, fx$metadata,
    x_chromosome_names = c("X", "chrX"), male_threshold = 0.8, female_threshold = 0.2
  )
  cfg <- list(
    output = list(figure_formats = "pdf", dpi = 150L),
    analyses = list(sex_check_male_f_threshold = 0.8, sex_check_female_f_threshold = 0.2)
  )
  dirs <- list(figures = tempdir())
  popgenVCF:::plot_sex_check(result, cfg, dirs)
  expect_true("27_sex_check_F_by_sample" %in% names(plots))
  p <- plots[["27_sex_check_F_by_sample"]]
  expect_identical(deparse(p$labels$y), "expression(italic(F))")

  plots2 <- list()
  local_mocked_bindings(
    save_plot = function(p, stem, ...) {
      plots2[[stem]] <<- p
      invisible(TRUE)
    },
    .package = "popgenVCF"
  )
  empty_result <- list(table = data.table::data.table(), n_x_snps = 0L)
  popgenVCF:::plot_sex_check(empty_result, cfg, dirs)
  expect_length(plots2, 0L)
})
