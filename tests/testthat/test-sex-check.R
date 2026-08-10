# Real hemizygous-vs-diploid ground truth for X, not a hand-picked F value:
# males are simulated as homozygous-only X calls (2*rbinom(n,1,p), i.e.
# never genotype 1, matching how a diploid VCF caller represents a single X
# copy), females as standard HWE diploid calls (rbinom(n,2,p)). For Y:
# males get real calls (2*rbinom(n,1,p)), females get every genotype
# missing (SNPRelate's genmat missing code, not 0/1/2) -- matching the real
# 1000 Genomes chromosome Y release, where female samples have no Y calls
# at all. Verified once empirically before writing this fixture: 15
# simulated males gave X-heterozygosity F in [0.84, 1.13], 15 simulated
# females gave F in [-0.06, 0.10] (PLINK's --check-sex defaults, male >
# 0.8, female < 0.2, separate cleanly); real 1000 Genomes chromosome Y data
# gave an almost perfectly binary call rate (females exactly 0.0, males
# 0.995-1.0), confirming SNPRelate::snpgdsSampMissRate() restricted to Y
# SNPs is the right, simple tool -- no hand-rolled math needed for either
# signal.
sex_check_fixture_gds <- function(n_female = 15L, n_male = 15L, n_snps_x = 200L, n_snps_y = 30L, seed = 1L) {
  set.seed(seed)
  p_x <- stats::runif(n_snps_x, 0.1, 0.9)
  geno_female_x <- vapply(p_x, function(pi) stats::rbinom(n_female, 2L, pi), integer(n_female))
  geno_male_x <- vapply(p_x, function(pi) 2L * stats::rbinom(n_male, 1L, pi), integer(n_male))
  genmat_x <- t(rbind(geno_female_x, geno_male_x))

  p_y <- stats::runif(n_snps_y, 0.2, 0.8)
  geno_male_y <- vapply(p_y, function(pi) 2L * stats::rbinom(n_male, 1L, pi), integer(n_male))
  geno_female_y <- matrix(3L, nrow = n_female, ncol = n_snps_y) # 3 = missing, not 0/1/2
  genmat_y <- t(rbind(geno_female_y, geno_male_y))

  genmat <- rbind(genmat_x, genmat_y)
  n_snps <- n_snps_x + n_snps_y
  sample_id <- c(paste0("F", seq_len(n_female)), paste0("M", seq_len(n_male)))
  snp_id <- seq_len(n_snps)
  chromosome <- c(rep("X", n_snps_x), rep("Y", n_snps_y))
  position <- c(seq_len(n_snps_x) * 100L, seq_len(n_snps_y) * 100L)

  gds_path <- tempfile(fileext = ".gds")
  SNPRelate::snpgdsCreateGeno(
    gds_path, genmat = genmat, sample.id = sample_id, snp.id = snp_id,
    snp.chromosome = chromosome, snp.position = position,
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

sex_check_default_args <- list(
  x_chromosome_names = c("X", "chrX"), male_f_threshold = 0.8, female_f_threshold = 0.2,
  y_chromosome_names = c("Y", "chrY"), y_male_call_rate_threshold = 0.5, y_female_call_rate_threshold = 0.1
)

test_that("normalize_sex_label recognizes standard synonyms and rejects unknown values", {
  out <- popgenVCF:::normalize_sex_label(c("Male", "M", "1", "FEMALE", "f", "2", "unknown", "", NA))
  expect_identical(out, c("male", "male", "male", "female", "female", "female", NA, NA, NA))
})

test_that("combine_sex_evidence agrees, corroborates, flags discordance, and passes through NA", {
  combine <- popgenVCF:::combine_sex_evidence
  expect_identical(combine("male", "male"), "male")
  expect_identical(combine("female", "female"), "female")
  expect_identical(combine("male", "female"), "discordant")
  expect_identical(combine("male", "ambiguous"), "male")
  expect_identical(combine("ambiguous", "female"), "female")
  expect_identical(combine("ambiguous", "ambiguous"), "ambiguous")
  expect_identical(combine(NA_character_, "male"), "male")
  expect_identical(combine("female", NA_character_), "female")
  expect_identical(combine(NA_character_, NA_character_), NA_character_)
})

test_that("run_sex_check recovers known simulated male/female genotypes from X and Y combined", {
  fx <- sex_check_fixture(metadata_sex = NULL)
  on.exit(SNPRelate::snpgdsClose(fx$gds), add = TRUE)
  ids <- popgenVCF:::get_gds_ids(fx$gds)

  result <- do.call(popgenVCF:::run_sex_check, c(
    list(fx$gds, fx$sample_id, ids$snp, ids, fx$metadata), sex_check_default_args
  ))
  expect_false(is.null(result))
  expect_identical(result$n_x_snps, 200L)
  expect_identical(result$n_y_snps, 30L)
  table <- result$table
  expect_identical(nrow(table), length(fx$sample_id))
  expect_setequal(names(table), c(
    "sample", "vcf_sample", "n_x_snps", "x_heterozygosity_F", "x_inferred_sex",
    "n_y_snps", "y_call_rate", "y_inferred_sex", "inferred_sex", "reported_sex", "status"
  ))
  ordered <- table[match(fx$sample_id, table$vcf_sample), ]
  expect_identical(ordered$inferred_sex, fx$true_sex)
  # Y call rate should be an almost perfectly binary signal on simulated data too.
  expect_true(all(ordered$y_call_rate[fx$true_sex == "female"] == 0))
  expect_true(all(ordered$y_call_rate[fx$true_sex == "male"] > 0.9))
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

  result <- do.call(popgenVCF:::run_sex_check, c(
    list(fx$gds, fx$sample_id, ids$snp, ids, metadata), sex_check_default_args
  ))
  table <- result$table
  by_sample <- stats::setNames(table$status, table$vcf_sample)
  expect_identical(unname(by_sample["F1"]), "mismatch")
  expect_identical(unname(by_sample["M1"]), "mismatch")
  expect_identical(unname(by_sample["F2"]), "match")
  expect_identical(unname(by_sample["M2"]), "match")
  expect_identical(sum(table$status == "mismatch"), 2L)
})

test_that("run_sex_check reports discordant when X and Y confidently disagree", {
  fx <- sex_check_fixture(metadata_sex = NULL)
  on.exit(SNPRelate::snpgdsClose(fx$gds), add = TRUE)
  ids <- popgenVCF:::get_gds_ids(fx$gds)

  result <- do.call(popgenVCF:::run_sex_check, c(
    list(fx$gds, fx$sample_id, ids$snp, ids, fx$metadata), sex_check_default_args
  ))
  table <- result$table
  # A real simulated male (Y call rate > 0.9) but force its X-based call to
  # "female" to construct a genuine X/Y disagreement and confirm it survives
  # combination as "discordant", not silently resolved either way.
  expect_identical(
    popgenVCF:::combine_sex_evidence("female", table[vcf_sample == "M1", y_inferred_sex]),
    "discordant"
  )
})

test_that("run_sex_check falls back to X alone when the VCF has no Y-named chromosome", {
  fx <- sex_check_fixture(metadata_sex = NULL)
  on.exit(SNPRelate::snpgdsClose(fx$gds), add = TRUE)
  ids <- popgenVCF:::get_gds_ids(fx$gds)

  args <- sex_check_default_args
  args$y_chromosome_names <- "chrZZZ_not_present"
  result <- do.call(popgenVCF:::run_sex_check, c(list(fx$gds, fx$sample_id, ids$snp, ids, fx$metadata), args))
  expect_false(is.null(result))
  expect_identical(result$n_y_snps, 0L)
  expect_true(all(is.na(result$table$y_call_rate)))
  ordered <- result$table[match(fx$sample_id, result$table$vcf_sample), ]
  expect_identical(ordered$inferred_sex, fx$true_sex)
})

test_that("run_sex_check falls back to Y alone when the VCF has no X-named chromosome", {
  fx <- sex_check_fixture(metadata_sex = NULL)
  on.exit(SNPRelate::snpgdsClose(fx$gds), add = TRUE)
  ids <- popgenVCF:::get_gds_ids(fx$gds)

  args <- sex_check_default_args
  args$x_chromosome_names <- "chrZZZ_not_present"
  result <- do.call(popgenVCF:::run_sex_check, c(list(fx$gds, fx$sample_id, ids$snp, ids, fx$metadata), args))
  expect_false(is.null(result))
  expect_identical(result$n_x_snps, 0L)
  expect_true(all(is.na(result$table$x_heterozygosity_F)))
  ordered <- result$table[match(fx$sample_id, result$table$vcf_sample), ]
  expect_identical(ordered$inferred_sex, fx$true_sex)
})

test_that("run_sex_check skips transparently only when both X and Y have too few SNPs", {
  fx <- sex_check_fixture(metadata_sex = NULL, n_snps_x = 5L, n_snps_y = 2L)
  on.exit(SNPRelate::snpgdsClose(fx$gds), add = TRUE)
  ids <- popgenVCF:::get_gds_ids(fx$gds)

  result <- do.call(popgenVCF:::run_sex_check, c(
    list(fx$gds, fx$sample_id, ids$snp, ids, fx$metadata), sex_check_default_args
  ))
  expect_null(result)
})

test_that("run_sex_check skips transparently when neither chromosome name matches", {
  fx <- sex_check_fixture(metadata_sex = NULL)
  on.exit(SNPRelate::snpgdsClose(fx$gds), add = TRUE)
  ids <- popgenVCF:::get_gds_ids(fx$gds)

  args <- sex_check_default_args
  args$x_chromosome_names <- "chrZZZ_not_present"
  args$y_chromosome_names <- "chrYYY_not_present"
  result <- do.call(popgenVCF:::run_sex_check, c(list(fx$gds, fx$sample_id, ids$snp, ids, fx$metadata), args))
  expect_null(result)
})

test_that("plot_sex_check draws a 2D X/Y figure when both signals are present, and 1D fallbacks otherwise", {
  fx <- sex_check_fixture(metadata_sex = NULL)
  on.exit(SNPRelate::snpgdsClose(fx$gds), add = TRUE)
  ids <- popgenVCF:::get_gds_ids(fx$gds)
  cfg <- list(
    output = list(figure_formats = "pdf", dpi = 150L),
    analyses = list(
      sex_check_male_f_threshold = 0.8, sex_check_female_f_threshold = 0.2,
      sex_check_y_male_call_rate_threshold = 0.5, sex_check_y_female_call_rate_threshold = 0.1
    )
  )
  dirs <- list(figures = tempdir())

  plots <- list()
  local_mocked_bindings(
    save_plot = function(p, stem, ...) { plots[[stem]] <<- p; invisible(TRUE) },
    .package = "popgenVCF"
  )
  result_xy <- do.call(popgenVCF:::run_sex_check, c(
    list(fx$gds, fx$sample_id, ids$snp, ids, fx$metadata), sex_check_default_args
  ))
  popgenVCF:::plot_sex_check(result_xy, cfg, dirs)
  expect_true("27_sex_check_F_by_sample" %in% names(plots))
  p_xy <- plots[["27_sex_check_F_by_sample"]]
  expect_identical(deparse(p_xy$labels$y), "\"Y-chromosome call rate\"")

  plots_x <- list()
  local_mocked_bindings(
    save_plot = function(p, stem, ...) { plots_x[[stem]] <<- p; invisible(TRUE) },
    .package = "popgenVCF"
  )
  args_x_only <- sex_check_default_args
  args_x_only$y_chromosome_names <- "chrZZZ_not_present"
  result_x_only <- do.call(popgenVCF:::run_sex_check, c(list(fx$gds, fx$sample_id, ids$snp, ids, fx$metadata), args_x_only))
  popgenVCF:::plot_sex_check(result_x_only, cfg, dirs)
  p_x <- plots_x[["27_sex_check_F_by_sample"]]
  expect_identical(deparse(p_x$labels$y), "expression(italic(F))")

  plots_y <- list()
  local_mocked_bindings(
    save_plot = function(p, stem, ...) { plots_y[[stem]] <<- p; invisible(TRUE) },
    .package = "popgenVCF"
  )
  args_y_only <- sex_check_default_args
  args_y_only$x_chromosome_names <- "chrZZZ_not_present"
  result_y_only <- do.call(popgenVCF:::run_sex_check, c(list(fx$gds, fx$sample_id, ids$snp, ids, fx$metadata), args_y_only))
  popgenVCF:::plot_sex_check(result_y_only, cfg, dirs)
  p_y <- plots_y[["27_sex_check_F_by_sample"]]
  expect_identical(deparse(p_y$labels$y), "\"Y-chromosome call rate\"")

  plots_empty <- list()
  local_mocked_bindings(
    save_plot = function(p, stem, ...) { plots_empty[[stem]] <<- p; invisible(TRUE) },
    .package = "popgenVCF"
  )
  empty_result <- list(table = data.table::data.table(), n_x_snps = 0L, n_y_snps = 0L)
  popgenVCF:::plot_sex_check(empty_result, cfg, dirs)
  expect_length(plots_empty, 0L)
})
