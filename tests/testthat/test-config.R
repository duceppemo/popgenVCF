test_that("integer ranges parse", {
  expect_equal(popgenVCF:::parse_int_range("2:5"), 2:5)
  expect_equal(popgenVCF:::parse_int_range("4,2,4"), c(2L,4L))
})

test_that("diversity_allelic_richness defaults to TRUE and preserves current pipeline behavior", {
  expect_true(popgenVCF::default_config()$analyses$diversity_allelic_richness)
})

test_that("max_variant_missing and ld_r2 are both user-configurable, with no forced override", {
  cfg <- popgenVCF::default_config(); cfg$input$vcf <- tempfile(); cfg$input$metadata <- tempfile(); cfg$output$directory <- tempdir()
  file.create(cfg$input$vcf, cfg$input$metadata)
  cfg$qc$ld_r2 <- .7; cfg$qc$max_variant_missing <- .4
  v <- expect_silent(popgenVCF::validate_config(cfg))
  expect_equal(v$qc$ld_r2, .7)
  expect_equal(v$qc$max_variant_missing, .4)
})

test_that("a malformed multi-element config value errors clearly instead of crashing cryptically or silently passing", {
  # Real gap found in a pre-release audit: every scalar-valued config leaf's
  # own `if (!is.finite(x) || x COMPARISON)`-style check assumes `x` already
  # has length 1. A malformed config (e.g. an accidental YAML sequence like
  # `maf: [0.1, 0.6]` from a copy-paste or formatting slip) reached these
  # checks as a multi-element vector: for most fields this crashed with a
  # cryptic, generic R error ("the condition has length > 1", a hard error
  # on every R version this package supports per DESCRIPTION's R >= 4.3
  # requirement) instead of this codebase's otherwise-consistent clear
  # diagnostics; for max_sample_missing/ld_r2 specifically, both elements
  # individually happened to be in-range, so the vectorized `any()`-based
  # check silently accepted the whole malformed vector with no error at
  # all, and the field was never coerced back to scalar.
  base_cfg <- function() {
    cfg <- popgenVCF::default_config()
    cfg$input$vcf <- tempfile(fileext = ".vcf")
    file.create(cfg$input$vcf)
    cfg$output$directory <- tempfile("popgenvcf-output-")
    cfg
  }

  cfg <- base_cfg(); cfg$qc$maf <- c(0.1, 0.6)
  expect_error(popgenVCF::validate_config(cfg), "qc.maf")

  cfg <- base_cfg(); cfg$qc$max_sample_missing <- c(0.1, 0.2)
  expect_error(popgenVCF::validate_config(cfg), "qc.max_sample_missing")

  cfg <- base_cfg(); cfg$qc$ld_r2 <- c(0.1, 0.2)
  expect_error(popgenVCF::validate_config(cfg), "qc.ld_r2")

  cfg <- base_cfg(); cfg$analyses$sex_check_female_f_threshold <- c(0.1, 0.2)
  expect_error(popgenVCF::validate_config(cfg), "analyses.sex_check_female_f_threshold")

  cfg <- base_cfg(); cfg$analyses$sexbias_test <- c("mAIc", "FST")
  expect_error(popgenVCF::validate_config(cfg), "analyses.sexbias_test")

  # A well-formed default config must still validate silently.
  expect_silent(popgenVCF::validate_config(base_cfg()))

  # analyses.pcadapt_k stays genuinely optional (NULL is valid); a
  # multi-element non-NULL value must still be rejected.
  cfg <- base_cfg(); cfg$analyses$pcadapt_k <- c(2L, 3L)
  expect_error(popgenVCF::validate_config(cfg), "analyses.pcadapt_k")
})

test_that("configuration schema is explicit and validated", {
  cfg <- popgenVCF::default_config()
  expect_identical(cfg$schema_version, "1.0")
  cfg$schema_version <- "999"
  expect_error(popgenVCF::validate_config(cfg), "Unsupported configuration schema_version")
})

test_that("publication figure settings are explicit and validated", {
  cfg <- popgenVCF::default_config()
  expect_identical(cfg$output$figure_style, "accessibility-first")
  expect_identical(cfg$output$base_font_size, 11)

  cfg$input$vcf <- tempfile(fileext = ".vcf")
  cfg$output$directory <- tempfile("popgenvcf-output-")
  file.create(cfg$input$vcf)
  cfg$output$figure_style <- "grayscale-safe"
  cfg$output$base_font_size <- 12
  validated <- popgenVCF::validate_config(cfg)
  expect_identical(validated$output$figure_style, "grayscale-safe")
  expect_identical(validated$output$base_font_size, 12)

  cfg$output$figure_style <- "rainbow"
  expect_error(popgenVCF::validate_config(cfg), "output.figure_style")
  cfg$output$figure_style <- "accessibility-first"
  cfg$output$base_font_size <- 7
  expect_error(popgenVCF::validate_config(cfg), "base_font_size")
})

test_that("ancestry backend inputs can be generated from retained VCF data", {
  cfg <- popgenVCF::default_config()
  cfg$input$vcf <- tempfile(fileext = ".vcf")
  cfg$output$directory <- tempfile("popgenvcf-output-")
  file.create(cfg$input$vcf)
  cfg$analyses$admixture$enabled <- TRUE
  cfg$analyses$faststructure$enabled <- TRUE
  cfg$analyses$snmf$enabled <- TRUE

  validated <- popgenVCF::validate_config(cfg)

  expect_null(validated$analyses$admixture$plink_prefix)
  expect_null(validated$analyses$admixture$q_sample_file)
  expect_null(validated$analyses$faststructure$plink_prefix)
  expect_null(validated$analyses$faststructure$q_sample_file)
  expect_null(validated$analyses$snmf$geno_file)
  expect_null(validated$analyses$snmf$q_sample_file)
})

test_that("dapc_loading_top_n is coerced and validated", {
  cfg <- popgenVCF::default_config()
  cfg$input$vcf <- tempfile(fileext = ".vcf")
  cfg$output$directory <- tempfile("popgenvcf-output-")
  file.create(cfg$input$vcf)

  expect_identical(cfg$analyses$dapc_loading_top_n, 20L)

  cfg$analyses$dapc_loading_top_n <- "15"
  validated <- popgenVCF::validate_config(cfg)
  expect_identical(validated$analyses$dapc_loading_top_n, 15L)

  cfg$analyses$dapc_loading_top_n <- 0L
  expect_error(popgenVCF::validate_config(cfg), "dapc_loading_top_n")
})

test_that("clonality_genotype_curve_replicates and clonality_ia_permutations are coerced and validated", {
  cfg <- popgenVCF::default_config()
  cfg$input$vcf <- tempfile(fileext = ".vcf")
  cfg$output$directory <- tempfile("popgenvcf-output-")
  file.create(cfg$input$vcf)

  expect_identical(cfg$analyses$clonality_genotype_curve_replicates, 100L)
  expect_identical(cfg$analyses$clonality_ia_permutations, 0L)

  cfg$analyses$clonality_genotype_curve_replicates <- "50"
  cfg$analyses$clonality_ia_permutations <- "99"
  validated <- popgenVCF::validate_config(cfg)
  expect_identical(validated$analyses$clonality_genotype_curve_replicates, 50L)
  expect_identical(validated$analyses$clonality_ia_permutations, 99L)

  cfg$analyses$clonality_genotype_curve_replicates <- -1L
  expect_error(popgenVCF::validate_config(cfg), "clonality_genotype_curve_replicates")

  cfg$analyses$clonality_genotype_curve_replicates <- 50L
  cfg$analyses$clonality_ia_permutations <- -1L
  expect_error(popgenVCF::validate_config(cfg), "clonality_ia_permutations")
})

test_that("sexbias_test and sexbias_permutations are coerced and validated", {
  cfg <- popgenVCF::default_config()
  cfg$input$vcf <- tempfile(fileext = ".vcf")
  cfg$output$directory <- tempfile("popgenvcf-output-")
  file.create(cfg$input$vcf)

  expect_identical(cfg$analyses$sexbias_test, "mAIc")
  expect_identical(cfg$analyses$sexbias_permutations, 0L)

  cfg$analyses$sexbias_permutations <- "99"
  validated <- popgenVCF::validate_config(cfg)
  expect_identical(validated$analyses$sexbias_permutations, 99L)

  cfg$analyses$sexbias_permutations <- -1L
  expect_error(popgenVCF::validate_config(cfg), "sexbias_permutations")

  cfg$analyses$sexbias_permutations <- 0L
  cfg$analyses$sexbias_test <- "bogus"
  expect_error(popgenVCF::validate_config(cfg), "sexbias_test")

  cfg$analyses$sexbias_test <- "FST"
  cfg$analyses$sexbias_permutations <- 0L
  expect_error(popgenVCF::validate_config(cfg), "sexbias_permutations.*FIS.*FST|FIS.*FST.*sexbias_permutations")

  cfg$analyses$sexbias_permutations <- 999L
  expect_silent(popgenVCF::validate_config(cfg))
})

test_that("amova_permutations and mantel_permutations are coerced and validated, and require >= 1", {
  # Real gap found in a pre-release audit: run_module_amova()/run_module_ibd()
  # (R/module_registry.R) hardcoded 999L with no config knob at all, unlike
  # the structurally identical clonality/sexbias/spatial-autocorrelation
  # permutation counts. Unlike those three (which gate an optional test
  # meaningfully skippable with 0), AMOVA's ade4::randtest()/Mantel's
  # vegan::mantel() significance tests are not optional once their analysis
  # runs, so 0 must be rejected, not just negative values.
  cfg <- popgenVCF::default_config()
  cfg$input$vcf <- tempfile(fileext = ".vcf")
  cfg$output$directory <- tempfile("popgenvcf-output-")
  file.create(cfg$input$vcf)

  expect_identical(cfg$analyses$amova_permutations, 999L)
  expect_identical(cfg$analyses$mantel_permutations, 999L)

  cfg$analyses$amova_permutations <- "500"
  cfg$analyses$mantel_permutations <- "250"
  validated <- popgenVCF::validate_config(cfg)
  expect_identical(validated$analyses$amova_permutations, 500L)
  expect_identical(validated$analyses$mantel_permutations, 250L)

  cfg$analyses$amova_permutations <- 0L
  expect_error(popgenVCF::validate_config(cfg), "amova_permutations")

  cfg$analyses$amova_permutations <- 500L
  cfg$analyses$mantel_permutations <- 0L
  expect_error(popgenVCF::validate_config(cfg), "mantel_permutations")

  cfg$analyses$mantel_permutations <- -1L
  expect_error(popgenVCF::validate_config(cfg), "mantel_permutations")
})

test_that("pcadapt_k, pcadapt_min_maf, and pcadapt_fdr_alpha are coerced and validated", {
  cfg <- popgenVCF::default_config()
  cfg$input$vcf <- tempfile(fileext = ".vcf")
  cfg$output$directory <- tempfile("popgenvcf-output-")
  file.create(cfg$input$vcf)

  expect_null(cfg$analyses$pcadapt_k)
  expect_identical(cfg$analyses$pcadapt_min_maf, 0.05)
  expect_identical(cfg$analyses$pcadapt_fdr_alpha, 0.05)

  cfg$analyses$pcadapt_k <- "4"
  cfg$analyses$pcadapt_min_maf <- "0.1"
  cfg$analyses$pcadapt_fdr_alpha <- "0.01"
  validated <- popgenVCF::validate_config(cfg)
  expect_identical(validated$analyses$pcadapt_k, 4L)
  expect_identical(validated$analyses$pcadapt_min_maf, 0.1)
  expect_identical(validated$analyses$pcadapt_fdr_alpha, 0.01)

  cfg$analyses$pcadapt_k <- 0L
  expect_error(popgenVCF::validate_config(cfg), "pcadapt_k")

  cfg$analyses$pcadapt_k <- NULL
  cfg$analyses$pcadapt_min_maf <- 0.5
  expect_error(popgenVCF::validate_config(cfg), "pcadapt_min_maf")

  cfg$analyses$pcadapt_min_maf <- 0.05
  cfg$analyses$pcadapt_fdr_alpha <- 1
  expect_error(popgenVCF::validate_config(cfg), "pcadapt_fdr_alpha")
})

test_that("pca_loading_top_n is coerced and validated", {
  cfg <- popgenVCF::default_config()
  cfg$input$vcf <- tempfile(fileext = ".vcf")
  cfg$output$directory <- tempfile("popgenvcf-output-")
  file.create(cfg$input$vcf)

  expect_identical(cfg$analyses$pca_loading_top_n, 20L)

  cfg$analyses$pca_loading_top_n <- "15"
  validated <- popgenVCF::validate_config(cfg)
  expect_identical(validated$analyses$pca_loading_top_n, 15L)

  cfg$analyses$pca_loading_top_n <- 0L
  expect_error(popgenVCF::validate_config(cfg), "pca_loading_top_n")
})

test_that("pca_metadata_color_min_group and pca_metadata_color_max_levels are coerced and validated", {
  cfg <- popgenVCF::default_config()
  cfg$input$vcf <- tempfile(fileext = ".vcf")
  cfg$output$directory <- tempfile("popgenvcf-output-")
  file.create(cfg$input$vcf)

  expect_true(cfg$analyses$pca_metadata_color)
  expect_identical(cfg$analyses$pca_metadata_color_min_group, 3L)
  expect_identical(cfg$analyses$pca_metadata_color_max_levels, 12L)

  cfg$analyses$pca_metadata_color_min_group <- "5"
  cfg$analyses$pca_metadata_color_max_levels <- "8"
  validated <- popgenVCF::validate_config(cfg)
  expect_identical(validated$analyses$pca_metadata_color_min_group, 5L)
  expect_identical(validated$analyses$pca_metadata_color_max_levels, 8L)

  cfg$analyses$pca_metadata_color_min_group <- 0L
  expect_error(popgenVCF::validate_config(cfg), "pca_metadata_color_min_group")

  cfg$analyses$pca_metadata_color_min_group <- 3L
  cfg$analyses$pca_metadata_color_max_levels <- 1L
  expect_error(popgenVCF::validate_config(cfg), "pca_metadata_color_max_levels")
})

test_that("hwe_alpha is coerced and validated", {
  cfg <- popgenVCF::default_config()
  cfg$input$vcf <- tempfile(fileext = ".vcf")
  cfg$output$directory <- tempfile("popgenvcf-output-")
  file.create(cfg$input$vcf)

  expect_identical(cfg$analyses$hwe_alpha, 0.05)

  cfg$analyses$hwe_alpha <- "0.01"
  validated <- popgenVCF::validate_config(cfg)
  expect_identical(validated$analyses$hwe_alpha, 0.01)

  cfg$analyses$hwe_alpha <- 0
  expect_error(popgenVCF::validate_config(cfg), "hwe_alpha")
  cfg$analyses$hwe_alpha <- 1
  expect_error(popgenVCF::validate_config(cfg), "hwe_alpha")
})

test_that("kinship_close_relative_threshold is coerced and validated", {
  cfg <- popgenVCF::default_config()
  cfg$input$vcf <- tempfile(fileext = ".vcf")
  cfg$output$directory <- tempfile("popgenvcf-output-")
  file.create(cfg$input$vcf)

  expect_identical(cfg$analyses$kinship_close_relative_threshold, 0.0442)

  cfg$analyses$kinship_close_relative_threshold <- "0.0884"
  validated <- popgenVCF::validate_config(cfg)
  expect_identical(validated$analyses$kinship_close_relative_threshold, 0.0884)

  cfg$analyses$kinship_close_relative_threshold <- 0
  expect_error(popgenVCF::validate_config(cfg), "kinship_close_relative_threshold")
  cfg$analyses$kinship_close_relative_threshold <- 0.51
  expect_error(popgenVCF::validate_config(cfg), "kinship_close_relative_threshold")
})

test_that("sex-check X and Y thresholds/chromosome names are coerced and validated", {
  fresh_cfg <- function() {
    cfg <- popgenVCF::default_config()
    cfg$input$vcf <- tempfile(fileext = ".vcf")
    cfg$output$directory <- tempfile("popgenvcf-output-")
    file.create(cfg$input$vcf)
    cfg
  }

  cfg <- fresh_cfg()
  expect_identical(cfg$analyses$sex_check_x_chromosome_names, c("X", "chrX"))
  expect_identical(cfg$analyses$sex_check_male_f_threshold, 0.8)
  expect_identical(cfg$analyses$sex_check_female_f_threshold, 0.2)
  expect_identical(cfg$analyses$sex_check_y_chromosome_names, c("Y", "chrY"))
  expect_identical(cfg$analyses$sex_check_y_male_call_rate_threshold, 0.5)
  expect_identical(cfg$analyses$sex_check_y_female_call_rate_threshold, 0.1)

  cfg$analyses$sex_check_male_f_threshold <- "0.9"
  cfg$analyses$sex_check_y_male_call_rate_threshold <- "0.6"
  validated <- popgenVCF::validate_config(cfg)
  expect_identical(validated$analyses$sex_check_male_f_threshold, 0.9)
  expect_identical(validated$analyses$sex_check_y_male_call_rate_threshold, 0.6)

  cfg$analyses$sex_check_female_f_threshold <- 0.95
  expect_error(popgenVCF::validate_config(cfg), "sex_check_female_f_threshold")

  cfg <- fresh_cfg()
  cfg$analyses$sex_check_y_female_call_rate_threshold <- 0.9
  expect_error(popgenVCF::validate_config(cfg), "sex_check_y_female_call_rate_threshold")

  cfg <- fresh_cfg()
  cfg$analyses$sex_check_y_male_call_rate_threshold <- 1.5
  expect_error(popgenVCF::validate_config(cfg), "sex_check_y_.*_call_rate_threshold")

  cfg <- fresh_cfg()
  cfg$analyses$sex_check_x_chromosome_names <- character()
  expect_error(popgenVCF::validate_config(cfg), "sex_check_x_chromosome_names")

  cfg <- fresh_cfg()
  cfg$analyses$sex_check_y_chromosome_names <- character()
  expect_error(popgenVCF::validate_config(cfg), "sex_check_y_chromosome_names")
})

test_that("roh_gt_error_phred is coerced and validated", {
  cfg <- popgenVCF::default_config()
  cfg$input$vcf <- tempfile(fileext = ".vcf")
  cfg$output$directory <- tempfile("popgenvcf-output-")
  file.create(cfg$input$vcf)

  expect_identical(cfg$analyses$roh_gt_error_phred, 30)

  cfg$analyses$roh_gt_error_phred <- "20"
  validated <- popgenVCF::validate_config(cfg)
  expect_identical(validated$analyses$roh_gt_error_phred, 20)

  cfg$analyses$roh_gt_error_phred <- 0
  expect_error(popgenVCF::validate_config(cfg), "roh_gt_error_phred")
  cfg$analyses$roh_gt_error_phred <- -5
  expect_error(popgenVCF::validate_config(cfg), "roh_gt_error_phred")
})

test_that("genome_scan window/step/min_snps keys are coerced and validated", {
  cfg <- popgenVCF::default_config()
  cfg$input$vcf <- tempfile(fileext = ".vcf")
  cfg$output$directory <- tempfile("popgenvcf-output-")
  file.create(cfg$input$vcf)

  expect_identical(cfg$analyses$genome_scan_window_bp, 50000L)
  expect_identical(cfg$analyses$genome_scan_step_bp, 50000L)
  expect_identical(cfg$analyses$genome_scan_min_snps, 5L)

  cfg$analyses$genome_scan_window_bp <- "10000"
  cfg$analyses$genome_scan_step_bp <- "5000"
  cfg$analyses$genome_scan_min_snps <- "3"
  validated <- popgenVCF::validate_config(cfg)
  expect_identical(validated$analyses$genome_scan_window_bp, 10000L)
  expect_identical(validated$analyses$genome_scan_step_bp, 5000L)
  expect_identical(validated$analyses$genome_scan_min_snps, 3L)

  cfg$analyses$genome_scan_window_bp <- 0L
  expect_error(popgenVCF::validate_config(cfg), "genome_scan_window_bp")
  cfg$analyses$genome_scan_window_bp <- 50000L

  cfg$analyses$genome_scan_step_bp <- 0L
  expect_error(popgenVCF::validate_config(cfg), "genome_scan_step_bp")
  cfg$analyses$genome_scan_step_bp <- 50000L

  cfg$analyses$genome_scan_min_snps <- 1L
  expect_error(popgenVCF::validate_config(cfg), "genome_scan_min_snps")
})

test_that("ld_decay max_distance_bp/bin_bp/slide keys are coerced and validated", {
  cfg <- popgenVCF::default_config()
  cfg$input$vcf <- tempfile(fileext = ".vcf")
  cfg$output$directory <- tempfile("popgenvcf-output-")
  file.create(cfg$input$vcf)

  expect_identical(cfg$analyses$ld_decay, TRUE)
  expect_identical(cfg$analyses$ld_decay_max_distance_bp, 500000L)
  expect_identical(cfg$analyses$ld_decay_bin_bp, 5000L)
  expect_identical(cfg$analyses$ld_decay_slide, 100L)

  cfg$analyses$ld_decay_max_distance_bp <- "100000"
  cfg$analyses$ld_decay_bin_bp <- "1000"
  cfg$analyses$ld_decay_slide <- "50"
  validated <- popgenVCF::validate_config(cfg)
  expect_identical(validated$analyses$ld_decay_max_distance_bp, 100000L)
  expect_identical(validated$analyses$ld_decay_bin_bp, 1000L)
  expect_identical(validated$analyses$ld_decay_slide, 50L)

  cfg$analyses$ld_decay_max_distance_bp <- 0L
  expect_error(popgenVCF::validate_config(cfg), "ld_decay_max_distance_bp")
  cfg$analyses$ld_decay_max_distance_bp <- 500000L

  cfg$analyses$ld_decay_bin_bp <- 0L
  expect_error(popgenVCF::validate_config(cfg), "ld_decay_bin_bp")
  cfg$analyses$ld_decay_bin_bp <- 5000L

  cfg$analyses$ld_decay_slide <- 0L
  expect_error(popgenVCF::validate_config(cfg), "ld_decay_slide")
})

test_that("ne_ld_max_snps is coerced and validated", {
  cfg <- popgenVCF::default_config()
  cfg$input$vcf <- tempfile(fileext = ".vcf")
  cfg$output$directory <- tempfile("popgenvcf-output-")
  file.create(cfg$input$vcf)

  expect_identical(cfg$analyses$ne_ld, TRUE)
  expect_identical(cfg$analyses$ne_ld_max_snps, 2000L)

  cfg$analyses$ne_ld_max_snps <- "500"
  validated <- popgenVCF::validate_config(cfg)
  expect_identical(validated$analyses$ne_ld_max_snps, 500L)

  cfg$analyses$ne_ld_max_snps <- 1L
  expect_error(popgenVCF::validate_config(cfg), "ne_ld_max_snps")
})

test_that("system resource helpers understand container limits", {
  expect_equal(popgenVCF:::cpu_set_size("0-3,8,10-11"), 7L)
  expect_equal(popgenVCF:::cpu_quota_size("150000 100000"), 2L)
  expect_true(is.na(popgenVCF:::cpu_quota_size("max 100000")))
  expect_true(is.na(popgenVCF:::memory_value_bytes("max")))

  resources <- popgenVCF:::detect_system_resources()
  expect_gte(resources$threads, 1L)
  expect_true(is.infinite(resources$memory_mb) || resources$memory_mb >= 1)
})

test_that("generated configuration lists every analysis, metadata-dependent ones enabled since the capability gate skips them safely", {
  # write_default_config() copies the package's own bundled, fully-commented
  # inst/example_config.yml rather than serializing default_config() from
  # scratch (see its own definition in R/cli.R): the capability gate already
  # skips each metadata-dependent module automatically, with a WARNING,
  # whenever the metadata it needs is actually absent, so there is nothing
  # unsafe about shipping them enabled in the written template. Ancestry
  # backends stay off by default for unrelated reasons (external tool, not
  # metadata). compute.threads/memory_mb are fixed, portable example values
  # in the reference file, not this machine's own auto-detected resources
  # (which default_config() itself uses at runtime when no config is
  # supplied) -- a generated template showing whatever hardware happened to
  # write it would be confusing to read and non-portable to share.
  path <- tempfile(fileext = ".yml")
  popgenVCF:::write_default_config(path)
  cfg <- yaml::read_yaml(path)

  expect_true(all(c(
    "diversity", "pca", "ibs", "tree", "fst", "dapc", "amova", "mantel",
    "isolation_by_distance", "chromosome_specific", "admixture",
    "faststructure", "snmf"
  ) %in% names(cfg$analyses)))
  metadata_dependent <- c(
    "diversity", "fst", "dapc", "amova", "mantel",
    "isolation_by_distance", "chromosome_specific"
  )
  expect_true(all(unlist(cfg$analyses[metadata_dependent], use.names = FALSE)))
  expect_true(cfg$analyses$bootstrap$enabled)
  expect_true(all(unlist(cfg$analyses[c("pca", "ibs", "tree")], use.names = FALSE)))
  expect_false(cfg$analyses$admixture$enabled)
  expect_false(cfg$analyses$faststructure$enabled)
  expect_false(cfg$analyses$snmf$enabled)
  expect_true(is.numeric(cfg$compute$threads) && cfg$compute$threads >= 1)
  expect_true(is.numeric(cfg$compute$memory_mb) && cfg$compute$memory_mb >= 1)
})

test_that("inst/example_config.yml -- the literal file --write-config now ships -- passes validate_config() end to end", {
  # Requested directly: is there a step that checks every line of this file
  # is actually valid? There wasn't -- existing coverage only spot-checked
  # individual fields (test-autosome-filtering.R's YAML-boolean-corruption
  # check, and the two specific bugs this session already found and fixed
  # in test-cli.R above), never the whole file through the real validator.
  # Both of those bugs (analyses.bootstrap.enabled drifted from the real
  # default; sex_check_y_chromosome_names' unquoted "Y" silently parsing as
  # a YAML boolean) were only caught by hand. This is the general guard: it
  # exercises every field validate_config() actually inspects, not just the
  # two already known to have been wrong, so a future edit to this file
  # that breaks something else fails a test instead of shipping silently.
  # Only the three fields that are legitimately user-specific placeholders
  # (input.vcf, input.metadata, output.directory) are substituted with real
  # stand-ins first; everything else is validated exactly as shipped.
  path <- system.file("example_config.yml", package = "popgenVCF", mustWork = TRUE)
  cfg <- popgenVCF::read_config(path)

  cfg$input$vcf <- tempfile(fileext = ".vcf")
  file.create(cfg$input$vcf)
  cfg$input$metadata <- tempfile(fileext = ".tsv")
  file.create(cfg$input$metadata)
  cfg$output$directory <- tempfile("popgenvcf-output-")

  validated <- expect_no_error(popgenVCF::validate_config(cfg))
  expect_true(validated$analyses$bootstrap$enabled)
  expect_identical(validated$analyses$sex_check_y_chromosome_names, c("Y", "chrY"))
})

test_that("auto ancestry threads resolve from the compute budget", {
  cfg <- popgenVCF::default_config()
  cfg$input$vcf <- tempfile(fileext = ".vcf")
  cfg$output$directory <- tempfile("popgenvcf-output-")
  file.create(cfg$input$vcf)
  cfg$compute$threads <- 3L
  cfg$analyses$admixture$threads <- "auto"
  cfg$analyses$snmf$threads <- "auto"

  validated <- popgenVCF::validate_config(cfg)
  expect_identical(validated$analyses$admixture$threads, 3L)
  expect_identical(validated$analyses$snmf$threads, 3L)
})

test_that("ancestry-backend timeout_seconds defaults and validates", {
  cfg <- popgenVCF::default_config()
  cfg$input$vcf <- tempfile(fileext = ".vcf")
  cfg$output$directory <- tempfile("popgenvcf-output-")
  file.create(cfg$input$vcf)

  validated <- popgenVCF::validate_config(cfg)
  expect_identical(validated$analyses$admixture$timeout_seconds, 14400)
  expect_identical(validated$analyses$faststructure$timeout_seconds, 14400)

  cfg$analyses$admixture$timeout_seconds <- 0
  expect_error(popgenVCF::validate_config(cfg), "analyses.admixture.timeout_seconds")

  cfg$analyses$admixture$timeout_seconds <- 60
  cfg$analyses$faststructure$timeout_seconds <- -1
  expect_error(popgenVCF::validate_config(cfg), "analyses.faststructure.timeout_seconds")
})

test_that("sNMF forwards CPU and deterministic seed settings to LEA", {
  args <- popgenVCF:::snmf_project_arguments(
    "input.geno", 2:4, 5L, TRUE, "new", 6L, 123L
  )
  expect_identical(args$CPU, 6L)
  expect_identical(args$seed, 123L)
  expect_identical(args$K, 2:4)
  expect_identical(args$repetitions, 5L)
  capped <- popgenVCF:::snmf_project_arguments(
    "input.geno", 2L, 2L, TRUE, "new", 8L, 123L
  )
  expect_identical(capped$CPU, 2L)
  expect_error(
    popgenVCF:::snmf_project_arguments("input.geno", 2L, 1L, TRUE, "new", 0L, 1L),
    "threads"
  )
})

test_that("sNMF diagnostics remain well formed without cross-entropy values", {
  completed <- popgenVCF:::complete_snmf_cross_entropy(
    3L, repetitions = 3L, values = numeric()
  )
  expect_named(completed, c("K", "run", "cross_entropy"))
  expect_identical(completed$K, rep(3L, 3L))
  expect_identical(completed$run, 1:3)
  expect_true(all(is.na(completed$cross_entropy)))

  matrix_values <- matrix(c(0.52, 0.50, 0.51), nrow = 1L)
  from_matrix <- popgenVCF:::complete_snmf_cross_entropy(
    2L, repetitions = 3L, values = matrix_values
  )
  expect_named(from_matrix, c("K", "run", "cross_entropy"))
  expect_equal(from_matrix$cross_entropy, as.numeric(matrix_values))
  expect_identical(from_matrix$run, 1:3)

  summarized <- popgenVCF:::summarize_snmf_k_diagnostics(
    data.frame(K = rep(2:3, each = 2L), run = rep(1:2, 2L))
  )
  expect_identical(summarized$K, 2:3)
  expect_true(all(is.na(summarized$cross_entropy)))
  expect_true(all(is.na(summarized$cross_entropy_se)))
})

test_that("sNMF diagnostic summaries ignore non-finite replicate values", {
  summarized <- popgenVCF:::summarize_snmf_k_diagnostics(data.frame(
    K = rep(2:3, each = 3L),
    run = rep(1:3, 2L),
    cross_entropy = c(0.5, NA, 0.4, 0.3, Inf, 0.2)
  ))

  expect_equal(summarized$cross_entropy, c(0.45, 0.25))
  expect_equal(
    summarized$cross_entropy_se,
    c(stats::sd(c(0.5, 0.4)), stats::sd(c(0.3, 0.2))) / sqrt(2)
  )
})

test_that("input.sample_column/population_column default to NULL and validate as single non-empty strings", {
  cfg <- popgenVCF::default_config()
  expect_null(cfg$input$sample_column)
  expect_null(cfg$input$population_column)

  cfg$input$vcf <- tempfile(fileext = ".vcf")
  cfg$output$directory <- tempfile("popgenvcf-output-")
  file.create(cfg$input$vcf)

  cfg$input$population_column <- "pathotype"
  validated <- popgenVCF::validate_config(cfg)
  expect_identical(validated$input$population_column, "pathotype")

  cfg$input$population_column <- ""
  expect_error(popgenVCF::validate_config(cfg), "input.population_column")
  cfg$input$population_column <- c("a", "b")
  expect_error(popgenVCF::validate_config(cfg), "input.population_column")

  cfg$input$population_column <- NULL
  cfg$input$sample_column <- ""
  expect_error(popgenVCF::validate_config(cfg), "input.sample_column")
})

test_that("default analysis toggles drive registry enablement", {
  # Population/geography-gated analyses stay enabled in default_config()
  # since the capability gate (analysis_capability_table()) already skips
  # each one automatically -- with a WARNING and an analysis_capabilities.tsv
  # record -- whenever the metadata it actually needs is absent. Only
  # ml_tree and the ancestry backends (admixture/faststructure/snmf) stay
  # off by default, for reasons unrelated to metadata.
  registry <- popgenVCF::default_analysis_registry()
  cfg <- popgenVCF::default_config()
  enabled <- names(registry$modules)[vapply(
    registry$modules, popgenVCF:::module_is_enabled, logical(1L), config = cfg
  )]
  expect_identical(enabled, c(
    "diversity", "bottleneck", "pca", "ibs", "kinship", "sex_check", "roh", "tree",
    "population_tree", "population_assignment", "fst", "genome_scan", "pcadapt",
    "ld_decay", "ne_ld", "dapc", "amova", "clonality", "sexbias", "ibd",
    "spatial_autocorrelation", "chromosome"
  ))
  expect_false(popgenVCF:::module_is_enabled(registry$modules$ml_tree, cfg))
  expect_false(popgenVCF:::module_is_enabled(registry$modules$admixture, cfg))
  expect_false(popgenVCF:::module_is_enabled(registry$modules$faststructure, cfg))
  expect_false(popgenVCF:::module_is_enabled(registry$modules$snmf, cfg))
})
