test_that("integer ranges parse", {
  expect_equal(popgenVCF:::parse_int_range("2:5"), 2:5)
  expect_equal(popgenVCF:::parse_int_range("4,2,4"), c(2L,4L))
})

test_that("fixed LD configuration is enforced", {
  cfg <- popgenVCF::default_config(); cfg$input$vcf <- tempfile(); cfg$input$metadata <- tempfile(); cfg$output$directory <- tempdir()
  file.create(cfg$input$vcf, cfg$input$metadata)
  cfg$qc$ld_r2 <- .7; cfg$qc$max_variant_missing <- .4
  expect_warning(v <- popgenVCF::validate_config(cfg))
  expect_equal(v$qc$ld_r2, .2)
  expect_equal(v$qc$max_variant_missing, .2)
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

test_that("system resource helpers understand container limits", {
  expect_equal(popgenVCF:::cpu_set_size("0-3,8,10-11"), 7L)
  expect_equal(popgenVCF:::cpu_quota_size("150000 100000"), 2L)
  expect_true(is.na(popgenVCF:::cpu_quota_size("max 100000")))
  expect_true(is.na(popgenVCF:::memory_value_bytes("max")))

  resources <- popgenVCF:::detect_system_resources()
  expect_gte(resources$threads, 1L)
  expect_true(is.infinite(resources$memory_mb) || resources$memory_mb >= 1)
})

test_that("generated configuration lists every analysis with safe metadata defaults", {
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
  expect_true(all(!unlist(cfg$analyses[metadata_dependent], use.names = FALSE)))
  expect_false(cfg$analyses$bootstrap$enabled)
  expect_true(all(unlist(cfg$analyses[c("pca", "ibs", "tree")], use.names = FALSE)))
  expect_false(cfg$analyses$admixture$enabled)
  expect_false(cfg$analyses$faststructure$enabled)
  expect_false(cfg$analyses$snmf$enabled)
  expect_equal(cfg$compute$threads, popgenVCF:::detect_available_threads())
  expect_equal(cfg$compute$memory_mb, popgenVCF:::detect_available_memory_mb())
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

test_that("template analysis toggles drive registry enablement", {
  registry <- popgenVCF::default_analysis_registry()
  cfg <- popgenVCF:::template_config()
  enabled <- names(registry$modules)[vapply(
    registry$modules, popgenVCF:::module_is_enabled, logical(1L), config = cfg
  )]
  expect_identical(enabled, c("pca", "ibs", "tree"))
})
