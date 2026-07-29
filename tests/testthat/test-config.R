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

test_that("template analysis toggles drive registry enablement", {
  registry <- popgenVCF::default_analysis_registry()
  cfg <- popgenVCF:::template_config()
  enabled <- names(registry$modules)[vapply(
    registry$modules, popgenVCF:::module_is_enabled, logical(1L), config = cfg
  )]
  expect_identical(enabled, c("pca", "ibs", "tree"))
})
