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
