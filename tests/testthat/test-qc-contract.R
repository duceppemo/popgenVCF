test_that("configuration enforces the fixed max_variant_missing contract; LD-pruning parameters are user-configurable", {
  cfg <- default_config()
  cfg$input$vcf <- tempfile(fileext = ".vcf")
  cfg$input$metadata <- tempfile(fileext = ".tsv")
  cfg$output$directory <- tempfile()
  writeLines("##fileformat=VCFv4.2", cfg$input$vcf)
  writeLines("sample\tpopulation\ns1\tp1", cfg$input$metadata)
  cfg$qc$ld_r2 <- 0.8
  cfg$qc$max_variant_missing <- 0.8
  cfg$qc$ld_slide_max_n <- 200L
  cfg$qc$ld_start_pos <- "random"
  out <- suppressWarnings(validate_config(cfg))
  expect_equal(out$qc$ld_r2, 0.8)
  expect_equal(out$qc$max_variant_missing, 0.2)
  expect_equal(out$qc$ld_slide_max_n, 200L)
  expect_identical(out$qc$ld_start_pos, "random")
})

test_that("validate_config rejects an out-of-range LD-pruning window/start position", {
  base_cfg <- function() {
    cfg <- default_config()
    cfg$input$vcf <- tempfile(fileext = ".vcf")
    cfg$input$metadata <- tempfile(fileext = ".tsv")
    cfg$output$directory <- tempfile()
    writeLines("##fileformat=VCFv4.2", cfg$input$vcf)
    writeLines("sample\tpopulation\ns1\tp1", cfg$input$metadata)
    cfg
  }

  cfg <- base_cfg(); cfg$qc$ld_slide_max_n <- 0L
  expect_error(validate_config(cfg), "ld_slide_max_n must be a positive integer")

  cfg <- base_cfg(); cfg$qc$ld_slide_max_bp <- -1
  expect_error(validate_config(cfg), "ld_slide_max_bp must be a positive number or Inf")

  cfg <- base_cfg(); cfg$qc$ld_start_pos <- "middle"
  expect_error(validate_config(cfg), "ld_start_pos must be one of")
})
