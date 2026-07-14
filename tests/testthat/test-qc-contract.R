test_that("configuration enforces fixed LD contract", {
  cfg <- default_config()
  cfg$input$vcf <- tempfile(fileext = ".vcf")
  cfg$input$metadata <- tempfile(fileext = ".tsv")
  cfg$output$directory <- tempfile()
  writeLines("##fileformat=VCFv4.2", cfg$input$vcf)
  writeLines("sample\tpopulation\ns1\tp1", cfg$input$metadata)
  cfg$qc$ld_r2 <- 0.8
  cfg$qc$max_variant_missing <- 0.8
  out <- suppressWarnings(validate_config(cfg))
  expect_equal(out$qc$ld_r2, 0.2)
  expect_equal(out$qc$max_variant_missing, 0.2)
  expect_equal(out$qc$ld_slide_max_n, 50L)
  expect_identical(out$qc$ld_start_pos, "first")
})
