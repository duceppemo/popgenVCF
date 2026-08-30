# Regression test for a real bug found in a pre-release bug audit: the
# variant_missing/ld_r2 fields of run_manifest.tsv were hardcoded 0.2
# literals rather than read from cfg$qc$max_variant_missing/cfg$qc$ld_r2,
# so a run with a non-default value silently produced a lying provenance
# record once those two parameters became genuinely user-configurable.

write_manifest_fixture <- function(root) {
  cfg <- default_config()
  cfg$output$directory <- root
  cfg$input$vcf <- tempfile(fileext = ".vcf")
  writeLines("##fileformat=VCFv4.2", cfg$input$vcf)
  cfg$qc$max_variant_missing <- 0.37
  cfg$qc$ld_r2 <- 0.63
  cfg <- validate_config(cfg)

  analysis <- new_popgen_vcf_analysis(cfg)
  analysis$samples$ids <- c("a", "b")
  analysis$samples$metadata <- data.table::data.table(
    sample = c("a", "b"), population = c("x", "y")
  )
  analysis$variants$qc_ids <- 1:4
  analysis$variants$ld_ids <- 1:3

  dirs <- list(root = root)
  list(cfg = cfg, analysis = analysis, dirs = dirs)
}

test_that("write_manifest records the actually-configured max_variant_missing and ld_r2", {
  root <- withr::local_tempdir()
  fx <- write_manifest_fixture(root)
  write_manifest(fx$cfg, fx$dirs, fx$analysis)

  manifest <- data.table::fread(file.path(root, "run_manifest.tsv"))
  expect_equal(
    as.numeric(manifest$value[manifest$field == "variant_missing"]), 0.37
  )
  expect_equal(
    as.numeric(manifest$value[manifest$field == "ld_r2"]), 0.63
  )
})
