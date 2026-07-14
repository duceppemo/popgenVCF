test_that("analysis object construction and result access work", {
  cfg <- popgenVCF::default_config()
  x <- popgenVCF::new_popgen_vcf_analysis(cfg)
  expect_identical(class(x), "PopgenVCFAnalysis")
  expect_true(popgenVCF::is_popgen_vcf_analysis(x))
  x <- popgenVCF::set_analysis_result(x, "example", list(value = 42))
  expect_equal(popgenVCF::get_analysis_result(x, "example")$value, 42)
  expect_null(popgenVCF::get_analysis_result(x, "missing"))
})

test_that("sample order invariant is enforced", {
  x <- popgenVCF::new_popgen_vcf_analysis(popgenVCF::default_config())
  x$samples$ids <- c("a", "b")
  x$samples$metadata <- data.table::data.table(sample = c("b", "a"), population = c("p", "p"))
  expect_error(popgenVCF::validate_analysis(x), "order")
})

test_that("LD SNPs must be a subset of QC SNPs", {
  x <- popgenVCF::new_popgen_vcf_analysis(popgenVCF::default_config())
  x$variants$qc_ids <- c(1L, 2L)
  x$variants$ld_ids <- c(1L, 3L)
  expect_error(popgenVCF::validate_analysis(x), "outside")
})

test_that("analysis objects survive serialization", {
  x <- popgenVCF::new_popgen_vcf_analysis(popgenVCF::default_config())
  f <- tempfile(fileext = ".rds")
  saveRDS(x, f)
  y <- readRDS(f)
  expect_identical(class(y), "PopgenVCFAnalysis")
  expect_true(popgenVCF::is_popgen_vcf_analysis(y))
})
