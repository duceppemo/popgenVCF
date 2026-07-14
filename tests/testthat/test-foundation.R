test_that("package identity and foundation files are consistent", {
  expect_equal(utils::packageDescription("popgenVCF")$Package, "popgenVCF")
  expect_true(nzchar(system.file("scripts", "popgenVCF", package = "popgenVCF")))
  expect_true(nzchar(system.file("extdata", "tiny.vcf", package = "popgenVCF")))
  expect_true(nzchar(system.file("extdata", "tiny_metadata.tsv", package = "popgenVCF")))
})

test_that("analysis object exposes only the primary class", {
  x <- popgenVCF::new_popgen_vcf_analysis(popgenVCF::default_config())
  expect_identical(class(x), "PopgenVCFAnalysis")
})
