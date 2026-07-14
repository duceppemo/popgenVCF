test_that("CLI recognizes documented options", {
  x <- popgenVCF:::parse_cli(c(
    "--config", "analysis.yml",
    "--vcf", "cohort.vcf.gz",
    "--metadata", "metadata.tsv",
    "--outdir", "results",
    "--threads", "8",
    "--seed", "42",
    "--maf", "0.05",
    "--max-sample-missing", "0.2",
    "--force-gds",
    "--no-report"
  ))
  expect_equal(x$config, "analysis.yml")
  expect_equal(x$vcf, "cohort.vcf.gz")
  expect_true(x$force_gds)
  expect_true(x$no_report)
})

test_that("CLI rejects unknown options", {
  expect_error(popgenVCF:::parse_cli(c("--not-real", "x")), "Unknown or incomplete")
})
