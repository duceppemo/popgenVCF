test_that("quickstart_dataset_paths returns existing, matching files", {
  paths <- popgenVCF::quickstart_dataset_paths()
  expect_true(dir.exists(paths$directory))
  expect_true(file.exists(paths$vcf))
  expect_true(file.exists(paths$metadata))

  metadata <- data.table::fread(paths$metadata)
  expect_setequal(names(metadata), c("sample", "population"))
  expect_identical(nrow(metadata), 160L)
  expect_identical(data.table::uniqueN(metadata$population), 8L)

  # Both known real duplicate/MZ-twin pairs this dataset was deliberately
  # built around (confirmed against the real source this session) must be
  # present, including the cross-population one.
  expect_true(all(c("HG03873", "HG03998", "NA19331", "NA19334") %in% metadata$sample))
  expect_identical(metadata$population[metadata$sample == "HG03873"], "ITU")
  expect_identical(metadata$population[metadata$sample == "HG03998"], "STU")
})

test_that("quickstart_dataset_paths' VCF sample IDs exactly match the metadata", {
  skip_if(Sys.which("bcftools") == "", "bcftools is not available")
  paths <- popgenVCF::quickstart_dataset_paths()
  vcf_samples <- system2(Sys.which("bcftools"), c("query", "-l", shQuote(paths$vcf)), stdout = TRUE)
  metadata <- data.table::fread(paths$metadata)
  expect_setequal(vcf_samples, metadata$sample)
  expect_false(anyDuplicated(vcf_samples) > 0L)
})
