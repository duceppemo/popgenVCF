test_that("package namespace is data.table-aware", {
  path <- system.file(
    "extdata", "validation", "core_validation_metadata.tsv",
    package = "popgenVCF"
  )
  expect_true(nzchar(path))

  metadata <- popgenVCF:::read_metadata(path, "yes")

  expect_s3_class(metadata, "data.table")
  expect_identical(names(metadata)[1:2], c("sample", "population"))
  expect_type(metadata$sample, "character")
  expect_type(metadata$population, "character")
})
