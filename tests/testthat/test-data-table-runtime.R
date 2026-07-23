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

test_that("sample QC filters the retained data.table column explicitly", {
  paths <- popgenVCF:::validation_fixture_paths()
  gds_path <- tempfile(fileext = ".gds")
  manifest_path <- paste0(gds_path, ".manifest.rds")
  gds <- popgenVCF:::prepare_gds(paths$vcf, gds_path, force = TRUE)
  on.exit({
    try(SNPRelate::snpgdsClose(gds), silent = TRUE)
    unlink(c(gds_path, manifest_path), force = TRUE)
  }, add = TRUE)

  ids <- popgenVCF:::get_gds_ids(gds)
  metadata <- popgenVCF:::read_metadata(paths$metadata, "yes")
  result <- popgenVCF:::harmonize_samples(
    gds = gds,
    ids = ids,
    metadata = metadata,
    max_missing = 1,
    metadata_supplied = TRUE
  )

  expect_equal(result$sample_ids, as.character(ids$sample))
  expect_true(all(result$qc$retained))
  expect_equal(result$metadata_match$retained_after_qc, rep(TRUE, length(ids$sample)))
})
