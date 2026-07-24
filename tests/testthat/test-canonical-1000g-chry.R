test_that("approved 1000 Genomes source metadata is complete", {
  source <- canonical_1000g_chrY_source()
  expect_invisible(validate_canonical_source(source))
  expect_equal(source$id, "1000g_phase3_chry_v2a")
  expect_equal(source$doi, "10.5281/zenodo.3359882")
  expect_equal(nrow(source$files), 3L)
  expect_true(all(grepl("^[a-f0-9]{32}$", source$files$upstream_md5)))
})

test_that("approved chromosome 22 source pins the bounded autosomal callset", {
  source <- canonical_1000g_chr22_source()
  expect_invisible(validate_canonical_source(source))
  expect_equal(source$id, "1000g_phase3_chr22_v5a")
  expect_equal(source$chromosome_scope, "chr22")
  expect_equal(source$sample_sex_policy, "mixed")
  expect_identical(source$files$upstream_md5, c(
    "ad7d6e0c05edafd7faed7601f7f3eaba",
    "4202e9a481aa8103b471531a96665047",
    "7ee5675553088230530a7fe88c22f201"
  ))
})

test_that("source verification fails closed when files are absent", {
  source <- canonical_1000g_chrY_source()
  result <- verify_canonical_source(source, tempfile())
  expect_false(any(result$passed))
  expect_true(all(!result$exists))
  expect_error(canonical_dataset_from_source(source, tempfile()), "verification failed")
})

test_that("verified source files are promoted to SHA-256 descriptors", {
  directory <- tempfile()
  dir.create(directory)
  payloads <- c("vcf", "index", "panel")
  filenames <- c("panel.vcf.gz", "panel.vcf.gz.tbi", "samples.panel")
  for (i in seq_along(filenames)) writeBin(charToRaw(payloads[[i]]), file.path(directory, filenames[[i]]))

  source <- canonical_1000g_chrY_source()
  source$id <- "test_approved_source"
  source$files <- data.frame(
    filename = filenames,
    upstream_md5 = unname(tools::md5sum(file.path(directory, filenames))),
    source = rep(NA_character_, 3L),
    stringsAsFactors = FALSE
  )

  verification <- verify_canonical_source(source, directory)
  expect_true(all(verification$passed))
  expect_true(all(grepl("^[a-f0-9]{64}$", verification$sha256)))

  descriptor <- canonical_dataset_from_source(source, directory)
  expect_s3_class(descriptor, "PopgenVCFCanonicalDataset")
  expect_equal(descriptor$id, "test_approved_source")
  expect_true(all(grepl("^[a-f0-9]{64}$", descriptor$files$sha256)))

  registry <- register_canonical_dataset(
    new_canonical_dataset_registry(), descriptor,
    approval = "approved", reviewed_by = source$reviewed_by,
    reviewed_at = source$reviewed_at
  )
  expect_equal(list_canonical_datasets(registry)$approval, "approved")
})

test_that("approved source evidence is deterministic", {
  directory <- tempfile()
  dir.create(directory)
  filenames <- c("a.vcf.gz", "a.vcf.gz.tbi", "a.panel")
  for (file in filenames) writeLines(file, file.path(directory, file))
  source <- canonical_1000g_chrY_source()
  source$id <- "evidence_source"
  source$files <- data.frame(
    filename = filenames,
    upstream_md5 = unname(tools::md5sum(file.path(directory, filenames))),
    source = rep(NA_character_, 3L), stringsAsFactors = FALSE
  )
  paths <- write_approved_canonical_source_evidence(source, directory, tempfile())
  expect_true(all(file.exists(paths[!is.na(paths)])))
  evidence <- data.table::fread(paths[["source_verification"]])
  expect_true(all(evidence$passed))
})
