test_that("canonical validation dataset contracts are deterministic", {
  checksum <- paste(rep("a", 64), collapse = "")
  spec <- new_canonical_validation_dataset(
    dataset_id = "canonical-demo", version = "1.0.0",
    source_uri = "https://example.org/dataset", license_id = "CC-BY-4.0",
    checksum_sha256 = checksum,
    samples = data.frame(sample_id = c("S2", "S1")),
    populations = data.frame(population = c("B", "A")),
    loci = data.frame(locus = c("L2", "L1")),
    expected_results = data.frame(metric = c("fst", "pi"), value = c(0.1, 0.01), tolerance = c(1e-6, 1e-6)),
    external_comparisons = data.frame(tool = "reference", version = "1.0")
  )
  expect_identical(spec$samples$sample_id, c("S1", "S2"))
  expect_true(validate_canonical_validation_dataset(spec))
  expect_true(any(grepl(
    "canonical-demo",
    canonical_validation_dataset_report(spec),
    fixed = TRUE
  )))
  expect_identical(spec$fingerprint, new_canonical_validation_dataset(
    "canonical-demo", "1.0.0", "https://example.org/dataset", "CC-BY-4.0", checksum,
    data.frame(sample_id = c("S2", "S1")), data.frame(population = c("B", "A")),
    data.frame(locus = c("L2", "L1")),
    data.frame(metric = c("fst", "pi"), value = c(0.1, 0.01), tolerance = c(1e-6, 1e-6)),
    data.frame(tool = "reference", version = "1.0"))$fingerprint)
})

test_that("canonical validation dataset contracts fail closed", {
  checksum <- paste(rep("b", 64), collapse = "")
  expect_error(new_canonical_validation_dataset(
    "x", "1", "source", "license", "bad",
    data.frame(sample_id = "S1"), data.frame(population = "A"),
    data.frame(locus = "L1"), data.frame(metric = "pi", value = 0.1)
  ), "SHA-256")
  spec <- new_canonical_validation_dataset(
    "x", "1", "source", "license", checksum,
    data.frame(sample_id = "S1"), data.frame(population = "A"),
    data.frame(locus = "L1"), data.frame(metric = "pi", value = 0.1)
  )
  spec$version <- "2"
  expect_error(validate_canonical_validation_dataset(spec), "Invalid")
})