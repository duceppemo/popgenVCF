test_that("sample identity preserves immutable keys and richer groupings", {
  identity <- new_sample_identity(data.table::data.table(
    sample = c("vcf_a", "vcf_b", "vcf_c"),
    alias = c("Plant_02", "Plant_01", NA_character_),
    individual = c("ind1", "ind2", "ind3"),
    family = c("fam1", "fam1", "fam2"),
    replicate = c("rep1", "rep1", NA_character_),
    display_order = c(2L, 1L, 3L)
  ))
  expect_s3_class(identity, "PopgenVCFSampleIdentity")
  expect_equal(identity$sample, c("vcf_a", "vcf_b", "vcf_c"))
  expect_equal(identity$public_sample, c("Plant_02", "Plant_01", "vcf_c"))
  expect_equal(resolve_sample_identity(identity, c("vcf_c", "vcf_a")), c("vcf_c", "Plant_02"))
  expect_equal(sample_identity_table(identity, ordered = TRUE)$public_sample,
               c("Plant_01", "Plant_02", "vcf_c"))
})

test_that("sample identity permits grouping reuse but rejects identity collisions", {
  identity <- new_sample_identity(data.table::data.table(
    sample = c("a", "b"), family = c("family", "family"), replicate = c("r1", "r1")
  ))
  groups <- sample_identity_groups(identity)
  expect_equal(groups[grouping == "family", n_samples], 2L)
  expect_equal(groups[grouping == "replicate", n_samples], 2L)

  expect_error(new_sample_identity(data.table::data.table(sample = c("a", "a"))), "unique")
  expect_error(new_sample_identity(data.table::data.table(
    sample = c("a", "b"), alias = c("b", NA_character_)
  )), "globally unique")
  expect_error(new_sample_identity(data.table::data.table(
    sample = c("a", "b"), display_order = c(1L, 1L)
  )), "display_order")
})

test_that("legacy alias helpers use canonical identity", {
  metadata <- data.table::data.table(
    sample = c("raw_a", "raw_b"), alias = c("A", "B"),
    individual = c("i1", "i2"), display_order = c(2L, 1L)
  )
  normalized <- popgenVCF:::normalize_sample_aliases(metadata)
  expect_equal(normalized$display_sample, c("A", "B"))
  expect_equal(normalized$individual, c("i1", "i2"))
  expect_equal(popgenVCF:::public_sample_ids(normalized, c("raw_b", "raw_a")), c("B", "A"))
})
