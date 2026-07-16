test_that("sample aliases are optional and become public names", {
  metadata <- data.table::data.table(
    sample = c("FASTQ_A_R1.fastq.gz", "FASTQ_B_R1.fastq.gz"),
    Alias = c("Plant_01", "Plant_02"),
    population = c("north", "south")
  )
  names(metadata)[names(metadata) == "Alias"] <- "alias"
  normalized <- popgenVCF:::normalize_sample_aliases(metadata)

  expect_equal(normalized$sample, metadata$sample)
  expect_equal(normalized$display_sample, c("Plant_01", "Plant_02"))
  expect_equal(
    popgenVCF:::public_sample_ids(normalized, rev(metadata$sample)),
    c("Plant_02", "Plant_01")
  )
})

test_that("missing aliases fall back to VCF sample IDs", {
  metadata <- data.table::data.table(
    sample = c("vcf_a", "vcf_b", "vcf_c"),
    alias = c("Meaningful_A", "", NA_character_)
  )
  normalized <- popgenVCF:::normalize_sample_aliases(metadata)
  expect_equal(normalized$display_sample, c("Meaningful_A", "vcf_b", "vcf_c"))
})

test_that("aliases must resolve to globally unique public names", {
  expect_error(
    popgenVCF:::normalize_sample_aliases(data.table::data.table(
      sample = c("a", "b"), alias = c("same", "same")
    )),
    "aliases must be unique"
  )
  expect_error(
    popgenVCF:::normalize_sample_aliases(data.table::data.table(
      sample = c("a", "b"), alias = c("b", NA_character_)
    )),
    "globally unique"
  )
})

test_that("unbounded LD windows never cross the SNPRelate API as Inf", {
  expect_identical(popgenVCF:::normalize_ld_window_bp(Inf), .Machine$integer.max)
  expect_identical(
    popgenVCF:::normalize_ld_window_bp(.Machine$integer.max + 1000),
    .Machine$integer.max
  )
  expect_identical(popgenVCF:::normalize_ld_window_bp(500000), 500000L)
  expect_error(popgenVCF:::normalize_ld_window_bp(0), "positive")
})
