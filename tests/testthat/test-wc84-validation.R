test_that("manual WC84 components reproduce the designed fixture", {
  paths <- popgenVCF:::validation_fixture_paths()
  dosage <- data.table::fread(paths$dosage, na.strings = "NA")
  metadata <- popgenVCF:::read_metadata(paths$metadata, "yes")
  genotype <- as.matrix(dosage[, -1L])
  rownames(genotype) <- dosage[[1L]]
  expected <- data.table::fread(paths$expected_variant_qc)
  keep <- expected[pass_combined == TRUE, variant_id]
  result <- popgenVCF:::manual_wc84_fst(
    genotype[, match(keep, colnames(genotype)), drop = FALSE],
    metadata$population
  )
  expect_equal(result$global, 7 / 27, tolerance = 1e-12)
  expect_equal(result$pairwise["PopA", "PopB"], 7 / 27, tolerance = 1e-12)
  expect_equal(diag(result$pairwise), c(PopA = 0, PopB = 0))
})

test_that("PCA and MDS metadata joins do not mutate shallow copies with :=", {
  scores <- data.table::data.table(sample = c("a", "b"), PC1 = c(-1, 1))
  metadata <- data.table::data.table(sample = c("a", "b"), population = c("A", "B"))
  data.table::set(scores, j = "population",
                  value = metadata$population[match(scores$sample, metadata$sample)])
  expect_equal(scores$population, c("A", "B"))
})
