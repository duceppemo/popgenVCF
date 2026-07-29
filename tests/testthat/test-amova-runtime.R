test_that("AMOVA maps populations by immutable sample ID when aliases are used", {
  sample_ids <- paste0("s", seq_len(6L))
  aliases <- paste("Sample", seq_len(6L))
  metadata <- data.table::data.table(
    sample = rev(sample_ids),
    alias = rev(aliases),
    population = rev(rep(c("A", "B"), each = 3L))
  )

  strata <- popgenVCF:::amova_population_strata(
    sample_ids, metadata, aliases
  )

  expect_identical(rownames(strata), aliases)
  expect_identical(
    as.character(strata$population),
    rep(c("A", "B"), each = 3L)
  )
  expect_identical(nlevels(strata$population), 2L)
})

test_that("AMOVA reports a clear error when fewer than two populations remain", {
  metadata <- data.table::data.table(
    sample = c("s1", "s2", "s3"),
    population = "A"
  )

  expect_error(
    popgenVCF:::amova_population_strata(metadata$sample, metadata),
    "at least two populations after sample QC; found 1",
    fixed = TRUE
  )
})

test_that("AMOVA runs with public aliases and immutable sample IDs", {
  sample_ids <- paste0("s", seq_len(6L))
  aliases <- paste("Sample", seq_len(6L))
  metadata <- data.table::data.table(
    sample = sample_ids,
    alias = aliases,
    population = rep(c("A", "B"), each = 3L)
  )
  set.seed(27L)
  genotype <- matrix(
    sample(0:2, length(sample_ids) * 12L, replace = TRUE),
    nrow = length(sample_ids)
  )

  result <- suppressWarnings(popgenVCF:::run_amova_analysis(
    genotype, sample_ids, metadata, permutations = 9L, seed = 27L
  ))

  expect_s3_class(result$components, "data.table")
  expect_s3_class(result$phi, "data.table")
  expect_gt(nrow(result$components), 0L)
  expect_gt(nrow(result$phi), 0L)
})
