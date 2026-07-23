test_that("complete diversity results do not report an empty missing component", {
  result <- list(
    sample = data.table::data.table(
      sample = c("sample_1", "sample_2"),
      observed_heterozygosity = c(0.2, 0.3),
      missing_rate = c(0.01, 0.02)
    ),
    population = data.table::data.table(
      population = "population_A",
      observed_heterozygosity = 0.25,
      expected_heterozygosity = 0.28,
      inbreeding_coefficient = 0.1
    ),
    locus = data.table::data.table(
      snp_id = c("snp_1", "snp_2")
    )
  )

  validation <- popgenVCF:::validate_diversity_result(
    result,
    analysis = NULL,
    context = NULL
  )

  expect_true(validation$valid)
  expect_length(validation$errors, 0L)
  expect_identical(validation$metrics$samples, 2L)
  expect_identical(validation$metrics$populations, 1L)
})

test_that("diversity validation names only components that are actually missing", {
  result <- list(
    sample = data.table::data.table(sample = "sample_1"),
    population = data.table::data.table(population = "population_A")
  )

  validation <- popgenVCF:::validate_diversity_result(
    result,
    analysis = NULL,
    context = NULL
  )

  expect_false(validation$valid)
  expect_identical(validation$errors, "missing component 'locus'")
  expect_false(any(grepl("missing component ''", validation$errors, fixed = TRUE)))
})

test_that("non-list diversity results retain a specific validation error", {
  validation <- popgenVCF:::validate_diversity_result(
    c(sample_1 = 0.2),
    analysis = NULL,
    context = NULL
  )

  expect_false(validation$valid)
  expect_identical(validation$errors, "diversity result is not a list")
})
