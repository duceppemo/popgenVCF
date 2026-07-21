test_that("publication IBD output is deterministic and ordered", {
  spec <- new_publication_ibd_spec()
  pairs <- data.frame(
    sample1 = c("B", "C", "A"), sample2 = c("A", "A", "C"),
    genetic_distance = c(0.2, 0.4, 0.3), geographic_distance = c(20, 40, 30)
  )
  regression <- data.frame(term = c("intercept", "slope"), estimate = c(0.01, 0.005))
  permutation <- data.frame(statistic = "mantel_r", value = 0.7, p_value = 0.01)
  output <- new_publication_ibd_output(spec, pairs, regression, permutation, "result-sha")
  expect_identical(output$pairs$sample1, c("A", "A", "A"))
  expect_identical(output$pairs$sample2, c("B", "C", "C"))
  expect_true(validate_publication_ibd_output(output, spec))
  expect_match(publication_ibd_caption(output, spec), "3 unique")
  expect_identical(output$fingerprint,
                   new_publication_ibd_output(spec, pairs, regression, permutation, "result-sha")$fingerprint)
})

test_that("publication IBD contracts fail closed", {
  spec <- new_publication_ibd_spec()
  regression <- data.frame(term = "slope", estimate = 0.1)
  expect_error(new_publication_ibd_output(
    spec,
    data.frame(sample1 = "A", sample2 = "A", genetic_distance = 0.1, geographic_distance = 10),
    regression, result_fingerprint = "x"
  ), "distinct")
  expect_error(new_publication_ibd_output(
    spec,
    data.frame(sample1 = c("A", "B"), sample2 = c("B", "A"),
               genetic_distance = c(0.1, 0.1), geographic_distance = c(10, 10)),
    regression, result_fingerprint = "x"
  ), "unique")
  output <- new_publication_ibd_output(
    spec,
    data.frame(sample1 = "A", sample2 = "B", genetic_distance = 0.1, geographic_distance = 10),
    regression, result_fingerprint = "x"
  )
  output$source_data$pairs$genetic_distance <- 0.2
  expect_error(validate_publication_ibd_output(output, spec), "drifted")
})
