test_that("publication spatial output is deterministic and ordered", {
  spec <- new_publication_spatial_spec()
  coordinates <- data.frame(
    sample_id = c("C", "A", "B"),
    longitude = c(-70.3, -70.1, -70.2),
    latitude = c(46.3, 46.1, 46.2)
  )
  statistics <- data.frame(statistic = c("Moran_I", "Geary_C"), value = c(0.4, 0.7))
  neighborhoods <- data.frame(distance_class = c(20, 10), estimate = c(0.2, 0.3))
  permutation <- data.frame(statistic = "Moran_I", p_value = 0.01, permutations = 999)
  output <- new_publication_spatial_output(
    spec, coordinates, statistics, neighborhoods, permutation, "result-sha"
  )
  expect_identical(output$coordinates$sample_id, c("A", "B", "C"))
  expect_true(validate_publication_spatial_output(output, spec))
  expect_match(publication_spatial_caption(output, spec), "3 georeferenced")
  expect_identical(
    output$fingerprint,
    new_publication_spatial_output(
      spec, coordinates, statistics, neighborhoods, permutation, "result-sha"
    )$fingerprint
  )
})

test_that("publication spatial contracts fail closed", {
  spec <- new_publication_spatial_spec()
  statistics <- data.frame(statistic = "Moran_I", value = 0.4)
  expect_error(
    new_publication_spatial_output(
      spec,
      data.frame(sample_id = c("A", "A"), longitude = c(-70, -71), latitude = c(46, 47)),
      statistics, result_fingerprint = "x"
    ),
    "unique"
  )
  expect_error(
    new_publication_spatial_output(
      spec,
      data.frame(sample_id = "A", longitude = Inf, latitude = 46),
      statistics, result_fingerprint = "x"
    ),
    "finite"
  )
  output <- new_publication_spatial_output(
    spec,
    data.frame(sample_id = "A", longitude = -70, latitude = 46),
    statistics, result_fingerprint = "x"
  )
  output$source_data$coordinates$longitude <- -71
  expect_error(validate_publication_spatial_output(output, spec), "drifted")
})
