test_that("publication ordination outputs are deterministic and ordered", {
  spec <- new_publication_ordination_spec(group_column = "population")
  coordinates <- data.frame(sample_id = c("b", "a", "c"), PC1 = c(2, 1, 3), PC2 = c(4, 3, 5))
  metadata <- data.frame(sample_id = c("c", "a", "b"), population = c("Y", "X", "X"))
  one <- new_publication_ordination_output(spec, coordinates, metadata, c(0.6, 0.3), result_fingerprint = "result-1")
  two <- new_publication_ordination_output(spec, coordinates, metadata, c(60, 30), result_fingerprint = "result-1")
  expect_identical(one$fingerprint, two$fingerprint)
  expect_identical(one$scores$sample_id, c("a", "b", "c"))
  expect_identical(one$groups, c("X", "Y"))
  expect_equal(one$variance$variance_percent, c(60, 30))
  expect_true(validate_publication_ordination_output(one, spec))
})

test_that("metadata alignment fails closed", {
  spec <- new_publication_ordination_spec(group_column = "population")
  coordinates <- data.frame(sample_id = c("a", "b"), PC1 = c(1, 2), PC2 = c(3, 4))
  metadata <- data.frame(sample_id = c("a", "c"), population = c("X", "Y"))
  expect_error(
    new_publication_ordination_output(
      spec,
      coordinates,
      metadata = metadata,
      variance_explained = c(60, 30),
      result_fingerprint = "result"
    ),
    "do not match exactly"
  )
})

test_that("non-finite values and duplicate samples fail closed", {
  spec <- new_publication_ordination_spec()
  expect_error(new_publication_ordination_output(spec, data.frame(sample_id = c("a", "a"), PC1 = 1:2, PC2 = 3:4), result_fingerprint = "result"), "unique")
  expect_error(new_publication_ordination_output(spec, data.frame(sample_id = c("a", "b"), PC1 = c(1, Inf), PC2 = 3:4), result_fingerprint = "result"), "finite")
})

test_that("variance and loadings validation fail closed", {
  spec <- new_publication_ordination_spec()
  coordinates <- data.frame(sample_id = c("a", "b"), PC1 = 1:2, PC2 = 3:4)
  expect_error(new_publication_ordination_output(spec, coordinates, variance_explained = c(80, 30), result_fingerprint = "result"), "exceed 100")
  expect_error(new_publication_ordination_output(spec, coordinates, loadings = data.frame(PC1 = 1:2), result_fingerprint = "result"), "missing")
})

test_that("style capacity and mutation are detected", {
  report_spec <- new_publication_report_spec("manuscript", formats = "html")
  layout <- publication_layout_profile("general")
  style <- publication_figure_style_profile("grayscale-safe")
  binding <- bind_publication_figure_style(report_spec, layout, style, groups = 1L)
  spec <- new_publication_ordination_spec(group_column = "population")
  coordinates <- data.frame(sample_id = c("a", "b"), PC1 = 1:2, PC2 = 3:4)
  metadata <- data.frame(sample_id = c("a", "b"), population = c("X", "Y"))
  expect_error(new_publication_ordination_output(spec, coordinates, metadata, result_fingerprint = "result", figure_binding = binding), "lacks capacity")
  output <- new_publication_ordination_output(new_publication_ordination_spec(), coordinates, result_fingerprint = "result")
  output$scores$PC1[1] <- 99
  expect_error(validate_publication_ordination_output(output, new_publication_ordination_spec()), "drifted|fingerprint")
})

test_that("caption and report are stable", {
  spec <- new_publication_ordination_spec()
  coordinates <- data.frame(sample_id = c("a", "b"), PC1 = 1:2, PC2 = 3:4)
  output <- new_publication_ordination_output(spec, coordinates, variance_explained = c(60, 30), result_fingerprint = "result")
  expect_match(publication_ordination_caption(output, spec), "PCA ordination of 2 samples")
  expect_identical(publication_ordination_report(output, spec), publication_ordination_report(output, spec))
})
