test_that("publication DAPC outputs are deterministic and ordered", {
  spec <- new_publication_dapc_spec(selected_k = 2L)
  coordinates <- data.frame(
    sample = c("b", "a", "c"), population = c("Y", "X", "Y"),
    cluster = c("2", "1", "2"), LD1 = c(2, 1, 3), LD2 = c(4, 3, 5)
  )
  membership <- matrix(
    c(0.2, 0.8, 0.9, 0.1, 0.3, 0.7), nrow = 3L, byrow = TRUE,
    dimnames = list(c("b", "a", "c"), c("cluster_1", "cluster_2"))
  )
  one <- new_publication_dapc_output(
    spec, coordinates, membership, result_fingerprint = "result-1"
  )
  two <- new_publication_dapc_output(
    spec, coordinates[c(3, 1, 2), ], membership[c("c", "b", "a"), ],
    result_fingerprint = "result-1"
  )
  expect_identical(one$fingerprint, two$fingerprint)
  expect_identical(one$coordinates$sample, c("a", "b", "c"))
  expect_identical(rownames(one$membership), c("a", "b", "c"))
  expect_true(validate_publication_dapc_output(one, spec))
})

test_that("membership identity, range, and row sums fail closed", {
  spec <- new_publication_dapc_spec(selected_k = 2L)
  coordinates <- data.frame(sample = c("a", "b"), LD1 = 1:2, LD2 = 3:4)
  membership <- matrix(c(0.8, 0.2, 0.1, 0.9), nrow = 2L, byrow = TRUE,
                       dimnames = list(c("a", "c"), c("one", "two")))
  expect_error(
    new_publication_dapc_output(spec, coordinates, membership, result_fingerprint = "result"),
    "do not match exactly"
  )
  rownames(membership) <- c("a", "b")
  membership[1, ] <- c(0.8, 0.3)
  expect_error(
    new_publication_dapc_output(spec, coordinates, membership, result_fingerprint = "result"),
    "sum to one"
  )
  membership[1, ] <- c(1.1, -0.1)
  expect_error(
    new_publication_dapc_output(spec, coordinates, membership, result_fingerprint = "result"),
    "between zero and one"
  )
})

test_that("selected K and style capacity fail closed", {
  coordinates <- data.frame(sample = c("a", "b"), LD1 = 1:2, LD2 = 3:4)
  membership <- matrix(c(0.8, 0.2, 0.1, 0.9), nrow = 2L, byrow = TRUE,
                       dimnames = list(c("a", "b"), c("one", "two")))
  spec <- new_publication_dapc_spec(selected_k = 3L)
  expect_error(
    new_publication_dapc_output(spec, coordinates, membership, result_fingerprint = "result"),
    "does not match selected_k"
  )

  report_spec <- new_publication_report_spec("manuscript", formats = "html")
  binding <- bind_publication_figure_style(
    report_spec, publication_layout_profile("general"),
    publication_figure_style_profile("grayscale-safe"), groups = 1L
  )
  expect_error(
    new_publication_dapc_output(
      new_publication_dapc_spec(selected_k = 2L), coordinates, membership,
      result_fingerprint = "result", figure_binding = binding
    ),
    "lacks capacity"
  )
})

test_that("source-data drift and mutation are detected", {
  spec <- new_publication_dapc_spec(selected_k = 2L)
  coordinates <- data.frame(sample = c("a", "b"), LD1 = 1:2, LD2 = 3:4)
  membership <- matrix(c(0.8, 0.2, 0.1, 0.9), nrow = 2L, byrow = TRUE,
                       dimnames = list(c("a", "b"), c("one", "two")))
  output <- new_publication_dapc_output(
    spec, coordinates, membership, result_fingerprint = "result"
  )
  output$source_data$membership$one[1] <- 0.5
  expect_error(validate_publication_dapc_output(output, spec), "drifted")
})

test_that("caption and report are stable", {
  spec <- new_publication_dapc_spec(selected_k = 2L)
  coordinates <- data.frame(sample = c("a", "b"), LD1 = 1:2, LD2 = 3:4)
  membership <- matrix(c(0.8, 0.2, 0.1, 0.9), nrow = 2L, byrow = TRUE,
                       dimnames = list(c("a", "b"), c("one", "two")))
  output <- new_publication_dapc_output(
    spec, coordinates, membership, result_fingerprint = "result"
  )
  expect_match(publication_dapc_caption(output, spec), "selected K = 2")
  expect_identical(publication_dapc_report(output, spec), publication_dapc_report(output, spec))
})
