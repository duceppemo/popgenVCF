test_that("numeric and exact external-reference comparisons are deterministic", {
  numeric <- compare_external_reference(
    c(a = 1, b = 2), c(a = 1, b = 2 + 1e-10),
    mode = "numeric", absolute_tolerance = 1e-8
  )
  expect_true(all(numeric$passed))
  expect_equal(numeric$metric, c("a", "b"))

  exact <- compare_external_reference(letters[1:3], letters[1:3], mode = "exact")
  expect_true(exact$passed)
  expect_false(compare_external_reference(1L, 1, mode = "exact")$passed)
})

test_that("matrix and subspace comparisons preserve scientific invariants", {
  matrix_reference <- matrix(1:4, 2, dimnames = list(c("s1", "s2"), c("v1", "v2")))
  matrix_result <- compare_external_reference(matrix_reference, matrix_reference, mode = "matrix")
  expect_true(all(matrix_result$passed))
  expect_equal(nrow(matrix_result), 4L)

  basis <- cbind(c(-1, 0, 1, 0), c(0, 1, 0, -1))
  rotated <- basis %*% matrix(c(0, -1, 1, 0), 2)
  subspace <- compare_external_reference(rotated, basis, mode = "subspace", absolute_tolerance = 1e-10)
  expect_true(all(subspace$passed))
  expect_equal(subspace$observed, c(1, 1), tolerance = 1e-10)
})

test_that("Q-matrix comparisons are label-switching invariant", {
  reference <- matrix(c(
    0.9, 0.1,
    0.8, 0.2,
    0.1, 0.9,
    0.2, 0.8
  ), ncol = 2, byrow = TRUE,
  dimnames = list(paste0("s", 1:4), c("cluster_1", "cluster_2")))
  observed <- reference[, 2:1, drop = FALSE]

  comparison <- compare_external_reference(observed, reference, mode = "q_matrix")
  expect_true(all(comparison$passed))
})

test_that("equivalence comparisons gate while diagnostics only record differences", {
  dataset <- list(value = 1)
  equivalence <- new_external_reference_spec(
    id = "scalar_equivalence", analysis = "fst", reference_tool = "reference",
    observed = function(x) x$value,
    reference = function(x) x$value + 0.1,
    absolute_tolerance = 1e-6,
    role = "equivalence"
  )
  diagnostic <- new_external_reference_spec(
    id = "scalar_diagnostic", analysis = "fst", reference_tool = "reference",
    observed = function(x) x$value,
    reference = function(x) x$value + 0.1,
    absolute_tolerance = 1e-6,
    role = "diagnostic",
    interpretation = "Different estimators are retained as a transparent diagnostic."
  )

  strict_result <- run_external_reference(equivalence, dataset)
  diagnostic_result <- run_external_reference(diagnostic, dataset)
  expect_equal(strict_result$status, "failed")
  expect_equal(diagnostic_result$status, "passed")
  expect_false(diagnostic_result$comparisons$passed)
  expect_match(diagnostic_result$message, "non-gating")
})

test_that("optional references skip transparently and adapter errors are recorded", {
  skipped <- new_external_reference_spec(
    id = "missing_reference", analysis = "pca", reference_tool = "optionalTool",
    observed = identity, reference = identity,
    requirements = function() "optionalTool is not installed"
  )
  skipped_result <- run_external_reference(skipped, 1)
  expect_equal(skipped_result$status, "skipped")
  expect_match(skipped_result$message, "not installed")

  broken <- new_external_reference_spec(
    id = "broken_reference", analysis = "ibs", reference_tool = "reference",
    observed = function(x) stop("adapter failed"), reference = identity
  )
  broken_result <- run_external_reference(broken, 1)
  expect_equal(broken_result$status, "error")
  expect_match(broken_result$message, "adapter failed")
})

test_that("external-reference tables retain scientific labels", {
  spec <- new_external_reference_spec(
    id = "table_fixture", analysis = "diversity", reference_tool = "adegenet",
    reference_version = "2.1", observed = function(x) x,
    reference = function(x) x, interpretation = "Observed heterozygosity equivalence."
  )
  result <- run_external_reference(spec, c(hobs = 0.5))
  tab <- external_reference_table(result)

  expect_equal(tab$status, "passed")
  expect_equal(tab$analysis, "diversity")
  expect_equal(tab$reference_tool, "adegenet")
  expect_equal(tab$metric, "hobs")
  expect_match(tab$interpretation, "heterozygosity")
})

test_that("external-reference contracts reject malformed specifications", {
  expect_error(
    new_external_reference_spec(
      id = "x", analysis = "pca", reference_tool = "tool",
      observed = 1, reference = identity
    ),
    "must be functions"
  )
  expect_error(
    new_external_reference_spec(
      id = "x", analysis = "pca", reference_tool = "tool",
      observed = identity, reference = identity,
      absolute_tolerance = -1
    ),
    "nonnegative"
  )
  expect_error(compare_external_reference(1:2, 1:3), "lengths differ")
})
