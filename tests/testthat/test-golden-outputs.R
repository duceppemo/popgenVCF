test_that("golden specifications and entries validate", {
  expect_error(new_golden_spec("x", absolute_tolerance = -1), "nonnegative")
  spec <- new_golden_spec("numeric", "numeric", absolute_tolerance = 1e-6)
  entry <- new_golden_entry(spec, c(a = 1, b = 2))
  expect_s3_class(entry, "PopgenVCFGoldenEntry")
  expect_invisible(validate_golden_entry(entry))

  broken <- entry
  broken$value[[1L]] <- 3
  expect_error(validate_golden_entry(broken), "digest mismatch")
})

test_that("exact and tolerant numerical comparisons gate correctly", {
  store <- new_golden_store(list(
    new_golden_entry(new_golden_spec("exact", "exact"), list(x = 1L)),
    new_golden_entry(new_golden_spec("numeric", "numeric", absolute_tolerance = 0.01), c(1, 2)),
    new_golden_entry(new_golden_spec("diagnostic", "numeric", absolute_tolerance = 0,
                                     role = "diagnostic"), 1)
  ))
  pass <- compare_golden_outputs(list(exact = list(x = 1L), numeric = c(1.001, 2), diagnostic = 2), store)
  expect_equal(pass$status, "passed")
  expect_equal(pass$comparisons[id == "diagnostic", status], "failed")

  fail <- compare_golden_outputs(list(exact = list(x = 2L), numeric = c(1, 2)), store)
  expect_equal(fail$status, "failed")
  expect_equal(fail$comparisons[id == "diagnostic", status], "skipped")
})

test_that("matrix, eigenspace and Q-matrix comparisons are invariant where appropriate", {
  matrix_value <- matrix(1:4, 2, dimnames = list(c("a", "b"), c("x", "y")))
  pca <- matrix(c(1, 0, 0, 1, 1, 1), nrow = 3)
  q <- matrix(c(.9, .1, .2, .8, .7, .3), ncol = 2, byrow = TRUE)
  store <- new_golden_store(list(
    new_golden_entry(new_golden_spec("matrix", "matrix", absolute_tolerance = 1e-8), matrix_value),
    new_golden_entry(new_golden_spec("pca", "eigenspace", absolute_tolerance = 1e-8), pca),
    new_golden_entry(new_golden_spec("q", "q_matrix", absolute_tolerance = 1e-8), q)
  ))
  observed <- list(matrix = matrix_value, pca = pca %*% diag(c(-1, 1)), q = q[, 2:1])
  result <- compare_golden_outputs(observed, store)
  expect_equal(result$status, "passed")
  expect_true(all(result$comparisons$passed))
})

test_that("golden replacement requires explicit approval", {
  original <- new_golden_entry(new_golden_spec("x"), 1)
  store <- new_golden_store(list(original))
  replacement <- new_golden_entry(new_golden_spec("x"), 2)
  expect_error(register_golden_entry(store, replacement), "already exists")
  expect_error(register_golden_entry(store, replacement, replace = TRUE), "requires")
  approved <- new_golden_entry(new_golden_spec("x"), 2,
                               approved_by = "maintainer",
                               approval_reason = "documented method change")
  updated <- register_golden_entry(store, approved, replace = TRUE)
  expect_equal(updated$entries$x$value, 2)
})

test_that("golden stores round-trip and detect corruption", {
  store <- new_golden_store(list(
    new_golden_entry(new_golden_spec("x", "numeric"), c(1, 2))
  ), metadata = list(dataset = "fixture"))
  path <- tempfile("golden-store-")
  write_golden_store(store, path)
  expect_true(verify_golden_store(path))
  restored <- read_golden_store(path)
  expect_identical(restored, store)
  expect_equal(golden_output_table(restored)$id, "x")

  cat("corruption", file = file.path(path, "entries", "x.rds"), append = TRUE)
  expect_error(verify_golden_store(path), "checksum mismatch")
})

test_that("missing entries and outputs are reported as skipped", {
  store <- new_golden_store(list(new_golden_entry(new_golden_spec("x"), 1)))
  result <- compare_golden_outputs(list(other = 1), store, ids = c("x", "missing"))
  expect_equal(result$comparisons$status, c("skipped", "skipped"))
  expect_equal(result$status, "passed")
})