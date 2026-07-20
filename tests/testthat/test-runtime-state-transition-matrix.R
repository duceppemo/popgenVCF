test_that("runtime transition matrix is complete and terminal states are immutable", {
  matrix <- runtime_state_transition_matrix()
  expect_identical(rownames(matrix), colnames(matrix))
  expect_identical(sort(rownames(matrix)), sort(c(
    "pending", "running", "success", "failed", "blocked", "cancelled", "skipped"
  )))
  for (terminal in c("success", "cancelled", "skipped")) {
    expect_true(matrix[terminal, terminal])
    expect_false(any(matrix[terminal, setdiff(colnames(matrix), terminal)]))
  }
})

test_that("every transition is enforced table-wise", {
  matrix <- runtime_state_transition_matrix()
  for (from in rownames(matrix)) for (to in colnames(matrix)) {
    if (isTRUE(matrix[from, to])) {
      expect_invisible(validate_runtime_state_transition(from, to))
    } else {
      expect_error(validate_runtime_state_transition(from, to),
                   paste0("forbidden runtime state transition: ", from, " -> ", to),
                   fixed = TRUE)
    }
  }
  expect_error(validate_runtime_state_transition("unknown", "success"), "unsupported")
})

test_that("runtime histories accept retries and reject terminal regressions", {
  expect_invisible(validate_runtime_state_history(
    c("pending", "running", "failed", "running", "success"), "qc"
  ))
  expect_invisible(validate_runtime_state_history(c("failed", "success"), "qc"))
  expect_error(validate_runtime_state_history(c("success", "running"), "qc"),
               "success -> running.*qc")
  expect_error(validate_runtime_state_history(c("cancelled", "failed"), "pca"),
               "cancelled -> failed.*pca")
})
