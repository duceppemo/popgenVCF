cancellation_test_analysis <- function() {
  analysis <- new_popgen_vcf_analysis(default_config())
  analysis$samples$ids <- c("a", "b")
  analysis$samples$metadata <- data.table::data.table(
    sample = c("a", "b"), population = c("x", "y")
  )
  analysis$variants$qc_ids <- 1:2
  analysis$variants$ld_ids <- 1:2
  analysis
}

cancellation_result_module <- function(name, callback = NULL) {
  force(name)
  force(callback)
  function(analysis, context) {
    analysis <- set_analysis_result(analysis, name, list(module = name))
    if (is.function(callback)) callback()
    list(analysis = analysis, context = context)
  }
}

test_that("cancellation tokens validate and preserve the first request", {
  expect_error(new_execution_cancellation_token(""), "non-empty")
  token <- new_execution_cancellation_token("test-token")
  expect_false(token$requested)
  request_execution_cancellation(token, "operator request")
  request_execution_cancellation(token, "later request")
  expect_true(token$requested)
  expect_equal(token$reason, "operator request")
  expect_s3_class(token, "PopgenVCFExecutionCancellationToken")
})

test_that("cancellation is observed at the next module boundary", {
  token <- new_execution_cancellation_token("boundary-test")
  registry <- new_analysis_registry()
  registry <- register_analysis(
    registry,
    "first",
    cancellation_result_module(
      "first",
      function() request_execution_cancellation(token, "stop after first")
    )
  )
  registry <- register_analysis(
    registry, "second", cancellation_result_module("second"), requires = "first"
  )
  registry <- register_analysis(
    registry, "third", cancellation_result_module("third"), requires = "second"
  )

  result <- execute_analysis_registry_with_cancellation(
    cancellation_test_analysis(), list(), registry,
    cancellation_token = token
  )

  expect_equal(result$execution$status, c("success", "cancelled", "blocked"))
  expect_equal(result$order, "first")
  expect_false("second" %in% names(result$analysis$results))
  expect_equal(result$engine$cancellation$cancelled_modules, "second")
  expect_equal(result$engine$cancellation$reason, "stop after first")
  expect_s3_class(result$checkpoint, "PopgenVCFExecutionCheckpoint")
  expect_equal(result$checkpoint$completed, "first")
})

test_that("pre-requested cancellation accepts no module outputs", {
  token <- new_execution_cancellation_token()
  request_execution_cancellation(token, "cancel before start")
  registry <- new_analysis_registry()
  registry <- register_analysis(
    registry, "only", cancellation_result_module("only")
  )

  result <- execute_analysis_registry_with_cancellation(
    cancellation_test_analysis(), list(), registry,
    cancellation_token = token
  )

  expect_equal(result$execution$status, "cancelled")
  expect_length(result$order, 0)
  expect_false("only" %in% names(result$analysis$results))
  expect_length(result$checkpoint$completed, 0)
})

test_that("cancellation cannot be made retryable", {
  token <- new_execution_cancellation_token()
  request_execution_cancellation(token, "do not retry")
  attempts <- new.env(parent = emptyenv())
  attempts$n <- 0L
  registry <- new_analysis_registry()
  registry <- register_analysis(
    registry,
    "only",
    function(analysis, context) {
      attempts$n <- attempts$n + 1L
      cancellation_result_module("only")(analysis, context)
    }
  )
  retry <- new_execution_retry_policy(
    max_attempts = 3L,
    retryable = function(module, error_message, attempt, ledger) TRUE,
    label = "retry-everything"
  )

  result <- execute_analysis_registry_with_cancellation(
    cancellation_test_analysis(), list(), registry,
    cancellation_token = token,
    retry_policy = retry
  )

  expect_equal(attempts$n, 0L)
  expect_equal(result$execution$status, "cancelled")
  expect_equal(result$attempt_execution$status, "cancelled")
})
