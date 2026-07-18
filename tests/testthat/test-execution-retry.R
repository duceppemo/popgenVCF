retry_test_analysis <- function() {
  analysis <- new_popgen_vcf_analysis(default_config())
  analysis$samples$ids <- c("a", "b")
  analysis$samples$metadata <- data.table::data.table(
    sample = c("a", "b"), population = c("x", "y")
  )
  analysis$variants$qc_ids <- 1:2
  analysis$variants$ld_ids <- 1:2
  analysis
}

retry_success_module <- function(name, counter = NULL) {
  force(name)
  force(counter)
  function(analysis, context) {
    if (!is.null(counter)) counter[[name]] <- (counter[[name]] %||% 0L) + 1L
    analysis <- set_analysis_result(analysis, name, list(module = name))
    list(analysis = analysis, context = context)
  }
}

retry_registry <- function(failures = 1L, counter = NULL) {
  attempts <- new.env(parent = emptyenv())
  attempts$value <- 0L
  flaky <- function(analysis, context) {
    attempts$value <- attempts$value + 1L
    if (!is.null(counter)) counter$flaky <- (counter$flaky %||% 0L) + 1L
    if (attempts$value <= failures) stop("temporary service failure", call. = FALSE)
    analysis <- set_analysis_result(analysis, "flaky", list(module = "flaky"))
    list(analysis = analysis, context = context)
  }

  registry <- new_analysis_registry()
  registry <- register_analysis(
    registry, "first", retry_success_module("first", counter)
  )
  registry <- register_analysis(registry, "flaky", flaky, requires = "first")
  registry <- register_analysis(
    registry, "last", retry_success_module("last", counter), requires = "flaky"
  )
  list(registry = registry, attempts = attempts)
}

retry_transient_policy <- function(max_attempts = 3L) {
  new_execution_retry_policy(
    max_attempts = max_attempts,
    retryable = function(module, error_message, attempt, ledger) {
      identical(module, "flaky") && grepl("temporary", error_message, fixed = TRUE)
    },
    label = "transient-test"
  )
}

test_that("retry policies validate explicit bounds", {
  expect_error(new_execution_retry_policy(max_attempts = 0), "positive integer")
  expect_error(new_execution_retry_policy(retryable = TRUE), "must be a function")
  expect_error(new_execution_retry_policy(backoff_seconds = -1), "non-negative")
  expect_s3_class(retry_transient_policy(), "PopgenVCFExecutionRetryPolicy")
})

test_that("transient failures recover without rerunning validated prerequisites", {
  counter <- new.env(parent = emptyenv())
  fixture <- retry_registry(failures = 1L, counter = counter)

  result <- execute_analysis_registry_with_retries(
    retry_test_analysis(), list(), fixture$registry,
    retry_policy = retry_transient_policy()
  )

  expect_equal(counter$first, 1L)
  expect_equal(counter$flaky, 2L)
  expect_equal(counter$last, 1L)
  expect_equal(result$order, c("first", "flaky", "last"))
  expect_equal(result$execution$status, rep("success", 3))
  expect_equal(result$execution$attempt_count, c(1L, 2L, 1L))
  expect_equal(result$execution$recovered, c(FALSE, TRUE, FALSE))
  expect_equal(result$engine$retry$attempts_run, 2L)
  expect_equal(result$engine$retry$recovered_modules, "flaky")
  expect_equal(result$attempt_execution$attempt, c(1L, 1L, 1L, 2L, 2L))
})

test_that("non-retryable failures stop recovery deterministically", {
  fixture <- retry_registry(failures = 5L)
  policy <- new_execution_retry_policy(
    max_attempts = 3L,
    retryable = function(module, error_message, attempt, ledger) FALSE,
    label = "never"
  )

  result <- execute_analysis_registry_with_retries(
    retry_test_analysis(), list(), fixture$registry, retry_policy = policy
  )

  expect_equal(fixture$attempts$value, 1L)
  expect_equal(result$execution$status, c("success", "failed", "blocked"))
  expect_true(result$engine$retry$stopped_non_retryable)
  expect_equal(result$engine$retry$exhausted_modules, c("flaky", "last"))
})

test_that("retry exhaustion retains every failed attempt", {
  fixture <- retry_registry(failures = 5L)
  result <- execute_analysis_registry_with_retries(
    retry_test_analysis(), list(), fixture$registry,
    retry_policy = retry_transient_policy(max_attempts = 2L)
  )

  expect_equal(fixture$attempts$value, 2L)
  expect_equal(result$execution$status, c("success", "failed", "blocked"))
  expect_equal(result$execution$attempt_count, c(1L, 2L, 0L))
  expect_equal(result$engine$retry$attempts_run, 2L)
  expect_equal(
    result$attempt_execution[module == "flaky", status],
    c("failed", "failed")
  )
})

test_that("default retry policy performs one standard attempt", {
  fixture <- retry_registry(failures = 1L)
  result <- execute_analysis_registry_with_retries(
    retry_test_analysis(), list(), fixture$registry
  )

  expect_equal(fixture$attempts$value, 1L)
  expect_equal(result$engine$retry$policy, "no-retry")
  expect_equal(result$engine$retry$attempts_run, 1L)
})
