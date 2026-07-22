timeout_test_analysis <- function() {
  analysis <- new_popgen_vcf_analysis(default_config())
  analysis$samples$ids <- c("a", "b")
  analysis$samples$metadata <- data.table::data.table(
    sample = c("a", "b"), population = c("x", "y")
  )
  analysis$variants$qc_ids <- 1:2
  analysis$variants$ld_ids <- 1:2
  analysis
}

timeout_busy_wait <- function(seconds) {
  # Use an elapsed-time delay rather than processor-speed-dependent arithmetic.
  # setTimeLimit(elapsed = ...) is checked when control returns from Sys.sleep(),
  # making this regression stable across R release/devel and runner hardware.
  Sys.sleep(seconds)
  invisible(seconds)
}

timeout_module <- function(name, delay = 0) {
  force(name)
  force(delay)
  function(analysis, context) {
    if (delay > 0) timeout_busy_wait(delay)
    analysis <- set_analysis_result(analysis, name, list(module = name))
    list(analysis = analysis, context = context)
  }
}

timeout_test_registry <- function(delay = 0.25) {
  registry <- new_analysis_registry()
  registry <- register_analysis(registry, "first", timeout_module("first"))
  registry <- register_analysis(
    registry, "slow", timeout_module("slow", delay), requires = "first"
  )
  register_analysis(
    registry, "last", timeout_module("last"), requires = "slow"
  )
}

test_that("timeout policies validate explicit budgets", {
  expect_error(new_execution_timeout_policy(default_seconds = 0), "positive")
  expect_error(
    new_execution_timeout_policy(module_seconds = c(0.1, 0.2)),
    "uniquely named"
  )
  expect_error(
    new_execution_timeout_policy(module_seconds = c(a = 0.1, a = 0.2)),
    "uniquely named"
  )
  policy <- new_execution_timeout_policy(
    default_seconds = Inf, module_seconds = c(slow = 0.01), label = "test"
  )
  expect_s3_class(policy, "PopgenVCFExecutionTimeoutPolicy")
  expect_equal(timeout_budget(policy, "slow"), 0.01)
  expect_identical(timeout_budget(policy, "first"), Inf)
})

test_that("timed-out modules fail closed and block descendants", {
  result <- execute_analysis_registry_with_timeouts(
    timeout_test_analysis(), list(), timeout_test_registry(),
    timeout_policy = new_execution_timeout_policy(
      module_seconds = c(slow = 0.02), label = "short-slow-budget"
    )
  )

  expect_equal(result$execution$status, c("success", "timed_out", "blocked"))
  expect_match(result$execution$error_message[[2]], "Execution timeout")
  expect_equal(result$execution$blocked_by[[3]], "slow")
  expect_equal(result$order, "first")
  expect_false("slow" %in% names(result$analysis$results))
  expect_equal(result$engine$timeout$timed_out_modules, "slow")
  expect_equal(result$engine$timeout$policy, "short-slow-budget")
})

test_that("default timeout policy preserves normal execution", {
  result <- execute_analysis_registry_with_timeouts(
    timeout_test_analysis(), list(), timeout_test_registry(delay = 0)
  )
  expect_equal(result$execution$status, rep("success", 3))
  expect_length(result$engine$timeout$timed_out_modules, 0)
  expect_equal(result$engine$timeout$policy, "no-timeout")
})

test_that("timeout failures participate in bounded retries", {
  attempts <- new.env(parent = emptyenv())
  attempts$n <- 0L
  registry <- new_analysis_registry()
  registry <- register_analysis(registry, "first", timeout_module("first"))
  registry <- register_analysis(
    registry, "slow",
    function(analysis, context) {
      attempts$n <- attempts$n + 1L
      if (attempts$n == 1L) timeout_busy_wait(0.25)
      analysis <- set_analysis_result(analysis, "slow", list(module = "slow"))
      list(analysis = analysis, context = context)
    },
    requires = "first"
  )

  retry <- new_execution_retry_policy(
    max_attempts = 2L,
    retryable = function(module, error_message, attempt, ledger) {
      identical(module, "slow") && grepl("Execution timeout", error_message)
    },
    label = "retry-timeout"
  )
  result <- execute_analysis_registry_with_timeouts(
    timeout_test_analysis(), list(), registry,
    timeout_policy = new_execution_timeout_policy(module_seconds = c(slow = 0.02)),
    retry_policy = retry
  )

  expect_equal(attempts$n, 2L)
  expect_equal(result$execution$status, c("success", "success"))
  expect_true(result$execution$recovered[[2]])
  expect_equal(
    result$attempt_execution[module == "slow", status],
    c("timed_out", "success")
  )
})
