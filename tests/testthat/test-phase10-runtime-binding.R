make_phase10_runtime_analysis <- function() {
  analysis <- new_popgen_vcf_analysis(default_config())
  analysis$samples$ids <- c("a", "b")
  analysis$samples$metadata <- data.table::data.table(
    sample = c("a", "b"), population = c("x", "y")
  )
  analysis$variants$qc_ids <- 1:2
  analysis$variants$ld_ids <- 1:2
  analysis
}

phase10_result_module <- function(name) {
  force(name)
  function(analysis, context) {
    analysis <- set_analysis_result(analysis, name, list(module = name))
    list(analysis = analysis, context = context)
  }
}

test_that("public analysis execution delegates to the unified engine", {
  registry <- new_analysis_registry()
  registry <- register_analysis(registry, "one", phase10_result_module("one"))
  request <- new_public_analysis_request(
    operation_id = "analysis.execute",
    analysis_id = "analysis-1"
  )

  response <- execute_public_analysis(
    request = request,
    analysis = make_phase10_runtime_analysis(),
    context = list(),
    registry = registry
  )

  expect_true(validate_public_analysis_response(response, request))
  expect_identical(response$status, "completed")
  expect_identical(response$scientific_values$completed_modules, "one")
  expect_true("one" %in% response$scientific_values$result_names)
})

test_that("public runtime failures use stable canonical errors", {
  registry <- new_analysis_registry()
  registry <- register_analysis(
    registry,
    "broken",
    function(analysis, context) stop("intentional failure", call. = FALSE)
  )
  request <- new_public_analysis_request(
    operation_id = "analysis.execute",
    analysis_id = "analysis-2"
  )

  response <- execute_public_analysis(
    request = request,
    analysis = make_phase10_runtime_analysis(),
    context = list(),
    registry = registry
  )

  expect_identical(response$status, "failed")
  expect_identical(response$error$code, "runtime_execution_failed")
  expect_match(response$error$message, "intentional failure")
  expect_true(validate_public_analysis_response(response, request))
})

test_that("public execution adapter rejects other operations", {
  request <- new_public_analysis_request(
    operation_id = "result.inspect",
    analysis_id = "analysis-3"
  )
  expect_error(
    execute_public_analysis(
      request,
      make_phase10_runtime_analysis(),
      list(),
      new_analysis_registry()
    ),
    "requires an analysis.execute request"
  )
})
