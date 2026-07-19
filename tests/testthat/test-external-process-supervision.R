supervised_rscript <- function() {
  file.path(R.home("bin"), paste0("Rscript", .Platform$exeext))
}

supervision_script <- function(lines) {
  path <- tempfile(fileext = ".R")
  writeLines(lines, path, useBytes = TRUE)
  path
}

test_that("supervision policy validates", {
  policy <- new_external_process_supervision_policy(
    timeout_seconds = 5,
    resource_policy = new_execution_resource_policy(threads = 2),
    label = "test-supervision"
  )
  expect_s3_class(policy, "PopgenVCFExternalProcessSupervisionPolicy")
  expect_equal(policy$timeout_seconds, 5)
  expect_error(new_external_process_supervision_policy(0), "positive")
})

test_that("resource rejection occurs before launch", {
  command <- new_external_command(supervised_rscript(), "--version")
  policy <- new_external_process_supervision_policy(
    resource_policy = new_execution_resource_policy(threads = 1)
  )
  result <- run_supervised_external_command(
    command,
    requirements = new_module_resource_requirements(threads = 2),
    supervision_policy = policy
  )
  expect_identical(result$status, "resource_unavailable")
  expect_true("threads" %in% result$supervision$admission$exceeded)
  expect_identical(result$supervision$cleanup, "completed")
})

test_that("pre-launch cancellation fails closed", {
  token <- new_execution_cancellation_token("process-test")
  request_execution_cancellation(token, "stop requested")
  result <- run_supervised_external_command(
    new_external_command(supervised_rscript(), "--version"),
    cancellation_token = token
  )
  expect_identical(result$status, "cancelled")
  expect_match(result$error_message, "stop requested")
  expect_true(result$supervision$cancellation$requested)
})

test_that("timeouts receive a distinct state", {
  skip_on_cran()
  script <- supervision_script("Sys.sleep(3)")
  on.exit(unlink(script), add = TRUE)
  result <- run_supervised_external_command(
    new_external_command(supervised_rscript(), shQuote(script)),
    supervision_policy = new_external_process_supervision_policy(timeout_seconds = 1)
  )
  expect_identical(result$status, "timed_out")
  expect_identical(result$exit_status, 124L)
  expect_match(result$error_message, "exceeded timeout")
})

test_that("successful supervised commands retain provenance", {
  script <- supervision_script("cat('supervised-output')")
  on.exit(unlink(script), add = TRUE)
  result <- run_supervised_external_command(
    new_external_command(supervised_rscript(), shQuote(script), label = "supervised-r")
  )
  expect_identical(result$status, "success")
  expect_identical(result$stdout, "supervised-output")
  expect_identical(result$command_fingerprint, result$command$fingerprint)
  expect_identical(result$supervision$admission$status, "admitted")
  expect_identical(result$supervision$cleanup, "completed")
})
