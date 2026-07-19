async_rscript <- function() {
  file.path(R.home("bin"), paste0("Rscript", .Platform$exeext))
}

async_script <- function(lines) {
  path <- tempfile(fileext = ".R")
  writeLines(lines, path, useBytes = TRUE)
  path
}

test_that("asynchronous handles complete successful commands", {
  script <- async_script(c(
    "cat('first\\n')",
    "flush.console()",
    "Sys.sleep(0.2)",
    "cat('second\\n')"
  ))
  on.exit(unlink(script), add = TRUE)

  handle <- start_supervised_external_command(
    new_external_command(async_rscript(), script, label = "async-success")
  )
  expect_s3_class(handle, "PopgenVCFExternalProcessHandle")
  expect_true(handle$state %in% c("running", "success"))

  result <- finalize_supervised_external_command(handle)
  expect_identical(result$status, "success")
  expect_match(result$stdout, "first")
  expect_match(result$stdout, "second")
  expect_identical(result$supervision$backend, "processx-async")
  expect_identical(result$supervision$lifecycle_events$sequence,
                   seq_len(nrow(result$supervision$lifecycle_events)))
})

test_that("asynchronous polling collects output without duplication", {
  script <- async_script(c(
    "cat('alpha')",
    "flush.console()",
    "Sys.sleep(0.5)",
    "cat('beta')"
  ))
  on.exit(unlink(script), add = TRUE)
  handle <- start_supervised_external_command(
    new_external_command(async_rscript(), script)
  )

  Sys.sleep(0.2)
  poll_supervised_external_command(handle)
  first <- read_supervised_external_output(handle)$stdout
  second <- read_supervised_external_output(handle)$stdout
  expect_identical(first, second)

  result <- finalize_supervised_external_command(handle)
  expect_identical(result$stdout, "alphabeta")
})

test_that("mid-process cancellation terminates execution", {
  skip_on_cran()
  script <- async_script("Sys.sleep(10)")
  on.exit(unlink(script), add = TRUE)
  token <- new_execution_cancellation_token("async-cancel")
  handle <- start_supervised_external_command(
    new_external_command(async_rscript(), script),
    cancellation_token = token,
    termination_grace_seconds = 0.1
  )

  request_execution_cancellation(token, "cancel during execution")
  poll_supervised_external_command(handle)
  result <- finalize_supervised_external_command(handle)

  expect_identical(result$status, "cancelled")
  expect_match(result$error_message, "cancel during execution")
  expect_true("termination_requested" %in%
                result$supervision$lifecycle_events$event)
})

test_that("asynchronous timeout is fail-closed", {
  skip_on_cran()
  script <- async_script("Sys.sleep(10)")
  on.exit(unlink(script), add = TRUE)
  handle <- start_supervised_external_command(
    new_external_command(async_rscript(), script),
    supervision_policy = new_external_process_supervision_policy(
      timeout_seconds = 0.2
    ),
    termination_grace_seconds = 0.1
  )

  result <- finalize_supervised_external_command(handle)
  expect_identical(result$status, "timed_out")
  expect_identical(result$exit_status, 124L)
  expect_match(result$error_message, "exceeded timeout")
})

test_that("resource rejection and launch failures finalize without a process", {
  rejected <- start_supervised_external_command(
    new_external_command(async_rscript(), "--version"),
    requirements = new_module_resource_requirements(threads = 2),
    supervision_policy = new_external_process_supervision_policy(
      resource_policy = new_execution_resource_policy(threads = 1)
    )
  )
  expect_identical(finalize_supervised_external_command(rejected)$status,
                   "resource_unavailable")

  missing <- start_supervised_external_command(
    new_external_command("definitely-not-a-popgenvcf-command")
  )
  expect_identical(finalize_supervised_external_command(missing)$status,
                   "launch_failed")
})
