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
  expect_identical(result$supervision$backend, "processx")
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

test_that("timeouts receive a distinct state and cleanup provenance", {
  skip_on_cran()
  script <- supervision_script("Sys.sleep(3)")
  on.exit(unlink(script), add = TRUE)
  result <- run_supervised_external_command(
    new_external_command(supervised_rscript(), script),
    supervision_policy = new_external_process_supervision_policy(timeout_seconds = 1)
  )
  expect_identical(result$status, "timed_out")
  expect_identical(result$exit_status, 124L)
  expect_match(result$error_message, "exceeded timeout")
  expect_true(result$supervision$termination$requested)
  expect_identical(result$supervision$termination$reason, "timeout")
  expect_identical(result$supervision$termination$tree_cleanup, "completed")
})

test_that("timeout cleanup prevents descendant processes from surviving", {
  skip_on_cran()
  marker <- tempfile("descendant-marker-")
  child <- supervision_script(c(
    "args <- commandArgs(trailingOnly = TRUE)",
    "Sys.sleep(2)",
    "writeLines('survived', args[[1]])"
  ))
  parent <- supervision_script(c(
    sprintf("system2(%s, c(%s, %s), wait = FALSE)",
      dQuote(supervised_rscript()), dQuote(child), dQuote(marker)),
    "Sys.sleep(5)"
  ))
  on.exit(unlink(c(marker, child, parent), force = TRUE), add = TRUE)

  result <- run_supervised_external_command(
    new_external_command(supervised_rscript(), parent),
    supervision_policy = new_external_process_supervision_policy(timeout_seconds = 0.5)
  )
  Sys.sleep(2.5)

  expect_identical(result$status, "timed_out")
  expect_false(file.exists(marker))
  expect_identical(result$supervision$termination$tree_cleanup, "completed")
})

test_that("successful supervised commands retain output and provenance", {
  script <- supervision_script(c(
    "cat('supervised-output')",
    "message('supervised-error')"
  ))
  on.exit(unlink(script), add = TRUE)
  result <- run_supervised_external_command(
    new_external_command(supervised_rscript(), script, label = "supervised-r")
  )
  expect_identical(result$status, "success")
  expect_match(result$stdout, "supervised-output", fixed = TRUE)
  expect_match(result$stderr, "supervised-error", fixed = TRUE)
  expect_identical(result$command_fingerprint, result$command$fingerprint)
  expect_identical(result$supervision$admission$status, "admitted")
  expect_identical(result$supervision$backend, "processx")
  expect_identical(result$supervision$termination$tree_cleanup, "not_required")
  expect_identical(result$supervision$cleanup, "completed")
})

test_that("non-zero commands preserve exit status and output", {
  script <- supervision_script(c(
    "cat('partial-output')",
    "message('diagnostic-error')",
    "quit(status = 7L)"
  ))
  on.exit(unlink(script), add = TRUE)
  result <- run_supervised_external_command(
    new_external_command(supervised_rscript(), script)
  )
  expect_identical(result$status, "nonzero_exit")
  expect_identical(result$exit_status, 7L)
  expect_match(result$stdout, "partial-output", fixed = TRUE)
  expect_match(result$stderr, "diagnostic-error", fixed = TRUE)
})

test_that("run_supervised_line_command() preserves system2(stdout=TRUE, stderr=TRUE)'s return shape on success", {
  script <- supervision_script(c(
    "cat('line one\\n')",
    "cat('line two\\n')",
    "message('an error line')"
  ))
  on.exit(unlink(script), add = TRUE)
  run <- run_supervised_line_command(
    supervised_rscript(), script, dirname(script), timeout_seconds = 30
  )
  expect_identical(run$status, 0L)
  expect_false(run$timed_out)
  expect_true(is.character(run$output))
  expect_true(any(grepl("line one", run$output, fixed = TRUE)))
  expect_true(any(grepl("line two", run$output, fixed = TRUE)))
  expect_true(any(grepl("an error line", run$output, fixed = TRUE)))
})

test_that("run_supervised_line_command() reports a non-zero exit status like system2()", {
  script <- supervision_script(c("cat('failing')", "quit(status = 3L)"))
  on.exit(unlink(script), add = TRUE)
  run <- run_supervised_line_command(
    supervised_rscript(), script, dirname(script), timeout_seconds = 30
  )
  expect_identical(run$status, 3L)
  expect_false(run$timed_out)
  expect_true(any(grepl("failing", run$output, fixed = TRUE)))
})

test_that("run_supervised_line_command() kills a hung process instead of blocking forever", {
  skip_on_cran()
  script <- supervision_script("Sys.sleep(5)")
  on.exit(unlink(script), add = TRUE)
  elapsed <- system.time(
    run <- run_supervised_line_command(
      supervised_rscript(), script, dirname(script), timeout_seconds = 1
    )
  )
  expect_true(run$timed_out)
  expect_true(is.na(run$status))
  expect_true(any(grepl("exceeded timeout", run$output, fixed = TRUE)))
  # The process was actually killed, not just reported as timed out while
  # still running in the background -- the call returns close to the
  # 1-second timeout, not the full 5-second sleep.
  expect_lt(elapsed[["elapsed"]], 4)
})
