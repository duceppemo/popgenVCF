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

test_that("an environment override merges onto the current environment instead of replacing it", {
  # processx::run()'s own `env` REPLACES the entire child environment by
  # default -- confirmed directly before this fix: env = c(FOO = "bar")
  # alone left the child process with no PATH/HOME/etc. at all, unlike
  # run_external_command()'s additive system2(env = ...) path. A real
  # override (e.g. OMP_NUM_THREADS) would have silently broken whatever
  # tool it was meant to configure by wiping its PATH out from under it.
  script <- supervision_script(c(
    "cat(nzchar(Sys.getenv('PATH')), '\\n', sep = '')",
    "cat(Sys.getenv('POPGENVCF_SUPERVISION_ENV_TEST'), sep = '')"
  ))
  on.exit(unlink(script), add = TRUE)
  result <- run_supervised_external_command(
    new_external_command(
      supervised_rscript(), script,
      environment = c(POPGENVCF_SUPERVISION_ENV_TEST = "present"),
      label = "env-merge"
    )
  )
  expect_identical(result$status, "success")
  expect_match(result$stdout, "^TRUE", fixed = FALSE)
  expect_match(result$stdout, "present", fixed = TRUE)
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

test_that("a NUL byte in a child's output cannot hang the supervised runner", {
  # With stdout = "|", processx::run() (3.9.0) spins forever once a child
  # writes a NUL to ONE of its streams: the reader raises "embedded nul"
  # without consuming the byte, and since the child has already exited, run()'s
  # own timeout has nothing to kill. This is the runner behind ADMIXTURE and
  # fastStructure. Verified directly against the old code: a NUL on stdout
  # alone hangs, on stderr alone hangs, and on BOTH it does not hang but
  # returns "success" with the output mangled -- so all three are covered, the
  # single-stream cases first, since they are the ones that hang.
  #
  # The spin is in compiled code, so setTimeLimit() cannot break it (also
  # verified). The scenarios therefore run in a child R process that is killed
  # outright if it hangs: a regression fails this test instead of hanging the
  # whole suite.
  skip_on_cran()
  skip_on_os("windows")
  root <- tempfile("nul-output-"); dir.create(root)
  emit <- function(name, lines) {
    path <- file.path(root, name)
    writeLines(c("#!/bin/sh", lines), path)
    Sys.chmod(path, "0755")
    path
  }
  scripts <- c(
    stdout = emit("nul-stdout.sh", "printf 'before\\000after\\n'"),
    stderr = emit("nul-stderr.sh", "printf 'err\\000or\\n' >&2"),
    both = emit("nul-both.sh", c("printf 'before\\000after\\n'", "printf 'err\\000or\\n' >&2"))
  )

  load_package <- if (pkgload::is_dev_package("popgenVCF")) {
    sprintf("suppressPackageStartupMessages(pkgload::load_all(%s, quiet = TRUE))", deparse(pkgload::pkg_path()))
  } else {
    "suppressPackageStartupMessages(library(popgenVCF))"
  }
  child <- c(
    load_package,
    "policy <- new_external_process_supervision_policy(timeout_seconds = 20)",
    sprintf("scripts <- %s", paste(deparse(scripts), collapse = "")),
    "for (case in names(scripts)) {",
    sprintf("  cmd <- new_external_command(executable = scripts[[case]], working_directory = %s)", deparse(root)),
    "  res <- run_supervised_external_command(cmd, supervision_policy = policy)",
    "  cat('RESULT', case, res$status, '|', gsub('\\n', '', res$stdout), '|', gsub('\\n', '', res$stderr), '\\n')",
    "}"
  )
  child_script <- file.path(root, "child.R")
  writeLines(child, child_script)
  out_file <- file.path(root, "child.out")
  run <- processx::run(
    file.path(R.home("bin"), "Rscript"), child_script, timeout = 90,
    error_on_status = FALSE, stdout = out_file, stderr = file.path(root, "child.err")
  )
  expect_false(run$timeout)
  results <- grep("^RESULT", readLines(out_file), value = TRUE)
  expect_identical(trimws(results), c(
    "RESULT stdout success | beforeafter |",
    "RESULT stderr success |  | error",
    "RESULT both success | beforeafter | error"
  ))
})

test_that("read_captured_process_stream drops NUL bytes and makes invalid UTF-8 regex-safe", {
  path <- tempfile()
  writeBin(as.raw(c(0x61, 0x00, 0x62, 0x0a, 0x63, 0xe9, 0xff, 0x0a)), path)
  text <- popgenVCF:::read_captured_process_stream(path)
  expect_true(validUTF8(text))
  expect_identical(strsplit(text, "\n", fixed = TRUE)[[1L]], c("ab", "c<e9><ff>"))
  expect_no_error(grepl("CV error", text))
  expect_identical(popgenVCF:::read_captured_process_stream(tempfile()), "")
  empty <- tempfile(); file.create(empty)
  expect_identical(popgenVCF:::read_captured_process_stream(empty), "")
})
