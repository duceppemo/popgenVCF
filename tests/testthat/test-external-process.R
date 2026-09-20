external_process_script <- function(lines) {
  path <- tempfile(fileext = ".R")
  writeLines(lines, path, useBytes = TRUE)
  path
}

rscript_executable <- function() {
  file.path(R.home("bin"), paste0("Rscript", .Platform$exeext))
}

test_that("external command specifications validate deterministically", {
  command <- new_external_command(
    rscript_executable(),
    args = "--version",
    environment = c(Z_TEST = "2", A_TEST = "1"),
    label = "r-version"
  )

  expect_s3_class(command, "PopgenVCFExternalCommand")
  expect_identical(names(command$environment), c("A_TEST", "Z_TEST"))
  expect_match(command$fingerprint, "^[0-9a-f]{64}$")
  expect_invisible(validate_external_command(command))

  changed <- command
  changed$args <- "--help"
  expect_error(validate_external_command(changed), "fingerprint")
  expect_error(new_external_command(""), "non-empty")
  expect_error(
    new_external_command(rscript_executable(), environment = c("1", "2")),
    "uniquely named"
  )
})

test_that("successful external commands capture stdout and stderr", {
  script <- external_process_script(c(
    "cat('standard-output')",
    "message('standard-error')"
  ))
  on.exit(unlink(script), add = TRUE)

  result <- run_external_command(new_external_command(
    rscript_executable(),
    args = script,
    label = "capture-output"
  ))

  expect_s3_class(result, "PopgenVCFExternalProcessResult")
  expect_identical(result$status, "success")
  expect_identical(result$exit_status, 0L)
  expect_match(result$stdout, "standard-output", fixed = TRUE)
  expect_match(result$stderr, "standard-error", fixed = TRUE)
  expect_gte(result$elapsed_seconds, 0)
  expect_identical(result$command_fingerprint, result$command$fingerprint)
})

test_that("non-zero exits remain distinct from launch failures", {
  script <- external_process_script(c(
    "cat('partial-output')",
    "message('failure-detail')",
    "quit(status = 7L)"
  ))
  on.exit(unlink(script), add = TRUE)

  failed <- run_external_command(new_external_command(
    rscript_executable(),
    args = script,
    label = "nonzero"
  ))
  missing <- run_external_command(new_external_command(
    paste0("popgenvcf-missing-executable-", Sys.getpid()),
    label = "missing"
  ))

  expect_identical(failed$status, "nonzero_exit")
  expect_identical(failed$exit_status, 7L)
  expect_match(failed$stdout, "partial-output", fixed = TRUE)
  expect_match(failed$stderr, "failure-detail", fixed = TRUE)
  expect_identical(missing$status, "launch_failed")
  expect_true(is.na(missing$exit_status))
  expect_match(missing$error_message, "Executable not found")
})

test_that("working directory and environment are explicit", {
  directory <- tempfile("popgenvcf-process-wd-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  script <- external_process_script(c(
    "cat(normalizePath(getwd()), '\\n', sep = '')",
    "cat(Sys.getenv('POPGENVCF_PROCESS_TEST'))"
  ))
  on.exit(unlink(script), add = TRUE)

  result <- run_external_command(new_external_command(
    rscript_executable(),
    args = script,
    working_directory = directory,
    environment = c(POPGENVCF_PROCESS_TEST = "visible"),
    label = "context"
  ))

  expect_identical(result$status, "success")
  expect_match(result$stdout, normalizePath(directory), fixed = TRUE)
  expect_match(result$stdout, "visible", fixed = TRUE)
})

test_that("run_external_command shell-quotes args and environment values, not just the executable", {
  # base::system2() only shQuote()s the executable itself -- args and env
  # are pasted into its internally-built shell command line unquoted. An
  # arg or env value containing shell metacharacters (or a plain space)
  # previously executed as shell syntax instead of being passed through
  # literally; confirmed directly before this test was added (an injected
  # `; touch ...; ` arg created the file). Rscript's own commandArgs()
  # reports each argument exactly as the OS delivered it to the process,
  # so this exercises the real, full round trip, not just the constructed
  # command string.
  marker <- tempfile("popgenvcf-injection-marker-")
  on.exit(unlink(marker), add = TRUE)
  injected_arg <- sprintf("; touch %s; echo", shQuote(marker))
  script <- external_process_script(c(
    "args <- commandArgs(trailingOnly = TRUE)",
    "cat(args[[1]], sep = '')",
    "cat('|', Sys.getenv('POPGENVCF_INJECTION_TEST'), sep = '')"
  ))
  on.exit(unlink(script), add = TRUE)

  result <- run_external_command(new_external_command(
    rscript_executable(),
    args = c(script, injected_arg),
    environment = c(POPGENVCF_INJECTION_TEST = "has space; and a semicolon"),
    label = "injection-guard"
  ))

  expect_identical(result$status, "success")
  expect_false(file.exists(marker))
  expect_match(result$stdout, injected_arg, fixed = TRUE)
  expect_match(result$stdout, "has space; and a semicolon", fixed = TRUE)
})

test_that("run_external_command keeps output usable when a tool prints a NUL or a non-UTF-8 byte", {
  skip_on_os("windows")
  root <- tempfile("external-bytes-"); dir.create(root)
  script <- file.path(root, "emit.sh")
  writeLines(c("#!/bin/sh", "echo one", "printf 'mid\\000dle\\n'", "printf 'caf\\351\\n'", "echo three"), script)
  Sys.chmod(script, "0755")
  result <- run_external_command(new_external_command(script, working_directory = root))
  expect_identical(result$status, "success")
  expect_identical(result$stdout, "one\nmiddle\ncaf<e9>\nthree")
  expect_true(validUTF8(result$stdout))
  expect_no_warning(expect_true(grepl("three", result$stdout)))
  expect_identical(result$stderr, "")
})
