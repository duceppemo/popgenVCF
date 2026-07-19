external_process_result_fixture <- function() {
  command <- new_external_command(
    executable = R.home("bin/R"),
    args = c("--version"),
    working_directory = tempdir(),
    label = "R-version"
  )
  new_external_process_result(
    command = command,
    status = "success",
    exit_status = 0L,
    stdout = "R version",
    stderr = "",
    started_at = "2026-07-19 12:00:00 UTC",
    finished_at = "2026-07-19 12:00:01 UTC",
    elapsed_seconds = 1,
    resolved_executable = normalizePath(R.home("bin/R"), mustWork = TRUE),
    error_message = NA_character_
  )
}

test_that("external process results validate their command and status", {
  result <- external_process_result_fixture()
  expect_invisible(validate_external_process_result(result))

  changed <- result
  changed$command_fingerprint <- paste0("x", result$command_fingerprint)
  expect_error(
    validate_external_process_result(changed),
    "command fingerprint mismatch"
  )

  changed <- result
  changed$status <- "success"
  changed$exit_status <- 2L
  expect_error(
    validate_external_process_result(changed),
    "require exit status zero"
  )

  changed <- result
  changed$status <- "unsupported"
  expect_error(
    validate_external_process_result(changed),
    "unsupported status"
  )
})

test_that("external process result serialization is deterministic", {
  result <- external_process_result_fixture()
  first <- tempfile(fileext = ".rds")
  second <- tempfile(fileext = ".rds")
  on.exit(unlink(c(
    first, paste0(first, ".sha256"),
    second, paste0(second, ".sha256")
  )), add = TRUE)

  write_external_process_result(result, first)
  write_external_process_result(result, second)

  expect_identical(
    readBin(first, "raw", n = file.info(first)$size),
    readBin(second, "raw", n = file.info(second)$size)
  )
  restored <- read_external_process_result(first)
  expect_s3_class(restored, "PopgenVCFExternalProcessResult")
  expect_identical(restored, result)
})

test_that("external process result readers fail closed", {
  result <- external_process_result_fixture()
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(c(path, paste0(path, ".sha256"))), add = TRUE)
  write_external_process_result(result, path)

  writeLines("malformed", paste0(path, ".sha256"))
  expect_error(read_external_process_result(path), "sidecar is malformed")

  write_external_process_result(result, path, overwrite = TRUE)
  envelope <- readRDS(path)
  envelope$payload$status <- "nonzero_exit"
  saveRDS(envelope, path, version = 3, compress = "xz")
  writeLines(
    paste(external_process_result_sidecar_digest(path), basename(path)),
    paste0(path, ".sha256")
  )
  expect_error(
    read_external_process_result(path),
    "runtime integrity digest mismatch"
  )

  saveRDS(result, path, version = 3, compress = "xz")
  writeLines(
    paste(external_process_result_sidecar_digest(path), basename(path)),
    paste0(path, ".sha256")
  )
  expect_error(read_external_process_result(path), "explicit migration")
})
