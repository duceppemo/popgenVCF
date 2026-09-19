attempt_ledger_fixture <- function() {
  data.table::data.table(
    module = c("pca", "fst", "pca"),
    status = c("failed", "success", "success"),
    attempt = c(1L, 1L, 2L),
    error_message = c("transient failure", NA_character_, NA_character_)
  )
}

test_that("attempt ledgers validate retry chains", {
  ledger <- new_attempt_ledger(attempt_ledger_fixture())
  expect_s3_class(ledger, "PopgenVCFAttemptLedger")
  expect_invisible(validate_attempt_ledger(ledger))

  duplicate <- data.table::rbindlist(list(ledger, ledger[1]))
  expect_error(
    new_attempt_ledger(duplicate),
    "module-attempt pairs must be unique"
  )

  gap <- data.table::copy(attempt_ledger_fixture())
  gap[module == "pca" & attempt == 2L, attempt := 3L]
  expect_error(new_attempt_ledger(gap), "retry chain is not contiguous")

  after_success <- data.table::data.table(
    module = c("pca", "pca"),
    status = c("success", "failed"),
    attempt = c(1L, 2L)
  )
  expect_error(new_attempt_ledger(after_success), "after a terminal state")

  # Real bug: ledger[module == name] is ambiguous once the ledger carries
  # a column literally named "name" -- data.table resolves a bare symbol
  # against the data.table's own columns before the validation loop's own
  # `name` variable, so this silently compared `module` against the wrong
  # values and returned zero rows for every module, skipping the
  # terminal-state check entirely. An extra "name" column is otherwise
  # unremarkable/permitted data, so this must still be caught.
  after_success_with_name_column <- data.table::data.table(
    module = c("pca", "pca"),
    status = c("success", "failed"),
    attempt = c(1L, 2L),
    name = c("some sample", "another sample")
  )
  expect_error(new_attempt_ledger(after_success_with_name_column), "after a terminal state")
})

test_that("attempt ledger serialization is deterministic", {
  ledger <- new_attempt_ledger(attempt_ledger_fixture())
  first <- tempfile(fileext = ".rds")
  second <- tempfile(fileext = ".rds")
  on.exit(unlink(c(first, paste0(first, ".sha256"), second,
                   paste0(second, ".sha256"))), add = TRUE)

  write_attempt_ledger(ledger, first)
  write_attempt_ledger(ledger, second)

  expect_identical(readBin(first, "raw", n = file.info(first)$size),
                   readBin(second, "raw", n = file.info(second)$size))
  restored <- read_attempt_ledger(first)
  expect_s3_class(restored, "PopgenVCFAttemptLedger")
  expect_identical(restored, ledger)
})

test_that("attempt ledger readers fail closed", {
  ledger <- new_attempt_ledger(attempt_ledger_fixture())
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(c(path, paste0(path, ".sha256"))), add = TRUE)
  write_attempt_ledger(ledger, path)

  writeLines("malformed", paste0(path, ".sha256"))
  expect_error(read_attempt_ledger(path), "sidecar is malformed")

  write_attempt_ledger(ledger, path, overwrite = TRUE)
  envelope <- readRDS(path)
  envelope$payload$attempt[[1]] <- 9L
  saveRDS(envelope, path, version = 3, compress = "xz")
  writeLines(
    paste(attempt_ledger_sidecar_digest(path), basename(path)),
    paste0(path, ".sha256")
  )
  expect_error(read_attempt_ledger(path), "runtime integrity digest mismatch")

  saveRDS(ledger, path, version = 3, compress = "xz")
  writeLines(
    paste(attempt_ledger_sidecar_digest(path), basename(path)),
    paste0(path, ".sha256")
  )
  expect_error(read_attempt_ledger(path), "explicit migration")
})