execution_ledger_fixture <- function() {
  new_persisted_execution_ledger(data.table::data.table(
    module = c("qc", "pca", "fst"),
    status = c("success", "success", "failed"),
    attempt = c(1L, 1L, 2L),
    elapsed_seconds = c(1.25, 2.5, 0.75)
  ))
}

rewrite_execution_ledger_sidecar <- function(path) {
  checksum <- digest::digest(file = path, algo = "sha256")
  writeLines(paste(checksum, basename(path)), paste0(path, ".sha256"), useBytes = TRUE)
}

test_that("execution ledgers round trip through integrity envelopes", {
  ledger <- execution_ledger_fixture()
  path <- tempfile(fileext = ".rds")

  write_execution_ledger(ledger, path)
  envelope <- readRDS(path)
  restored <- read_execution_ledger(path)

  expect_s3_class(envelope, "PopgenVCFRuntimeEnvelope")
  expect_identical(envelope$kind, "execution_ledger")
  expect_identical(envelope$schema, new_runtime_schema_metadata("execution_ledger"))
  expect_s3_class(restored, "PopgenVCFExecutionLedger")
  expect_true(data.table::is.data.table(restored))
  expect_identical(restored, ledger)
  expect_error(write_execution_ledger(ledger, path), "already exists")
})

test_that("execution ledger serialization is byte-for-byte deterministic", {
  ledger <- execution_ledger_fixture()
  first <- tempfile(fileext = ".rds")
  second <- tempfile(fileext = ".rds")

  write_execution_ledger(ledger, first)
  write_execution_ledger(ledger, second)

  expect_identical(
    readBin(first, "raw", n = file.info(first)$size),
    readBin(second, "raw", n = file.info(second)$size)
  )
})

test_that("execution ledger invariants fail closed", {
  expect_error(
    new_persisted_execution_ledger(data.frame(module = c("a", "a"), status = "success")),
    "unique"
  )
  expect_error(
    new_persisted_execution_ledger(data.frame(module = "a", status = "unknown")),
    "unsupported status"
  )
  expect_error(
    new_persisted_execution_ledger(data.frame(module = "a", status = "success", attempt = 0L)),
    "positive integers"
  )
})

test_that("scheduler ledgers remain accepted runtime ledgers", {
  ledger <- new_persisted_execution_ledger(data.table::data.table(
    module = c("qc", "pca"),
    status = c("pending", "blocked")
  ))
  expect_s3_class(ledger, "PopgenVCFExecutionLedger")
})

test_that("payload mutation fails after a valid file checksum", {
  path <- tempfile(fileext = ".rds")
  write_execution_ledger(execution_ledger_fixture(), path)
  envelope <- readRDS(path)
  envelope$payload$status[[1]] <- "failed"
  saveRDS(envelope, path, version = 3, compress = "xz")
  rewrite_execution_ledger_sidecar(path)

  expect_error(read_execution_ledger(path), "runtime integrity digest mismatch")
})

test_that("future and legacy execution ledger formats fail closed", {
  ledger <- execution_ledger_fixture()
  future <- tempfile(fileext = ".rds")
  write_execution_ledger(ledger, future)
  envelope <- readRDS(future)
  envelope$schema$version <- envelope$schema$version + 1L
  saveRDS(envelope, future, version = 3, compress = "xz")
  rewrite_execution_ledger_sidecar(future)
  expect_error(read_execution_ledger(future), "unsupported future runtime schema")

  legacy <- tempfile(fileext = ".rds")
  saveRDS(ledger, legacy, version = 3, compress = "xz")
  rewrite_execution_ledger_sidecar(legacy)
  expect_error(read_execution_ledger(legacy), "requires explicit migration")
})

test_that("file corruption and malformed sidecars are detected", {
  ledger <- execution_ledger_fixture()
  corrupted <- tempfile(fileext = ".rds")
  write_execution_ledger(ledger, corrupted)
  writeBin(as.raw(c(1, 2, 3)), corrupted)
  expect_error(read_execution_ledger(corrupted), "checksum mismatch")

  truncated <- tempfile(fileext = ".rds")
  write_execution_ledger(ledger, truncated)
  bytes <- readBin(truncated, "raw", n = file.info(truncated)$size)
  writeBin(bytes[seq_len(max(1L, length(bytes) %/% 3L))], truncated)
  rewrite_execution_ledger_sidecar(truncated)
  expect_error(read_execution_ledger(truncated), "unreadable or truncated")

  malformed <- tempfile(fileext = ".rds")
  write_execution_ledger(ledger, malformed)
  writeLines(character(), paste0(malformed, ".sha256"))
  expect_error(read_execution_ledger(malformed), "sidecar is malformed")
})
