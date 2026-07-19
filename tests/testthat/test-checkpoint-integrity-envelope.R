checkpoint_envelope_fixture <- function() {
  registry <- checkpoint_registry()
  checkpoint <- new_execution_checkpoint(
    execute_analysis_registry(checkpoint_analysis(), list(), registry),
    registry
  )
  list(registry = registry, checkpoint = checkpoint)
}

rewrite_checkpoint_sidecar <- function(path) {
  checksum <- digest::digest(file = path, algo = "sha256")
  writeLines(paste(checksum, basename(path)), paste0(path, ".sha256"), useBytes = TRUE)
}

test_that("checkpoint files contain versioned integrity envelopes", {
  fixture <- checkpoint_envelope_fixture()
  path <- tempfile(fileext = ".rds")

  write_execution_checkpoint(fixture$checkpoint, path)
  envelope <- readRDS(path)

  expect_s3_class(envelope, "PopgenVCFRuntimeEnvelope")
  expect_identical(envelope$kind, "checkpoint")
  expect_identical(envelope$schema, new_runtime_schema_metadata("checkpoint"))
  expect_identical(runtime_integrity_payload(envelope), fixture$checkpoint)
  expect_identical(
    read_execution_checkpoint(path, fixture$registry),
    fixture$checkpoint
  )
})

test_that("checkpoint serialization is byte-for-byte deterministic", {
  fixture <- checkpoint_envelope_fixture()
  first <- tempfile(fileext = ".rds")
  second <- tempfile(fileext = ".rds")

  write_execution_checkpoint(fixture$checkpoint, first)
  write_execution_checkpoint(fixture$checkpoint, second)

  expect_identical(readBin(first, "raw", n = file.info(first)$size),
                   readBin(second, "raw", n = file.info(second)$size))
  expect_identical(
    digest::digest(file = first, algo = "sha256"),
    digest::digest(file = second, algo = "sha256")
  )
})

test_that("inner payload mutation fails after a valid file checksum", {
  fixture <- checkpoint_envelope_fixture()
  path <- tempfile(fileext = ".rds")
  write_execution_checkpoint(fixture$checkpoint, path)

  envelope <- readRDS(path)
  envelope$payload$completed <- character()
  saveRDS(envelope, path, version = 3, compress = "xz")
  rewrite_checkpoint_sidecar(path)

  expect_error(
    read_execution_checkpoint(path, fixture$registry),
    "runtime integrity digest mismatch"
  )
})

test_that("unsupported future checkpoint envelopes fail closed", {
  fixture <- checkpoint_envelope_fixture()
  path <- tempfile(fileext = ".rds")
  write_execution_checkpoint(fixture$checkpoint, path)

  envelope <- readRDS(path)
  envelope$schema$version <- envelope$schema$version + 1L
  saveRDS(envelope, path, version = 3, compress = "xz")
  rewrite_checkpoint_sidecar(path)

  expect_error(
    read_execution_checkpoint(path, fixture$registry),
    "unsupported future runtime schema"
  )
})

test_that("legacy unwrapped checkpoints require migration", {
  fixture <- checkpoint_envelope_fixture()
  path <- tempfile(fileext = ".rds")
  saveRDS(fixture$checkpoint, path, version = 3, compress = "xz")
  rewrite_checkpoint_sidecar(path)

  expect_error(
    read_execution_checkpoint(path, fixture$registry),
    "legacy unwrapped execution checkpoint requires explicit migration"
  )
})

test_that("truncated checkpoint files are classified explicitly", {
  fixture <- checkpoint_envelope_fixture()
  path <- tempfile(fileext = ".rds")
  write_execution_checkpoint(fixture$checkpoint, path)

  bytes <- readBin(path, "raw", n = file.info(path)$size)
  writeBin(bytes[seq_len(max(1L, length(bytes) %/% 3L))], path)
  rewrite_checkpoint_sidecar(path)

  expect_error(
    read_execution_checkpoint(path, fixture$registry),
    "unreadable or truncated"
  )
})

test_that("malformed checkpoint sidecars fail closed", {
  fixture <- checkpoint_envelope_fixture()
  path <- tempfile(fileext = ".rds")
  write_execution_checkpoint(fixture$checkpoint, path)
  writeLines(character(), paste0(path, ".sha256"))

  expect_error(
    read_execution_checkpoint(path, fixture$registry),
    "sidecar is malformed"
  )
})
