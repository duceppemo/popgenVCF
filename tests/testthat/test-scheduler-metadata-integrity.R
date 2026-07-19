scheduler_metadata_fixture <- function() {
  new_scheduler_metadata(data.table::data.table(
    module = c("qc", "pca", "fst"),
    wave = c(1L, 2L, 2L),
    batch = c(1L, 2L, 2L),
    requires = c("", "qc", "qc"),
    dispatch_sequence = c(1L, 2L, 3L),
    completion_sequence = c(1L, 3L, 2L),
    merge_sequence = c(1L, 2L, 3L),
    worker_pid = c(101L, 102L, 103L)
  ))
}

rewrite_scheduler_metadata_sidecar <- function(path) {
  checksum <- digest::digest(file = path, algo = "sha256")
  writeLines(paste(checksum, basename(path)), paste0(path, ".sha256"), useBytes = TRUE)
}

test_that("scheduler metadata round trips through integrity envelopes", {
  metadata <- scheduler_metadata_fixture()
  path <- tempfile(fileext = ".rds")
  write_scheduler_metadata(metadata, path)
  envelope <- readRDS(path)
  restored <- read_scheduler_metadata(path)
  expect_s3_class(envelope, "PopgenVCFRuntimeEnvelope")
  expect_identical(envelope$kind, "scheduler_metadata")
  expect_s3_class(restored, "PopgenVCFSchedulerMetadata")
  expect_identical(restored, metadata)
})

test_that("scheduler metadata serialization is deterministic", {
  metadata <- scheduler_metadata_fixture()
  first <- tempfile(fileext = ".rds")
  second <- tempfile(fileext = ".rds")
  write_scheduler_metadata(metadata, first)
  write_scheduler_metadata(metadata, second)
  expect_identical(readBin(first, "raw", n = file.info(first)$size),
                   readBin(second, "raw", n = file.info(second)$size))
})

test_that("scheduler metadata invariants fail closed", {
  x <- as.data.frame(scheduler_metadata_fixture())
  x$module[[2]] <- "qc"
  expect_error(new_scheduler_metadata(x), "unique and non-empty")
  x <- as.data.frame(scheduler_metadata_fixture())
  x$requires[[1]] <- "missing"
  expect_error(new_scheduler_metadata(x), "unknown dependencies")
  x <- as.data.frame(scheduler_metadata_fixture())
  x$dispatch_sequence <- c(1L, 1L, 2L)
  expect_error(new_scheduler_metadata(x), "unique positive integers")
})

test_that("scheduler metadata mutation and incompatible formats fail closed", {
  path <- tempfile(fileext = ".rds")
  write_scheduler_metadata(scheduler_metadata_fixture(), path)
  envelope <- readRDS(path)
  envelope$payload$batch[[1]] <- 9L
  saveRDS(envelope, path, version = 3, compress = "xz")
  rewrite_scheduler_metadata_sidecar(path)
  expect_error(read_scheduler_metadata(path), "runtime integrity digest mismatch")

  future <- tempfile(fileext = ".rds")
  write_scheduler_metadata(scheduler_metadata_fixture(), future)
  envelope <- readRDS(future)
  envelope$schema$version <- envelope$schema$version + 1L
  saveRDS(envelope, future, version = 3, compress = "xz")
  rewrite_scheduler_metadata_sidecar(future)
  expect_error(read_scheduler_metadata(future), "unsupported future runtime schema")
})

test_that("scheduler metadata corruption and malformed sidecars are detected", {
  path <- tempfile(fileext = ".rds")
  write_scheduler_metadata(scheduler_metadata_fixture(), path)
  writeBin(as.raw(c(1, 2, 3)), path)
  expect_error(read_scheduler_metadata(path), "checksum mismatch")

  malformed <- tempfile(fileext = ".rds")
  write_scheduler_metadata(scheduler_metadata_fixture(), malformed)
  writeLines(character(), paste0(malformed, ".sha256"))
  expect_error(read_scheduler_metadata(malformed), "sidecar is malformed")
})
