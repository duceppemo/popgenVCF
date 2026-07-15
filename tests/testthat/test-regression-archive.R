test_that("release benchmark records validate and retain component identity", {
  record <- new_release_benchmark_record(
    release = "v0.10.0-test", package_version = "0.10.0",
    git_sha = paste(rep("a", 40), collapse = ""),
    components = list(validation = data.table::data.table(check = "pca", passed = TRUE)),
    provenance = list(command = "R CMD check"),
    environment = list(r = "4.5.0"), datasets = list(fixture = "1"),
    parameters = list(seed = 1L), created_at = "2026-07-15 UTC"
  )
  expect_s3_class(record, "PopgenVCFReleaseBenchmarkRecord")
  expect_equal(names(record$component_digests), "validation")
  expect_silent(validate_release_benchmark_record(record))

  broken <- record
  broken$components$validation$passed <- FALSE
  expect_error(validate_release_benchmark_record(broken), "digest mismatch")
})

test_that("benchmark archives are append-only and retrievable", {
  make_record <- function(release) new_release_benchmark_record(
    release, "0.10.0", paste(rep(substr(release, 2, 2), 40), collapse = ""),
    components = list(validation = data.table::data.table(label = release, passed = TRUE)),
    created_at = "2026-07-15 UTC"
  )
  first <- make_record("v1")
  second <- make_record("v2")
  archive <- new_benchmark_archive(list(first), metadata = list(project = "popgenVCF"))
  archive <- register_release_benchmark(archive, second)

  expect_equal(names(archive$records), c("v1", "v2"))
  expect_identical(get_release_benchmark(archive, "v2"), second)
  expect_error(register_release_benchmark(archive, second), "already exists")
  expect_error(get_release_benchmark(archive, "missing"), "not found")

  tab <- benchmark_archive_table(archive)
  expect_equal(tab$release, c("v1", "v2"))
  expect_equal(tab$component_count, c(1L, 1L))
})

test_that("archive directory exports round-trip and verify checksums", {
  record <- new_release_benchmark_record(
    "v0.10.0", "0.10.0", paste(rep("b", 40), collapse = ""),
    components = list(
      validation = data.table::data.table(label = c("pca", "ibs"), passed = TRUE),
      diagnostics = list(value = 1)
    ),
    provenance = list(github_run = "fixture"),
    environment = list(platform = "test"),
    created_at = "2026-07-15 UTC"
  )
  archive <- new_benchmark_archive(list(record), metadata = list(schema = "fixture"))
  path <- tempfile("archive-")
  write_benchmark_archive(archive, path)

  expect_true(file.exists(file.path(path, "archive.rds")))
  expect_true(file.exists(file.path(path, "releases.tsv")))
  expect_true(file.exists(file.path(path, "manifest.tsv")))
  expect_true(file.exists(file.path(path, "releases", "v0.10.0", "record.rds")))
  expect_true(file.exists(file.path(path, "releases", "v0.10.0", "summary.tsv")))
  expect_true(file.exists(file.path(path, "releases", "v0.10.0", "metadata.json")))
  expect_true(verify_benchmark_archive(path))
  expect_identical(read_benchmark_archive(path), archive)
  expect_error(write_benchmark_archive(archive, path), "already exists")
})

test_that("archive verification detects corruption", {
  record <- new_release_benchmark_record(
    "v1", "1.0.0", paste(rep("c", 40), collapse = ""),
    components = list(validation = data.table::data.table(passed = TRUE)),
    created_at = "2026-07-15 UTC"
  )
  path <- tempfile("archive-")
  write_benchmark_archive(new_benchmark_archive(list(record)), path)
  cat("corrupt", file = file.path(path, "releases", "v1", "summary.tsv"), append = TRUE)
  expect_error(verify_benchmark_archive(path), "checksum mismatch")
})

test_that("archive constructors reject malformed inputs", {
  expect_error(new_release_benchmark_record("v1", "1", "abc", list()), "must not be empty")
  expect_error(new_release_benchmark_record("", "1", "abc", list(x = 1)), "release")
  expect_error(new_benchmark_archive(metadata = list(1)), "named list")
})
