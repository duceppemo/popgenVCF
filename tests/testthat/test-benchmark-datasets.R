test_that("default benchmark catalogue is searchable", {
  catalogue <- default_benchmark_dataset_catalogue()
  tab <- list_benchmark_datasets(catalogue)

  expect_s3_class(catalogue, "PopgenVCFBenchmarkDatasetCatalogue")
  expect_true(all(c("synthetic_tiny", "1000g_subset", "hgdp_subset", "hapmap_subset") %in% tab$id))
  expect_equal(list_benchmark_datasets(catalogue, scale = "tiny")$id, "synthetic_tiny")
  expect_equal(list_benchmark_datasets(catalogue, organism = "synthetic")$id, "synthetic_tiny")
  expect_true("synthetic_tiny" %in% list_benchmark_datasets(catalogue, analysis = "pca")$id)
  expect_true(all(list_benchmark_datasets(catalogue, source_type = "remote")$source_type == "remote"))
})

test_that("embedded datasets materialize and are reused offline", {
  entry <- default_benchmark_dataset_catalogue()$entries[["synthetic_tiny@1"]]
  cache <- tempfile("popgenvcf-cache-")

  path <- resolve_benchmark_dataset(entry, cache_dir = cache)
  expect_true(file.exists(path))
  expect_true(verify_benchmark_dataset(path, entry$checksum))
  expect_identical(resolve_benchmark_dataset(entry, cache_dir = cache, offline = TRUE), path)

  dataset <- benchmark_dataset_from_entry(entry, cache_dir = cache, offline = TRUE)
  loaded <- dataset$loader()
  expect_equal(dim(loaded$genotype), c(3L, 3L))
  expect_equal(loaded$population, c("A", "A", "B"))
})

test_that("local and file URL sources are cached atomically", {
  source <- tempfile(fileext = ".rds")
  saveRDS(list(value = 42), source, version = 3)
  checksum <- digest::digest(source, algo = "sha256", file = TRUE)
  cache <- tempfile("popgenvcf-cache-")

  local <- new_benchmark_dataset_entry(
    "local_fixture", source_type = "local", filename = "fixture.rds",
    checksum = checksum, source = source
  )
  local_path <- resolve_benchmark_dataset(local, cache_dir = cache)
  expect_equal(readRDS(local_path)$value, 42)

  remote <- new_benchmark_dataset_entry(
    "remote_fixture", source_type = "remote", filename = "remote.rds",
    checksum = checksum, source = paste0("file://", normalizePath(source))
  )
  remote_path <- resolve_benchmark_dataset(remote, cache_dir = cache)
  expect_equal(readRDS(remote_path)$value, 42)
  expect_identical(resolve_benchmark_dataset(remote, cache_dir = cache, offline = TRUE), remote_path)
})

test_that("checksum failures and offline cache misses are actionable", {
  source <- tempfile(fileext = ".rds")
  saveRDS(1:3, source, version = 3)
  checksum <- digest::digest(source, algo = "sha256", file = TRUE)
  cache <- tempfile("popgenvcf-cache-")
  remote <- new_benchmark_dataset_entry(
    "checked_remote", source_type = "remote", filename = "checked.rds",
    checksum = checksum, source = paste0("file://", normalizePath(source))
  )

  path <- resolve_benchmark_dataset(remote, cache_dir = cache)
  writeLines("corrupt", path)
  expect_false(verify_benchmark_dataset(path, checksum))
  expect_error(resolve_benchmark_dataset(remote, cache_dir = cache, offline = TRUE), "offline mode")

  bad <- remote
  bad$id <- "bad_checksum"
  bad$checksum <- paste(rep("0", 64), collapse = "")
  expect_error(resolve_benchmark_dataset(bad, cache_dir = tempfile("cache-")), "SHA256")
})

test_that("planned remote datasets fail transparently", {
  entry <- default_benchmark_dataset_catalogue()$entries[["1000g_subset@1"]]
  expect_false(entry$published)
  expect_error(resolve_benchmark_dataset(entry, cache_dir = tempfile("cache-")), "not yet published")
})

test_that("catalogue contracts reject malformed entries and duplicates", {
  expect_error(new_benchmark_dataset_entry("x", filename = "x.rds", source = "not a function"), "materializer")
  expect_error(new_benchmark_dataset_entry("x", source_type = "local", filename = "x", source = "x", checksum = "bad"), "SHA256")

  entry <- embedded_tiny_benchmark_entry()
  catalogue <- new_benchmark_dataset_catalogue(list(entry))
  expect_error(register_benchmark_dataset(catalogue, entry), "duplicate")
})
