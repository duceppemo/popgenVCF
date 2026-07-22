test_that("canonical dataset descriptors fail closed", {
  files <- data.frame(filename = "fixture.txt", sha256 = paste(rep("a", 64), collapse = ""))
  descriptor <- new_canonical_dataset("example", "1", "Example", "CC0-1.0",
                                      "Example citation", files, analyses = c("PCA", "IBS"))
  expect_s3_class(descriptor, "PopgenVCFCanonicalDataset")
  expect_equal(descriptor$analyses, c("ibs", "pca"))
  expect_error(new_canonical_dataset("example", "1", "Example", "", "citation", files), "license")
  bad <- files; bad$sha256 <- "abc"
  expect_error(new_canonical_dataset("example", "1", "Example", "CC0-1.0", "citation", bad), "SHA256")
})

test_that("canonical materialization is explicit and checksum pinned", {
  mirror <- tempfile(); destination <- tempfile()
  dir.create(mirror)
  writeLines("canonical payload", file.path(mirror, "fixture.txt"), useBytes = TRUE)
  sha <- digest::digest(file.path(mirror, "fixture.txt"), algo = "sha256", file = TRUE)
  size <- unname(file.info(file.path(mirror, "fixture.txt"))$size)
  descriptor <- new_canonical_dataset("example", "1", "Example", "CC0-1.0", "Citation",
    data.frame(filename = "fixture.txt", sha256 = sha, size_bytes = size))
  expect_error(materialize_canonical_dataset(descriptor, destination), "downloads are disabled")
  path <- materialize_canonical_dataset(descriptor, destination, source_dir = mirror)
  expect_true(dir.exists(path))
  evidence <- verify_canonical_dataset(descriptor, destination)
  expect_true(all(evidence$passed))
  writeLines("corrupt", file.path(destination, "fixture.txt"), useBytes = TRUE)
  expect_false(verify_canonical_dataset(descriptor, destination)$passed)
})

test_that("external comparisons align identifiers and apply tolerances", {
  reference <- data.frame(sample = c("b", "a"), value = c(2, 1))
  observed <- data.frame(sample = c("a", "b"), value = c(1 + 1e-9, 2.1))
  result <- compare_external_results(observed, reference, "sample", "value",
                                     tolerance = 1e-6, tool = "plink2", tool_version = "2.0")
  expect_equal(result$sample, c("a", "b"))
  expect_equal(result$status, c("pass", "fail"))
  expect_true(all(result$tool == "plink2"))
  expect_error(compare_external_results(rbind(observed, observed[1, ]), reference,
                                        "sample", "value", tool = "x", tool_version = "1"),
               "unique")
})

test_that("validation evidence is deterministic and complete", {
  directory <- tempfile(); output <- tempfile(); dir.create(directory)
  writeLines("payload", file.path(directory, "fixture.txt"), useBytes = TRUE)
  sha <- digest::digest(file.path(directory, "fixture.txt"), algo = "sha256", file = TRUE)
  descriptor <- new_canonical_dataset("example", "1", "Example", "CC0-1.0", "Citation",
    data.frame(filename = "fixture.txt", sha256 = sha))
  comparisons <- compare_external_results(data.frame(id = 1, x = 1), data.frame(id = 1, x = 1),
                                          "id", "x", tool = "reference", tool_version = "1")
  paths <- write_canonical_validation_evidence(descriptor, directory, output, comparisons)
  expect_true(all(file.exists(unlist(paths))))
  expect_match(readLines(paths$methods), "SHA-256")
})
