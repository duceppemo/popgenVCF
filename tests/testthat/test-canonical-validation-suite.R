suite_fixture <- function(id, directory, analyses = c("pca", "fst")) {
  file <- file.path(directory, paste0(id, ".vcf"))
  writeLines("##fileformat=VCFv4.2", file)
  new_canonical_dataset(
    id = id, version = "1", title = paste("Dataset", id),
    license = "CC0-1.0", citation = paste("Citation", id),
    organism = "Test organism", analyses = analyses,
    files = data.frame(filename = basename(file),
      sha256 = digest::digest(file, algo = "sha256", file = TRUE),
      size_bytes = unname(file.info(file)$size), source = NA_character_)
  )
}

suite_registry <- function(specs) {
  registry <- new_canonical_dataset_registry()
  for (descriptor in specs) registry <- register_canonical_dataset(
    registry, descriptor, approval = "approved", reviewed_by = "reviewer",
    reviewed_at = "2026-07-22")
  registry
}

test_that("suite entries and execution order are deterministic", {
  a <- tempfile(); z <- tempfile(); dir.create(a); dir.create(z)
  da <- suite_fixture("a_panel", a); dz <- suite_fixture("z_panel", z)
  suite <- new_canonical_validation_suite("core", "Core suite")
  suite <- register_canonical_validation(suite, "z_panel", z)
  suite <- register_canonical_validation(suite, "a_panel", a)
  expect_equal(names(suite$entries), c("a_panel", "z_panel"))
  result <- run_canonical_validation_suite(suite, suite_registry(list(da, dz)))
  table <- canonical_validation_suite_table(result)
  expect_equal(table$dataset_id, c("a_panel", "z_panel"))
  expect_true(all(table$passed))
})

test_that("suite registration rejects duplicates unless replacement is explicit", {
  suite <- new_canonical_validation_suite("core", "Core suite")
  suite <- register_canonical_validation(suite, "panel", tempfile())
  expect_error(register_canonical_validation(suite, "panel", tempfile()), "already registered")
  replaced <- register_canonical_validation(suite, "panel", tempfile(), replace = TRUE)
  expect_length(replaced$entries, 1L)
})

test_that("fail-fast and continue policies are enforced", {
  good <- tempfile(); bad <- tempfile(); dir.create(good); dir.create(bad)
  dg <- suite_fixture("good", good); db <- suite_fixture("bad", bad)
  registry <- suite_registry(list(dg, db))
  unlink(file.path(bad, db$files$filename))
  suite <- new_canonical_validation_suite("strict", "Strict", fail_fast = TRUE)
  suite <- register_canonical_validation(suite, "bad", bad)
  suite <- register_canonical_validation(suite, "good", good)
  strict <- run_canonical_validation_suite(suite, registry)
  expect_equal(names(strict$results), "bad")
  suite$fail_fast <- FALSE
  continued <- run_canonical_validation_suite(suite, registry)
  expect_equal(names(continued$results), c("bad", "good"))
  expect_equal(canonical_validation_suite_table(continued)$status, c("fail", "pass"))
})

test_that("custom validations and coverage summaries are aggregated", {
  a <- tempfile(); b <- tempfile(); dir.create(a); dir.create(b)
  da <- suite_fixture("a", a, c("pca", "fst")); db <- suite_fixture("b", b, c("pca", "ibs"))
  registry <- suite_registry(list(da, db))
  suite <- new_canonical_validation_suite("coverage", "Coverage", fail_fast = FALSE)
  check <- function(descriptor, directory) data.frame(check = "scientific", passed = TRUE)
  suite <- register_canonical_validation(suite, "b", b, validation = check)
  suite <- register_canonical_validation(suite, "a", a)
  result <- run_canonical_validation_suite(suite, registry)
  expect_equal(canonical_validation_suite_table(result)$checks, c(0L, 1L))
  coverage <- canonical_validation_coverage(suite, registry)
  expect_equal(coverage$analysis, c("fst", "ibs", "pca", "pca"))
})

test_that("suite evidence is deterministic", {
  directory <- tempfile(); dir.create(directory)
  descriptor <- suite_fixture("panel", directory)
  suite <- register_canonical_validation(
    new_canonical_validation_suite("release", "Release suite"), "panel", directory)
  result <- run_canonical_validation_suite(suite, suite_registry(list(descriptor)))
  paths <- write_canonical_validation_suite(result, tempfile())
  expect_true(all(file.exists(paths)))
  evidence <- data.table::fread(paths[["summary"]])
  expect_true(evidence$passed)
})
