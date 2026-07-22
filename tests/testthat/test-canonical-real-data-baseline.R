real_data_snapshot_fixture <- function(approval = "proposed") {
  directory <- tempfile(); dir.create(directory)
  file <- file.path(directory, "fixture.vcf")
  writeLines("##fileformat=VCFv4.2", file)
  dataset <- new_canonical_dataset(
    id = "dataset_a", version = "1", title = "Fixture", license = "CC0-1.0",
    citation = "Fixture citation", organism = "Test organism", analyses = "pca",
    files = data.frame(filename = basename(file),
      sha256 = digest::digest(file, algo = "sha256", file = TRUE),
      size_bytes = unname(file.info(file)$size), source = NA_character_)
  )
  metric <- new_canonical_baseline_metric(
    id = "sample_count", dataset_id = "dataset_a", analysis = "pca",
    expected = 2L, comparator = "exact", tolerance = 0,
    version = "2026.1", rationale = "Deterministic fixture sample count")
  args <- list(
    dataset = dataset,
    registry = new_canonical_baseline_registry(list(metric)),
    sample_metadata = data.frame(
      sample_id = c("sample_b", "sample_a"),
      population = c("POP2", "POP1"),
      superpopulation = c("SUPER", "SUPER"),
      sex = c("male", "male")),
    dataset_version = "1",
    generated_by = "scheduled-full-validation",
    generated_at = "2026-07-22T12:00:00Z",
    source_commit = paste(rep("a", 40), collapse = ""),
    approval = approval)
  if (approval == "approved") {
    args$approved_by <- "scientific reviewer"
    args$approved_at <- "2026-07-22"
  }
  do.call(new_canonical_real_data_baseline_snapshot, args)
}

test_that("real-data baseline snapshots are deterministic and complete", {
  snapshot <- real_data_snapshot_fixture()
  expect_equal(snapshot$sample_metadata$sample_id, c("sample_a", "sample_b"))
  expect_equal(snapshot$sample_count, 2L)
  expect_equal(snapshot$approval, "proposed")
  expect_invisible(validate_canonical_real_data_baseline_snapshot(snapshot))
})

test_that("production use fails closed for unapproved snapshots", {
  expect_error(
    validate_canonical_real_data_baseline_snapshot(
      real_data_snapshot_fixture(), require_approved = TRUE),
    "not approved")
  expect_invisible(validate_canonical_real_data_baseline_snapshot(
    real_data_snapshot_fixture("approved"), require_approved = TRUE))
})

test_that("real-data snapshots reject incomplete sample metadata", {
  snapshot <- real_data_snapshot_fixture()
  metadata <- snapshot$sample_metadata
  metadata$sex[1] <- NA_character_
  expect_error(new_canonical_real_data_baseline_snapshot(
    dataset = structure(list(), class = "invalid"),
    registry = snapshot$baseline_registry,
    sample_metadata = metadata,
    dataset_version = "1", generated_by = "test",
    generated_at = "2026-07-22T12:00:00Z",
    source_commit = paste(rep("a", 40), collapse = "")),
    "canonical dataset|PopgenVCFCanonicalDataset")
})

test_that("snapshot JSON is deterministic and approval-aware", {
  proposed <- real_data_snapshot_fixture()
  path <- tempfile(fileext = ".json")
  expect_equal(write_canonical_real_data_baseline_snapshot(proposed, path), normalizePath(path))
  payload <- jsonlite::read_json(path, simplifyVector = TRUE)
  expect_equal(payload$dataset_id, "dataset_a")
  expect_equal(payload$sample_count, 2L)
  expect_error(write_canonical_real_data_baseline_snapshot(
    proposed, tempfile(fileext = ".json"), require_approved = TRUE), "not approved")
})
