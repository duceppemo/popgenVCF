registry_test_descriptor <- function(id = "candidate_panel") {
  payload <- tempfile(fileext = ".vcf")
  writeLines("##fileformat=VCFv4.2", payload)
  new_canonical_dataset(
    id = id,
    version = "1.0",
    title = "Candidate canonical panel",
    license = "CC-BY-4.0",
    citation = "Example authors (2026). Candidate panel.",
    organism = "Test organism",
    analyses = c("pca", "fst"),
    files = data.frame(
      filename = "panel.vcf",
      sha256 = digest::digest(payload, algo = "sha256", file = TRUE),
      size_bytes = unname(file.info(payload)$size),
      source = NA_character_,
      stringsAsFactors = FALSE
    )
  )
}

test_that("canonical registries register and list deterministic entries", {
  registry <- new_canonical_dataset_registry()
  registry <- register_canonical_dataset(registry, registry_test_descriptor("z_panel"))
  registry <- register_canonical_dataset(registry, registry_test_descriptor("a_panel"))

  table <- list_canonical_datasets(registry)
  expect_equal(table$id, c("a_panel", "z_panel"))
  expect_equal(table$approval, rep("candidate", 2))
  expect_equal(table$files, rep(1L, 2))
  expect_equal(get_canonical_dataset(registry, "A_PANEL")$id, "a_panel")
})

test_that("duplicate identifiers fail closed unless replacement is explicit", {
  descriptor <- registry_test_descriptor()
  registry <- register_canonical_dataset(new_canonical_dataset_registry(), descriptor)
  expect_error(register_canonical_dataset(registry, descriptor), "already registered")
  replaced <- register_canonical_dataset(registry, descriptor, notes = "reviewed", replace = TRUE)
  expect_equal(length(replaced$entries), 1L)
})

test_that("approval requires review provenance", {
  descriptor <- registry_test_descriptor()
  registry <- new_canonical_dataset_registry()
  expect_error(
    register_canonical_dataset(registry, descriptor, approval = "approved"),
    "reviewed_by"
  )
  approved <- register_canonical_dataset(
    registry, descriptor, approval = "approved",
    reviewed_by = "scientific-review-board", reviewed_at = "2026-07-22"
  )
  expect_s3_class(get_canonical_dataset(approved, descriptor$id, TRUE),
                  "PopgenVCFCanonicalDataset")
})

test_that("candidate datasets cannot be materialized through the registry", {
  registry <- register_canonical_dataset(
    new_canonical_dataset_registry(), registry_test_descriptor()
  )
  expect_error(
    materialize_registered_canonical_dataset(
      registry, "candidate_panel", destination = tempfile()
    ),
    "not approved"
  )
})

test_that("registry evidence is deterministic", {
  registry <- register_canonical_dataset(
    new_canonical_dataset_registry(), registry_test_descriptor(),
    approval = "approved", reviewed_by = "reviewer", reviewed_at = "2026-07-22"
  )
  path <- write_canonical_dataset_registry(registry, tempfile(fileext = ".tsv"))
  table <- data.table::fread(path)
  expect_equal(table$id, "candidate_panel")
  expect_equal(table$approval, "approved")
  expect_equal(table$reviewed_by, "reviewer")
})
