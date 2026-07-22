publication_narrative_test_results <- function() {
  list(
    pca = structure(list(variance = c(0.34, 0.19), n_samples = 8L, n_snps = 120L), class = "PopgenVCFPCAResult"),
    ibs = structure(list(method = "SNPRelate IBS"), class = "PopgenVCFIBSResult"),
    tree = structure(list(), class = "PopgenVCFTreeResult"),
    diversity = structure(list(), class = "PopgenVCFDiversityResult"),
    fst = structure(list(method = "Weir and Cockerham 1984"), class = "PopgenVCFFSTResult"),
    amova = structure(list(), class = "PopgenVCFAMOVAResult"),
    dapc = structure(list(parameters = list(n_pca = 12L, n_da = 2L)), class = "PopgenVCFDAPCResult"),
    ibd = structure(list(), class = "PopgenVCFIBDResult"),
    ancestry = structure(list(backend = "ADMIXTURE", selected_k = 3L), class = "PopgenVCFAncestryResult")
  )
}

publication_narrative_test_project <- function() {
  new_popgenvcf_project(
    "Narrative completeness fixture", results = publication_narrative_test_results(),
    project_id = "00000000-0000-0000-0000-000000000930"
  )
}

test_that("all canonical publication families have complete deterministic narratives", {
  project <- publication_narrative_test_project()
  inventory <- publication_narrative_inventory(project)
  completeness <- publication_narrative_completeness(inventory)

  expect_identical(inventory$kind, c("pca", "ibs", "tree", "diversity", "fst", "amova", "dapc", "ibd", "ancestry"))
  expect_true(all(inventory$state == "present"))
  expect_true(all(inventory$method_complete))
  expect_true(all(inventory$caption_complete))
  expect_true(all(inventory$citation_complete))
  expect_true(all(nzchar(inventory$supplementary_summary)))
  expect_equal(completeness$required_families, 9L)
  expect_equal(completeness$complete_families, 9L)
  expect_true(completeness$passed)
})

test_that("absent and non-certifying states remain explicit without scientific claims", {
  project <- new_popgenvcf_project(
    "Fallback fixture",
    modules = list(pca = list(status = "skipped", reason = "No variants passed QC."),
                   ancestry = list(status = "diagnostic-only", reason = "Exploratory backend comparison.")),
    results = list(ancestry = structure(list(backend = "ADMIXTURE", selected_k = 2L,
                                              status = "diagnostic-only"), class = "PopgenVCFAncestryResult")),
    project_id = "00000000-0000-0000-0000-000000000931"
  )
  inventory <- publication_narrative_inventory(project)
  expect_equal(inventory[kind == "pca", state], "skipped")
  expect_match(inventory[kind == "pca", reason], "No variants")
  expect_true(is.na(inventory[kind == "pca", method]))
  expect_equal(inventory[kind == "ancestry", state], "diagnostic-only")
  expect_match(inventory[kind == "ancestry", supplementary_summary], "diagnostic-only")
  expect_true(publication_narrative_completeness(inventory)$passed)
})

test_that("duplicate narrative and caption ownership fail closed", {
  duplicate_results <- publication_narrative_test_results()
  duplicate_results$pca_copy <- duplicate_results$pca
  duplicate <- new_popgenvcf_project(
    "Duplicate narrative fixture", results = duplicate_results,
    project_id = "00000000-0000-0000-0000-000000000932"
  )
  expect_error(publication_narrative_inventory(duplicate), "duplicate narrative ownership")

  inventory <- publication_narrative_inventory(publication_narrative_test_project())
  artifacts <- data.table::data.table(id = "pca_ancestry_plot")
  expect_error(publication_validate_caption_ownership(artifacts, inventory), "conflicting caption ownership")
})

test_that("publication bundles retain completeness and supplementary evidence", {
  project <- publication_narrative_test_project()
  bundle <- new_publication_bundle(project)
  expect_identical(bundle$schema_version, "1.2")
  expect_equal(nrow(bundle$narrative_inventory), 9L)
  expect_true(bundle$narrative_completeness$passed)

  directory <- tempfile("publication-completeness-")
  generate_publication_bundle(project, directory, include_project = FALSE, include_fair = FALSE)
  required <- c(
    "manuscript/narrative-inventory.tsv",
    "manuscript/narrative-completeness.tsv",
    "supplementary/analysis-summaries.tsv",
    "provenance/publication.json"
  )
  expect_true(all(file.exists(file.path(directory, required))))
  evidence <- data.table::fread(file.path(directory, "manuscript", "narrative-completeness.tsv"))
  expect_true(evidence$passed)
  expect_true(validate_publication_bundle(directory))
})
