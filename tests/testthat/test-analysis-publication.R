test_that("analysis narratives distinguish scientific interpretations", {
  project <- new_popgenvcf_project("Narratives", project_id = "00000000-0000-0000-0000-000000000068")
  project$results <- list(
    pca = structure(list(variance = c(0.31, 0.18), parameters = list(method = "SNPRelate")), class = "PopgenVCFPCAResult"),
    ibs = structure(list(method = "SNPRelate IBS"), class = "PopgenVCFIBSResult"),
    fst = structure(list(method = "Weir and Cockerham 1984"), class = "PopgenVCFFSTResult"),
    dapc = structure(list(parameters = list(n_pca = 20L, n_da = 2L)), class = "PopgenVCFDAPCResult")
  )
  narratives <- publication_analysis_narratives(project)
  expect_equal(nrow(narratives), 4L)
  expect_setequal(narratives$kind, c("pca", "ibs", "fst", "dapc"))
  expect_match(narratives[kind == "pca", legend], "31.0%")
  expect_match(narratives[kind == "ibs", method], "not interpreted as FST")
  expect_match(narratives[kind == "fst", method], "population level")
  expect_match(narratives[kind == "dapc", method], "not interpreted as model-based ancestry")
})

test_that("ancestry narratives preserve model-based interpretation", {
  project <- new_popgenvcf_project("Ancestry", project_id = "00000000-0000-0000-0000-000000000069")
  project$results <- list(ancestry = structure(list(backend = "ADMIXTURE", selected_k = 3L), class = "PopgenVCFAncestryResult"))
  narrative <- publication_analysis_narratives(project)
  expect_match(narrative$method, "ADMIXTURE")
  expect_match(narrative$method, "K = 3")
  expect_match(narrative$legend, "cluster labels are arbitrary")
  expect_match(narrative$citation_keys, "Alexander2009ADMIXTURE")
})

test_that("metadata-poor projects return an empty narrative table", {
  project <- new_popgenvcf_project("Empty narratives", project_id = "00000000-0000-0000-0000-000000000070")
  narratives <- publication_analysis_narratives(project)
  expect_s3_class(narratives, "data.table")
  expect_equal(nrow(narratives), 0L)
  bundle <- new_publication_bundle(project)
  expect_equal(nrow(bundle$analyses), 0L)
})

test_that("publication bundles write narratives and bibliography", {
  project <- new_popgenvcf_project("Publication narratives", project_id = "00000000-0000-0000-0000-000000000071")
  project$results <- list(
    pca = structure(list(variance = c(0.4, 0.2)), class = "PopgenVCFPCAResult"),
    fst = structure(list(), class = "PopgenVCFFSTResult")
  )
  directory <- tempfile("publication-narratives-")
  generate_publication_bundle(project, directory, include_project = FALSE, include_fair = FALSE)
  expect_true(file.exists(file.path(directory, "manuscript", "analysis-narratives.tsv")))
  expect_true(file.exists(file.path(directory, "manuscript", "references.bib")))
  methods <- paste(readLines(file.path(directory, "manuscript", "methods.md")), collapse = "\n")
  references <- paste(readLines(file.path(directory, "manuscript", "references.bib")), collapse = "\n")
  expect_match(methods, "## Pca")
  expect_match(methods, "## Fst")
  expect_match(references, "Zheng2012SNPRelate")
  expect_match(references, "Weir1984FST")
  expect_true(validate_publication_bundle(directory))
})

test_that("bibliography writer tolerates no analyses", {
  path <- tempfile(fileext = ".bib")
  empty <- data.table::data.table(citation_keys = character())
  expect_silent(write_publication_bibliography(empty, path))
  expect_true(file.exists(path))
  expect_equal(length(readLines(path)), 0L)
})
