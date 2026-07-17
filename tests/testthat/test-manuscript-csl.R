test_that("citation profiles validate built-in and custom styles", {
  generic <- new_citation_profile("generic-author-year")
  expect_s3_class(generic, "PopgenVCFCitationProfile")
  expect_null(generic$csl_path)
  expect_silent(validate_citation_profile(generic))

  csl <- tempfile(fileext = ".csl")
  writeLines(c("<?xml version=\"1.0\" encoding=\"utf-8\"?>", "<style xmlns=\"http://purl.org/net/xbiblio/csl\" version=\"1.0\"></style>"), csl)
  custom <- new_citation_profile("custom", csl)
  expect_true(nzchar(custom$csl_sha256))
  expect_identical(custom$bundle_path, "citation-style.csl")
  expect_error(new_citation_profile("custom", tempfile(fileext = ".txt")), "does not exist")
})

test_that("canonical BibTeX keys are extracted deterministically", {
  project <- new_popgenvcf_project("Citation keys", project_id = "00000000-0000-0000-0000-000000000081")
  manuscript <- new_manuscript(project)
  manuscript$bibliography <- c(
    "@article{zeta2026, title={Zeta}}",
    "@software{alpha2025, title={Alpha}}",
    "@article{zeta2026, title={Duplicate identity}}"
  )
  expect_identical(manuscript_citation_keys(manuscript), c("alpha2025", "zeta2026"))
})

test_that("citation profiles produce Pandoc-ready portable manuscript sources", {
  project <- new_popgenvcf_project("CSL manuscript", project_id = "00000000-0000-0000-0000-000000000082")
  manuscript <- new_manuscript(project, title = "CSL-ready manuscript")
  manuscript$bibliography <- c(
    "@article{popgen2026,",
    "  title={Reproducible population genomics},",
    "  author={Doe, Jane},",
    "  year={2026}",
    "}"
  )
  csl <- tempfile(fileext = ".csl")
  writeLines(c("<?xml version=\"1.0\" encoding=\"utf-8\"?>", "<style xmlns=\"http://purl.org/net/xbiblio/csl\" version=\"1.0\"></style>"), csl)
  manuscript <- set_manuscript_citation_profile(manuscript, new_citation_profile("test-style", csl))

  markdown <- render_manuscript_markdown(manuscript)
  expect_identical(markdown[[1L]], "---")
  expect_true(any(markdown == "bibliography: references.bib"))
  expect_true(any(markdown == "csl: citation-style.csl"))

  directory <- tempfile("manuscript-csl-")
  write_manuscript(manuscript, directory)
  expect_true(file.exists(file.path(directory, "references.bib")))
  expect_true(file.exists(file.path(directory, "citation-style.csl")))
  expect_true(file.exists(file.path(directory, "citation-profile.json")))
  expect_true(file.exists(file.path(directory, "citation-manifest.tsv")))
  manifest <- data.table::fread(file.path(directory, "citation-manifest.tsv"))
  expect_identical(manifest$citation_key, "popgen2026")
  expect_identical(manifest$style_id, "test-style")
  expect_true(validate_manuscript(directory))

  writeLines("tampered", file.path(directory, "citation-style.csl"))
  expect_error(validate_manuscript(directory), "checksum mismatch")
})
