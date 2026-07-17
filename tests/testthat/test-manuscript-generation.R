test_that("manuscript specifications preserve generated and author text boundaries", {
  project <- new_popgenvcf_project(
    "Manuscript project", project_id = "00000000-0000-0000-0000-000000000081")
  manuscript <- new_manuscript(
    project,
    authors = data.frame(name = "Jane Doe", affiliation = "Population Genomics Lab"),
    abstract = "Author abstract.",
    introduction = "Author introduction.",
    results = "Author interpretation.",
    discussion = "Author discussion.",
    keywords = c("population genomics", "VCF")
  )

  expect_s3_class(manuscript, "PopgenVCFManuscript")
  expect_identical(manuscript$project_id, project$project_id)
  expect_match(manuscript$methods, "popgenVCF")
  expect_identical(manuscript$results, "Author interpretation.")
  expect_silent(validate_manuscript(manuscript))
})

test_that("metadata-poor manuscripts contain explicit author placeholders", {
  project <- new_popgenvcf_project(
    "Minimal manuscript", project_id = "00000000-0000-0000-0000-000000000082")
  manuscript <- new_manuscript(project)
  source <- render_manuscript_markdown(manuscript)

  expect_match(paste(source, collapse = "\n"), "Author-supplied abstract required")
  expect_match(paste(source, collapse = "\n"), "Author-supplied scientific interpretation required")
  expect_match(paste(source, collapse = "\n"), project$project_id, fixed = TRUE)
})

test_that("manuscript source is deterministic", {
  project <- new_popgenvcf_project(
    "Deterministic manuscript", project_id = "00000000-0000-0000-0000-000000000083")
  publication <- new_publication_bundle(project)
  first <- new_manuscript(project, publication = publication, keywords = c("zeta", "alpha"))
  second <- new_manuscript(project, publication = publication, keywords = c("alpha", "zeta"))

  expect_identical(render_manuscript_markdown(first), render_manuscript_markdown(second))
  expect_identical(first$keywords, c("alpha", "zeta"))
})

test_that("written manuscript directories are checksummed", {
  project <- new_popgenvcf_project(
    "Portable manuscript", project_id = "00000000-0000-0000-0000-000000000084")
  manuscript <- new_manuscript(project, authors = data.frame(name = "Jane Doe"))
  directory <- tempfile("manuscript-")

  result <- write_manuscript(manuscript, directory)
  expect_true(dir.exists(result))
  expect_true(file.exists(file.path(directory, "manuscript.md")))
  expect_true(file.exists(file.path(directory, "manuscript.rds")))
  expect_true(validate_manuscript(directory))

  writeLines("tampered", file.path(directory, "manuscript.md"))
  expect_error(validate_manuscript(directory), "checksum mismatch")
})

test_that("author metadata is validated", {
  project <- new_popgenvcf_project(
    "Invalid authors", project_id = "00000000-0000-0000-0000-000000000085")
  expect_error(new_manuscript(project, authors = data.frame(affiliation = "Lab")), "name column")
  expect_error(new_manuscript(project, authors = data.frame(name = "")), "non-empty")
})
