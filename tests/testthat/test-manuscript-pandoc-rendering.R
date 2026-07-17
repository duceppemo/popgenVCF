test_that("pandoc rendering arguments are deterministic", {
  project <- new_popgenvcf_project("pandoc-test")
  manuscript <- new_manuscript(project, title = "Pandoc test")
  directory <- tempfile()
  write_manuscript(manuscript, directory)
  first <- pandoc_render_arguments(directory, "html")
  second <- pandoc_render_arguments(directory, "html")
  expect_identical(first, second)
  expect_true(any(grepl("--citeproc", first, fixed = TRUE)))
  expect_true(any(grepl("manuscript.html", first, fixed = TRUE)))
})

test_that("dry-run rendering records command without requiring pandoc", {
  project <- new_popgenvcf_project("pandoc-dry")
  manuscript <- new_manuscript(project, title = "Pandoc dry run")
  directory <- tempfile()
  write_manuscript(manuscript, directory)
  record <- render_manuscript(directory, "docx", pandoc = "", dry_run = TRUE)
  expect_s3_class(record, "PopgenVCFManuscriptRender")
  expect_true(record$dry_run)
  expect_true(is.na(record$status))
  expect_true(grepl("manuscript.docx$", record$output))
})

test_that("missing pandoc fails clearly outside dry-run", {
  project <- new_popgenvcf_project("pandoc-missing")
  manuscript <- new_manuscript(project, title = "Pandoc missing")
  directory <- tempfile()
  write_manuscript(manuscript, directory)
  expect_error(render_manuscript(directory, "html", pandoc = ""), "Pandoc is not available")
})

test_that("render validation detects modified output", {
  output <- tempfile(fileext = ".html")
  writeLines("original", output)
  record <- structure(list(
    schema_version = "1.0", format = "html", manuscript_directory = tempdir(), pandoc = list(),
    arguments = character(), output = output, stdout = tempfile(), stderr = tempfile(), status = 0L,
    dry_run = FALSE, output_sha256 = digest::digest(output, algo = "sha256", file = TRUE)
  ), class = "PopgenVCFManuscriptRender")
  expect_true(validate_manuscript_render(record))
  writeLines("modified", output)
  expect_error(validate_manuscript_render(record), "checksum mismatch")
})