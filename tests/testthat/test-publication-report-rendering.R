publication_report_test_manuscript <- function() {
  x <- list(
    schema_version = "1.0",
    project_id = "project-001",
    project_digest = "project-digest",
    publication_digest = "publication-digest",
    title = "Deterministic report",
    authors = data.table::data.table(
      name = "A. Author", affiliation = NA_character_, email = NA_character_,
      orcid = NA_character_, corresponding = TRUE
    ),
    abstract = "Abstract.", keywords = "population genetics",
    introduction = "Introduction.", methods = "Generated methods.",
    results = "Author interpretation.", discussion = "Discussion.",
    captions = data.table::data.table(id = character(), caption = character()),
    artifacts = data.table::data.table(), software = data.table::data.table(),
    parameters = data.table::data.table(),
    declarations = list(
      data_availability = "Data statement.", software_availability = "Software statement.",
      reproducibility = "Reproducibility statement.", funding = "Funding statement.",
      author_contributions = "Contribution statement.", competing_interests = "None."
    ),
    bibliography = NULL
  )
  class(x) <- c("PopgenVCFManuscript", "list")
  validate_manuscript(x)
  x
}

test_that("publication report specifications are canonical and deterministic", {
  a <- new_publication_report_spec(c("pdf", "html", "html"))
  b <- new_publication_report_spec(c("html", "pdf"))
  expect_s3_class(a, "PopgenVCFPublicationReportSpec")
  expect_identical(a$formats, c("html", "pdf"))
  expect_identical(a$fingerprint, b$fingerprint)
  expect_true(validate_publication_report_spec(a))
  expect_error(new_publication_report_spec("txt"), "formats")
})

test_that("publication report plans bind manuscript, source, and renderer identity", {
  manuscript <- publication_report_test_manuscript()
  spec <- new_publication_report_spec(c("docx", "html", "pdf"))
  a <- new_publication_report_plan(manuscript, spec, "quarto", "1.8.0")
  b <- new_publication_report_plan(manuscript, spec, "quarto", "1.8.0")
  expect_s3_class(a, "PopgenVCFPublicationReportPlan")
  expect_identical(a$fingerprint, b$fingerprint)
  expect_identical(a$outputs$format, c("docx", "html", "pdf"))
  expect_true(validate_publication_report_plan(a, manuscript, spec))
  expect_match(publication_report_plan_report(a)[[1L]], "Publication report rendering plan", fixed = TRUE)
})

test_that("publication report plans fail closed on manuscript and plan mutation", {
  manuscript <- publication_report_test_manuscript()
  spec <- new_publication_report_spec()
  plan <- new_publication_report_plan(manuscript, spec)

  changed <- manuscript
  changed$title <- "Changed title"
  expect_error(
    validate_publication_report_plan(plan, changed, spec),
    "not bound to the supplied manuscript"
  )

  plan$outputs$path[[1L]] <- "other.html"
  expect_error(
    validate_publication_report_plan(plan, manuscript, spec),
    "fingerprint mismatch|not bound"
  )
})

test_that("rendered output manifests verify checksums and detect mutation", {
  manuscript <- publication_report_test_manuscript()
  spec <- new_publication_report_spec(c("html", "pdf"))
  plan <- new_publication_report_plan(manuscript, spec)
  dir <- withr::local_tempdir()
  writeLines("html output", file.path(dir, "manuscript.html"))
  writeLines("pdf output", file.path(dir, "manuscript.pdf"))

  manifest <- new_publication_report_output_manifest(plan, dir, c("warning-b", "warning-a", "warning-a"))
  expect_s3_class(manifest, "PopgenVCFPublicationReportOutputManifest")
  expect_identical(manifest$warnings, c("warning-a", "warning-b"))
  expect_true(validate_publication_report_output_manifest(manifest, plan, dir))

  writeLines("mutated", file.path(dir, "manuscript.html"))
  expect_error(
    validate_publication_report_output_manifest(manifest, plan, dir),
    "checksum mismatch"
  )
})

test_that("output manifests fail closed on missing rendered formats", {
  manuscript <- publication_report_test_manuscript()
  spec <- new_publication_report_spec(c("docx", "html"))
  plan <- new_publication_report_plan(manuscript, spec)
  dir <- withr::local_tempdir()
  writeLines("html output", file.path(dir, "manuscript.html"))
  expect_error(
    new_publication_report_output_manifest(plan, dir),
    "Missing rendered publication output"
  )
})
