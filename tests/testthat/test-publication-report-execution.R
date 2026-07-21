publication_report_mock_renderer <- function(fail_format = NULL, omit_format = NULL) {
  new_publication_report_renderer(
    "mock", "1.0.0", c("docx", "html", "pdf"),
    function(source_path, output_path, format, parameters) {
      if (!is.null(fail_format) && identical(format, fail_format)) {
        return(list(status = 2L, stdout = character(),
                    stderr = paste("failed", format), warnings = "mock warning"))
      }
      if (is.null(omit_format) || !identical(format, omit_format)) {
        writeLines(c(paste("format", format), readLines(source_path, warn = FALSE)), output_path)
      }
      list(status = 0L, stdout = paste("rendered", format),
           stderr = character(), warnings = if (format == "html") "html warning" else character())
    }
  )
}

test_that("renderer adapters validate supported formats and identity", {
  renderer <- publication_report_mock_renderer()
  expect_s3_class(renderer, "PopgenVCFPublicationReportRenderer")
  expect_true(validate_publication_report_renderer(renderer))
  expect_error(
    new_publication_report_renderer("bad", "1.0", "txt", identity),
    "formats"
  )
})

test_that("publication report execution is deterministic and verifiable", {
  manuscript <- publication_report_test_manuscript()
  spec <- new_publication_report_spec(c("html", "pdf"))
  renderer <- publication_report_mock_renderer()
  plan <- new_publication_report_plan(manuscript, spec, renderer$id, renderer$version)
  dir_a <- withr::local_tempdir()
  dir_b <- withr::local_tempdir()

  a <- execute_publication_report_plan(
    plan, manuscript, spec, renderer, dir_a,
    parameters = list(z = 2L, a = "stable")
  )
  b <- execute_publication_report_plan(
    plan, manuscript, spec, renderer, dir_b,
    parameters = list(a = "stable", z = 2L)
  )

  expect_s3_class(a, "PopgenVCFPublicationReportExecution")
  expect_true(a$succeeded)
  expect_identical(names(a$parameters), c("a", "z"))
  expect_identical(a$fingerprint, b$fingerprint)
  expect_identical(a$output_manifest$fingerprint, b$output_manifest$fingerprint)
  expect_true(validate_publication_report_execution(a, plan, dir_a))
  expect_match(publication_report_execution_report(a)[[1L]],
               "Publication report rendering execution", fixed = TRUE)
})

test_that("publication report execution propagates renderer failure and halts", {
  manuscript <- publication_report_test_manuscript()
  spec <- new_publication_report_spec(c("docx", "html", "pdf"))
  renderer <- publication_report_mock_renderer(fail_format = "html")
  plan <- new_publication_report_plan(manuscript, spec, renderer$id, renderer$version)
  dir <- withr::local_tempdir()

  execution <- execute_publication_report_plan(plan, manuscript, spec, renderer, dir)

  expect_false(execution$succeeded)
  expect_null(execution$output_manifest)
  expect_match(execution$failure, "failed html", fixed = TRUE)
  expect_identical(execution$attempts$status, c(0L, 2L, 125L))
  expect_true(validate_publication_report_execution(execution, plan, dir))
})

test_that("renderer success without expected output fails closed", {
  manuscript <- publication_report_test_manuscript()
  spec <- new_publication_report_spec("html")
  renderer <- publication_report_mock_renderer(omit_format = "html")
  plan <- new_publication_report_plan(manuscript, spec, renderer$id, renderer$version)
  dir <- withr::local_tempdir()

  execution <- execute_publication_report_plan(plan, manuscript, spec, renderer, dir)
  expect_false(execution$succeeded)
  expect_match(execution$failure, "did not create", fixed = TRUE)
})

test_that("execution records detect source, output, and record mutation", {
  manuscript <- publication_report_test_manuscript()
  spec <- new_publication_report_spec("html")
  renderer <- publication_report_mock_renderer()
  plan <- new_publication_report_plan(manuscript, spec, renderer$id, renderer$version)
  dir <- withr::local_tempdir()
  execution <- execute_publication_report_plan(plan, manuscript, spec, renderer, dir)

  writeLines("changed source", file.path(dir, "manuscript.md"))
  expect_error(
    validate_publication_report_execution(execution, plan, dir),
    "source checksum mismatch"
  )

  execution$renderer$version <- "changed"
  expect_error(
    validate_publication_report_execution(execution, plan, dir),
    "not bound|fingerprint mismatch"
  )
})
