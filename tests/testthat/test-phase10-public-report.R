make_phase10_report_result <- function() {
  new_diversity_result(data.frame(
    population = c("north", "south"),
    observed_heterozygosity = c(0.21, 0.24),
    stringsAsFactors = FALSE
  ))
}

test_that("public report rendering delegates to the existing report engine", {
  request <- new_public_analysis_request("report.render", "analysis-report-1")
  output_dir <- tempfile("phase10-report-")

  response <- render_public_report(
    request = request,
    report = list(diversity = make_phase10_report_result()),
    output_dir = output_dir,
    title = "Canonical diversity report",
    formats = "html",
    render = FALSE
  )

  expect_true(validate_public_analysis_response(response, request))
  expect_identical(response$status, "completed")
  expect_identical(response$scientific_values$title, "Canonical diversity report")
  expect_identical(response$scientific_values$section_ids, c(diversity = "diversity"))
  expect_false(response$scientific_values$rendered)
  expect_identical(
    names(response$artifact_ids),
    c("report::plan", "report::sections", "report::source")
  )
  expect_true(file.exists(file.path(output_dir, "population_genomics_report.qmd")))
})

test_that("equivalent report plans produce equivalent public responses", {
  request <- new_public_analysis_request("report.render", "analysis-report-2")
  first <- build_population_genomics_report_plan(
    list(diversity = make_phase10_report_result()),
    title = "Stable report"
  )
  second <- build_population_genomics_report_plan(
    list(diversity = make_phase10_report_result()),
    title = "Stable report"
  )
  first$created_at <- "2026-01-01T00:00:00Z"
  second$created_at <- "2026-07-20T23:59:59Z"
  first$reproducibility$platform <- "internal-platform-one"
  second$reproducibility$platform <- "internal-platform-two"

  response_one <- render_public_report(
    request, first, tempfile("phase10-report-one-"), render = FALSE
  )
  response_two <- render_public_report(
    request, second, tempfile("phase10-report-two-"), render = FALSE
  )

  expect_identical(response_one$fingerprint, response_two$fingerprint)
  expect_identical(
    response_one$scientific_values$report_id,
    response_two$scientific_values$report_id
  )
})

test_that("public report responses hide paths and renderer internals", {
  request <- new_public_analysis_request("report.render", "analysis-report-3")
  private_dir <- file.path(tempdir(), "private", "renderer", "workspace")
  response <- render_public_report(
    request,
    list(diversity = make_phase10_report_result()),
    private_dir,
    render = FALSE
  )

  serialized <- paste(capture.output(str(response)), collapse = "\n")
  expect_false(grepl(private_dir, serialized, fixed = TRUE))
  expect_false(grepl("created_at", serialized, fixed = TRUE))
  expect_false(grepl("platform", serialized, fixed = TRUE))
  expect_false(grepl("quarto", serialized, ignore.case = TRUE))
})

test_that("public report adapter fails closed", {
  wrong_request <- new_public_analysis_request("artifact.list", "analysis-report-4")
  response <- render_public_report(
    wrong_request,
    list(diversity = make_phase10_report_result()),
    tempfile("phase10-report-")
  )
  expect_identical(response$status, "rejected")
  expect_identical(response$error$code, "unsupported_operation")

  request <- new_public_analysis_request("report.render", "analysis-report-4")
  response <- render_public_report(
    request,
    list(diversity = make_phase10_report_result()),
    tempfile("phase10-report-"),
    formats = "docx"
  )
  expect_identical(response$status, "rejected")
  expect_identical(response$error$code, "unsupported_report_format")

  response <- render_public_report(
    request,
    list(not_a_result = list()),
    tempfile("phase10-report-")
  )
  expect_identical(response$status, "rejected")
  expect_identical(response$error$code, "invalid_report_input")
})

test_that("renderer failures use a stable public error", {
  request <- new_public_analysis_request("report.render", "analysis-report-5")
  response <- render_public_report(
    request,
    list(diversity = make_phase10_report_result()),
    tempfile("phase10-report-"),
    render = TRUE,
    formats = "not-supported"
  )

  expect_identical(response$status, "rejected")
  expect_identical(response$error$code, "unsupported_report_format")
})
