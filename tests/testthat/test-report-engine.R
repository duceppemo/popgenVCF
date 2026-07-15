test_that("report plans discover and order canonical analyses", {
  pca <- new_pca_result(
    data.frame(sample_id = c("a", "b"), PC1 = c(-1, 1), PC2 = c(0, 0)),
    c(2, 1)
  )
  diversity <- new_diversity_result(data.frame(population = "A", Ho = .2))
  fst <- new_fst_result(.1, data.frame(population_1 = "A", population_2 = "B", fst = .1))
  plan <- build_population_genomics_report_plan(list(fst = fst, pca = pca, diversity = diversity))
  expect_s3_class(plan, "PopgenVCFReportPlan")
  expect_equal(plan$sections$analysis, c("pca", "diversity", "fst"))
  expect_true(all(plan$sections$validation_passed))
})

test_that("report filters and invalid inputs behave transparently", {
  pca <- new_pca_result(data.frame(sample_id = "a", PC1 = 0, PC2 = 0), c(1, .5))
  diversity <- new_diversity_result(data.frame(population = "A", Ho = .2))
  plan <- build_population_genomics_report_plan(list(pca, diversity), include = "pca")
  expect_equal(plan$sections$analysis, "pca")
  expect_error(build_population_genomics_report_plan(list(pca), exclude = "pca"), "no report sections")
  expect_error(build_population_genomics_report_plan(list(list(x = 1))), "canonical result")
})

test_that("report source generation does not require Quarto", {
  pca <- new_pca_result(data.frame(sample_id = c("a", "b"), PC1 = c(-1, 1), PC2 = 0), c(1, .5))
  out <- tempfile("popgen-report-")
  result <- write_population_genomics_report(list(pca = pca), out, render = FALSE)
  expect_s3_class(result$plan, "PopgenVCFReportPlan")
  expect_s3_class(result$artifacts, "PopgenVCFArtifactManifest")
  expect_true(all(file.exists(result$paths)))
  qmd <- readLines(result$paths[["source"]], warn = FALSE)
  expect_true(any(grepl("Principal component analysis", qmd, fixed = TRUE)))
  expect_true(any(grepl("Reproducibility", qmd, fixed = TRUE)))
})

test_that("report rendering gives actionable Quarto error", {
  pca <- new_pca_result(data.frame(sample_id = "a", PC1 = 0, PC2 = 0), c(1, .5))
  if (!nzchar(Sys.which("quarto"))) {
    expect_error(write_population_genomics_report(list(pca), tempfile(), render = TRUE), "Quarto is required")
  } else {
    succeed()
  }
})
