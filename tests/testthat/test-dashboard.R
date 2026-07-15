test_that("dashboard summaries and quality scores are transparent", {
  ids <- c("s1", "s2", "s3")
  metadata <- data.frame(sample_id = ids, population = c("A", "A", "B"))
  pca <- new_pca_result(
    data.frame(sample_id = ids, PC1 = c(-1, 0, 1), PC2 = c(.2, -.1, -.1)),
    c(2, 1), metadata = metadata,
    provenance = list(package = "SNPRelate", input_hashes = data.frame(file = "input.vcf.gz", sha256 = "abc")),
    validation = data.frame(check = c("coordinates", "eigenvalues"), passed = TRUE)
  )
  fst <- new_fst_result(
    .12,
    data.frame(population_1 = "A", population_2 = "B", fst = .12),
    metadata = metadata,
    provenance = list(method = "Weir-Cockerham"),
    validation = data.frame(check = "global_pairwise_consistency", passed = TRUE)
  )
  plan <- build_population_genomics_report_plan(list(fst = fst, pca = pca))
  summary <- build_dashboard_summary(plan)
  quality <- calculate_scientific_quality(plan)

  expect_equal(summary$samples, 3L)
  expect_equal(summary$populations, 2L)
  expect_equal(summary$global_fst, .12)
  expect_true(is.finite(quality$score))
  expect_true(all(c("component", "weight", "applicable", "fraction", "reason") %in% names(quality$components)))
  expect_equal(sum(quality$components$weight), 1)
})

test_that("dashboard source mode emits a reproducibility bundle", {
  ids <- c("a", "b")
  pca <- new_pca_result(
    data.frame(sample_id = ids, PC1 = c(-1, 1), PC2 = c(0, 0)),
    c(2, 1),
    provenance = list(package = "SNPRelate"),
    validation = data.frame(check = "scientific", passed = TRUE)
  )
  out <- tempfile("popgen-dashboard-")
  dashboard <- write_population_genomics_dashboard(list(pca = pca), out, render = FALSE)

  expect_s3_class(dashboard$plan, "PopgenVCFReportPlan")
  expect_s3_class(dashboard$artifacts, "PopgenVCFArtifactManifest")
  expect_true(all(file.exists(artifact_manifest_table(dashboard$artifacts)$path)))
  expect_true(file.exists(file.path(out, "dashboard_summary.json")))
  expect_true(file.exists(file.path(out, "scientific_quality.json")))
  expect_true(file.exists(file.path(out, "population_genomics_reproducibility.tar.gz")))

  qmd <- readLines(file.path(out, "population_genomics_dashboard.qmd"), warn = FALSE)
  expect_true(any(grepl("Quality score", qmd, fixed = TRUE)))
  expect_true(any(grepl("Interactive PCA", qmd, fixed = TRUE)))
})

test_that("dashboard handles absent optional metadata and artifacts", {
  sim <- diag(2)
  rownames(sim) <- colnames(sim) <- c("x", "y")
  ibs <- new_ibs_result(sim, provenance = list(package = "SNPRelate"))
  plan <- build_population_genomics_report_plan(list(ibs = ibs))
  summary <- build_dashboard_summary(plan)
  quality <- calculate_scientific_quality(plan)

  expect_equal(summary$populations, 0L)
  expect_false(quality$components[component == "artifact_integrity", applicable])
  expect_true(is.finite(quality$score))
})

test_that("dashboard rendering gives actionable Quarto guidance", {
  pca <- new_pca_result(data.frame(sample_id = "a", PC1 = 0, PC2 = 0), 1,
                        provenance = list(package = "SNPRelate"))
  if (!nzchar(Sys.which("quarto"))) {
    expect_error(
      write_population_genomics_dashboard(list(pca = pca), tempfile(), render = TRUE),
      "Quarto is required"
    )
  }
})
