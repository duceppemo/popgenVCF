minimal_standard_report_result <- function() {
  list(
    metadata = data.frame(
      sample = c("sample_1", "sample_2"),
      population = c("north", "south")
    ),
    qc_snps = 1:4,
    final_snps = 1:3,
    qc = list(sequential = data.frame(stage = "retained", variants = 4L)),
    diversity = list(population = data.frame(
      population = c("north", "south"),
      observed_heterozygosity = c(0.2, 0.3)
    )),
    pca = list(scores = data.frame(
      sample = c("sample_1", "sample_2"), PC1 = c(-1, 1), PC2 = c(0, 0)
    )),
    fst = list(
      global = 0.1,
      long = data.frame(population_1 = "north", population_2 = "south", fst = 0.1)
    ),
    dapc = NULL,
    amova = NULL,
    ibd = NULL,
    admixture_cv = NULL,
    chromosome_summary = NULL,
    faststructure = NULL,
    snmf = NULL
  )
}

test_that("standard report inventory selects one browser-friendly figure per stem", {
  root <- tempfile("standard-report-")
  dir.create(file.path(root, "figures"), recursive = TRUE)
  results <- file.path(root, "analysis_results.rds")
  saveRDS(minimal_standard_report_result(), results)

  png_path <- file.path(root, "figures", "07_PCA_PC1_PC2.png")
  grDevices::png(png_path, width = 320, height = 240)
  graphics::plot(1:2, 1:2)
  grDevices::dev.off()
  grDevices::pdf(file.path(root, "figures", "07_PCA_PC1_PC2.pdf"))
  graphics::plot(1:2, 1:2)
  grDevices::dev.off()

  inventory <- popgenVCF:::report_figure_inventory(results)
  expect_equal(nrow(inventory), 1L)
  expect_identical(inventory$format, "png")
  expect_identical(inventory$stem, "07_PCA_PC1_PC2")
  expect_match(inventory$caption, "PCA PC 1 PC 2", fixed = TRUE)

  pdf_inventory <- popgenVCF:::report_figure_inventory(results, target = "pdf")
  expect_equal(nrow(pdf_inventory), 1L)
  expect_identical(pdf_inventory$format, "pdf")
})

test_that("standard HTML report embeds figures without requiring an RDS viewer", {
  skip_if_not(rmarkdown::pandoc_available())
  root <- tempfile("standard-report-render-")
  dir.create(file.path(root, "figures"), recursive = TRUE)
  results <- file.path(root, "analysis_results.rds")
  saveRDS(minimal_standard_report_result(), results)

  png_path <- file.path(root, "figures", "03_sample_missingness.png")
  grDevices::png(png_path, width = 320, height = 240)
  graphics::barplot(c(0.1, 0.2), names.arg = c("sample_1", "sample_2"))
  grDevices::dev.off()

  report_dir <- file.path(root, "report")
  rendered <- render_report(
    results, report_dir, title = "Portable report", formats = "html"
  )
  expect_true(file.exists(rendered))
  expect_identical(names(rendered), "html")
  html <- paste(readLines(rendered[["html"]], warn = FALSE), collapse = "\n")
  expect_match(html, "Figure gallery", fixed = TRUE)
  expect_match(html, "data:image/png;base64", fixed = TRUE)
  expect_match(html, "03_sample_missingness.png", fixed = TRUE)
  expect_match(html, "reproducibility archive", fixed = TRUE)
})

test_that("reduced HTML reports omit unavailable analysis sections", {
  skip_if_not(rmarkdown::pandoc_available())
  root <- tempfile("reduced-standard-report-")
  dir.create(root)
  results <- file.path(root, "analysis_results.rds")
  reduced <- minimal_standard_report_result()
  reduced[c(
    "diversity", "fst", "dapc", "amova", "ibd", "admixture_cv",
    "chromosome_summary", "faststructure", "snmf"
  )] <- NULL
  saveRDS(reduced, results)

  rendered <- render_report(
    results, file.path(root, "report"), title = "Reduced report",
    formats = "html"
  )
  html <- paste(readLines(rendered[["html"]], warn = FALSE), collapse = "\n")

  expect_match(html, 'id="quality-control"', fixed = TRUE)
  expect_match(html, 'id="principal-component-analysis"', fixed = TRUE)
  expect_no_match(html, "The global Weir-Cockerham", fixed = TRUE)
  expect_no_match(html, 'id="genetic-diversity"', fixed = TRUE)
  expect_no_match(html, 'id="population-differentiation"', fixed = TRUE)
  expect_no_match(
    html, 'id="discriminant-analysis-of-principal-components"', fixed = TRUE
  )
  expect_no_match(html, 'id="analysis-of-molecular-variance"', fixed = TRUE)
  expect_no_match(html, 'id="admixture-model-cross-validation"', fixed = TRUE)
  expect_no_match(
    html, 'id="population-structure-reproducibility"', fixed = TRUE
  )
})

test_that("populated reproducibility sections render mixed-type tables", {
  skip_if_not(rmarkdown::pandoc_available())
  root <- tempfile("populated-reproducibility-report-")
  dir.create(root)
  results <- file.path(root, "analysis_results.rds")
  populated <- minimal_standard_report_result()
  populated$dapc <- list(diagnostics = data.table::data.table(
    K = 2L,
    assignment_accuracy = 0.8,
    replicate_max_rmse = 0.01
  ))
  populated$faststructure <- list(runs = data.table::data.table(
    K = 2L,
    exit_status = 0L,
    executable = "structure.py",
    log_file = "faststructure_K2.log",
    q_file = "faststructure_K2.meanQ"
  ))
  populated$snmf <- list(diagnostics = data.table::data.table(
    K = 2L, run = 1L, cross_entropy = 0.5
  ))
  saveRDS(populated, results)

  rendered <- expect_no_error(render_report(
    results, file.path(root, "report"),
    title = "Populated reproducibility report", formats = "html"
  ))
  html <- paste(readLines(rendered[["html"]], warn = FALSE), collapse = "\n")

  expect_match(
    html, 'id="population-structure-reproducibility"', fixed = TRUE
  )
  expect_match(html, 'id="faststructure"', fixed = TRUE)
  expect_match(
    html, 'id="sparse-non-negative-matrix-factorization"', fixed = TRUE
  )
  expect_match(html, "structure.py", fixed = TRUE)
})

test_that("standard PDF report uses compact vector figure sources", {
  skip_if_not(rmarkdown::pandoc_available())
  skip_if(is.null(popgenVCF:::report_latex_engine()), "No LaTeX engine")
  root <- tempfile("standard-report-pdf-")
  dir.create(file.path(root, "figures"), recursive = TRUE)
  results <- file.path(root, "analysis_results.rds")
  saveRDS(minimal_standard_report_result(), results)

  pdf_figure <- file.path(root, "figures", "07_PCA_PC1_PC2.pdf")
  grDevices::pdf(pdf_figure, width = 7, height = 5)
  graphics::plot(1:2, 1:2, main = "Principal component analysis")
  grDevices::dev.off()
  png_figure <- file.path(root, "figures", "07_PCA_PC1_PC2.png")
  grDevices::png(png_figure, width = 1200, height = 900)
  graphics::plot(1:2, 1:2, main = "Principal component analysis")
  grDevices::dev.off()

  rendered <- render_report(
    results, file.path(root, "report"), title = "Compact report",
    formats = "pdf"
  )
  expect_identical(names(rendered), "pdf")
  expect_true(file.exists(rendered[["pdf"]]))
  header <- readBin(rendered[["pdf"]], what = "raw", n = 4L)
  expect_identical(rawToChar(header), "%PDF")
  expect_lt(file.info(rendered[["pdf"]])$size, 1e6)
})

test_that("standard report rejects unsupported formats", {
  expect_error(
    render_report(tempfile(), tempfile(), formats = "docx"),
    "formats"
  )
})
