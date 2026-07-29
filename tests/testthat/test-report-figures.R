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
  rendered <- render_report(results, report_dir, title = "Portable report")
  expect_true(file.exists(rendered))
  html <- paste(readLines(rendered, warn = FALSE), collapse = "\n")
  expect_match(html, "Figure gallery", fixed = TRUE)
  expect_match(html, "data:image/png;base64", fixed = TRUE)
  expect_match(html, "03_sample_missingness.png", fixed = TRUE)
  expect_match(html, "reproducibility archive", fixed = TRUE)
})
