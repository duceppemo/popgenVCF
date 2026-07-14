test_that("default PCA module declares complete publication artifacts", {
  registry <- popgenVCF::default_analysis_registry()
  module <- registry$modules$pca

  expect_setequal(
    module$artifacts,
    c(
      "coordinates", "variance", "pc1_pc2_pdf", "pc1_pc2_svg",
      "pc1_pc2_png", "methods", "caption", "validation", "figure_source"
    )
  )
  expect_true(module$artifacts_must_exist)
})

test_that("plot saving omits raster-only dpi for vector devices", {
  out <- tempfile("plot-output-")
  dirs <- list(figures = file.path(out, "figures"))
  dir.create(dirs$figures, recursive = TRUE)
  p <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) +
    ggplot2::geom_point()

  expect_silent(popgenVCF:::save_plot(p, "device_test", dirs, c("pdf", "png"), dpi = 150L))
  expect_true(file.exists(file.path(dirs$figures, "device_test.pdf")))
  expect_true(file.exists(file.path(dirs$figures, "device_test.png")))

  if (requireNamespace("svglite", quietly = TRUE)) {
    expect_silent(popgenVCF:::save_plot(p, "device_test", dirs, "svg", dpi = 150L))
    expect_true(file.exists(file.path(dirs$figures, "device_test.svg")))
  }
})

test_that("PCA and IBS plots support VCF-only data without population metadata", {
  out <- tempfile("ordination-output-")
  dirs <- list(figures = file.path(out, "figures"))
  dir.create(dirs$figures, recursive = TRUE)
  cfg <- list(
    output = list(
      figure_formats = "pdf",
      dpi = 150L,
      label_samples = "none"
    )
  )

  pca <- list(
    scores = data.table::data.table(
      sample = c("s1", "s2", "s3"),
      PC1 = c(-1, 0, 1),
      PC2 = c(0.5, -0.5, 0)
    ),
    variance = data.table::data.table(
      PC = c("PC1", "PC2"),
      proportion = c(0.6, 0.4),
      percent = c(60, 40)
    )
  )
  expect_silent(popgenVCF:::plot_pca(pca, cfg, dirs))
  expect_true(file.exists(file.path(dirs$figures, "07_PCA_PC1_PC2.pdf")))

  distance <- matrix(
    c(0, 0.2, 0.4, 0.2, 0, 0.3, 0.4, 0.3, 0),
    nrow = 3,
    dimnames = list(c("s1", "s2", "s3"), c("s1", "s2", "s3"))
  )
  ibs <- list(
    mds = data.table::data.table(
      sample = c("s1", "s2", "s3"),
      MDS1 = c(-1, 0, 1),
      MDS2 = c(0.5, -0.5, 0)
    ),
    distance = distance
  )
  expect_silent(popgenVCF:::plot_ibs(ibs, cfg, dirs))
  expect_true(file.exists(file.path(dirs$figures, "08_IBS_MDS.pdf")))
  expect_true(file.exists(file.path(dirs$figures, "09_IBS_heatmap.pdf")))
})
