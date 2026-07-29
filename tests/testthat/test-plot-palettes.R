test_that("population figures share a high-contrast accessible palette", {
  expected <- c(
    A = "#0072B2", B = "#E69F00", C = "#009E73", D = "#D55E00",
    E = "#CC79A7", F = "#56B4E9"
  )

  expect_identical(popgenVCF:::population_palette(names(expected)), expected)
  expect_identical(
    unname(popgenVCF:::population_palette(rev(names(expected)))),
    unname(expected)
  )
})

test_that("requested population plots use accessible colors and expanded titles", {
  plots <- list()
  local_mocked_bindings(
    save_plot = function(p, stem, ...) {
      plots[[stem]] <<- p
      invisible(TRUE)
    },
    .package = "popgenVCF"
  )
  cfg <- default_config()
  cfg$output$label_samples <- "none"
  populations <- c("A", "B", "C")
  expected <- population_palette(populations)
  sample_qc <- data.table::data.table(
    sample = paste0("sample_", seq_along(populations)),
    population = populations,
    missing_rate = c(0.01, 0.02, 0.03)
  )
  reports <- list(
    variant = data.table::data.table(
      maf = c(0.1, 0.2), missing_rate = c(0.01, 0.02)
    ),
    sequential = data.table::data.table(
      step = c("Input", "Retained"), variants = c(10L, 8L)
    )
  )
  plot_qc_reports(reports, sample_qc, cfg, list(figures = tempdir()))

  div <- list(
    sample = data.table::data.table(
      population = rep(populations, each = 2L),
      observed_heterozygosity = seq(0.1, 0.35, length.out = 6L)
    ),
    population = data.table::data.table(
      population = populations,
      observed_heterozygosity = c(0.2, 0.25, 0.3),
      expected_heterozygosity = c(0.22, 0.27, 0.32)
    )
  )
  plot_diversity(div, data.table::data.table(), cfg, list(figures = tempdir()))

  pca <- list(
    scores = data.table::data.table(
      sample = paste0("sample_", seq_along(populations)),
      population = populations,
      PC1 = c(-1, 0, 1), PC2 = c(0.5, -0.5, 0)
    ),
    variance = data.table::data.table(
      PC = c("PC1", "PC2"), proportion = c(0.6, 0.4),
      percent = c(60, 40)
    )
  )
  plot_pca(pca, cfg, list(figures = tempdir()))

  distance <- matrix(
    c(0, 0.2, 0.4, 0.2, 0, 0.3, 0.4, 0.3, 0),
    nrow = 3L, dimnames = list(sample_qc$sample, sample_qc$sample)
  )
  ibs <- list(
    mds = data.table::data.table(
      sample = sample_qc$sample, population = populations,
      MDS1 = c(-1, 0, 1), MDS2 = c(0.5, -0.5, 0)
    ),
    distance = distance
  )
  plot_ibs(ibs, cfg, list(figures = tempdir()))

  expect_identical(
    unname(plots[["03_sample_missingness"]]$scales$get_scales("fill")$palette(3L)),
    unname(expected)
  )
  expect_identical(
    unname(plots[["05_sample_heterozygosity"]]$scales$get_scales("fill")$palette(3L)),
    unname(expected)
  )
  expect_identical(
    unname(plots[["07_PCA_PC1_PC2"]]$scales$get_scales("colour")$palette(3L)),
    unname(expected)
  )
  expect_identical(
    unname(plots[["07_PCA_PC1_PC2"]]$scales$get_scales("shape")$palette(3L)),
    unname(population_shapes(populations))
  )
  expect_identical(
    unname(plots[["08_IBS_MDS"]]$scales$get_scales("colour")$palette(3L)),
    unname(expected)
  )
  expect_identical(
    unname(plots[["08_IBS_MDS"]]$scales$get_scales("shape")$palette(3L)),
    unname(population_shapes(populations))
  )
  expect_identical(
    unname(plots[["06_population_diversity"]]$scales$get_scales("fill")$palette(2L)),
    unname(diversity_metric_palette())
  )
  expect_identical(plots[["07_PCA_PC1_PC2"]]$labels$title,
                   "Principal component analysis")
  expect_identical(plots[["08_IBS_MDS"]]$labels$title,
                   "Multidimensional scaling of identity-by-state distance")
})

test_that("ancestry palettes are deterministic and configured explicitly", {
  q <- data.table::data.table(
    sample = c("s1", "s2"),
    population = c("A", "B"),
    cluster_1 = c(0.8, 0.2),
    cluster_2 = c(0.2, 0.8)
  )
  captured <- NULL
  local_mocked_bindings(
    save_plot = function(p, ...) {
      captured <<- p
      invisible(TRUE)
    },
    .package = "popgenVCF"
  )

  plot_q_matrix(q, 2L, default_config(), list(figures = tempdir()))

  expected <- cluster_palette(c("cluster_1", "cluster_2"))
  expect_identical(
    captured$scales$get_scales("fill")$palette(2L),
    expected
  )
  expect_identical(
    captured$scales$get_scales("fill")$name,
    "Ancestry component"
  )
})
