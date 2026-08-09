test_that("pca_metadata_colour_columns keeps factor-like columns with enough samples per level", {
  metadata <- data.table::data.table(
    sample = paste0("s", 1:12),
    population = rep(c("popA", "popB"), each = 6),
    sex = rep(c("M", "F"), 6),
    species = "onlyone",
    free_id = paste0("uid", 1:12),
    batch = rep(c("1", "2", "3"), 4)
  )
  cols <- popgenVCF:::pca_metadata_colour_columns(
    metadata, metadata$sample, min_group = 3L, max_levels = 12L
  )
  expect_setequal(cols, c("sex", "batch"))
  expect_false("population" %in% cols)
  expect_false("species" %in% cols)
  expect_false("free_id" %in% cols)
})

test_that("pca_metadata_colour_columns drops levels below min_group and re-checks remaining level count", {
  metadata <- data.table::data.table(
    sample = paste0("s", 1:8),
    rare = c("A", "A", "A", "B", "B", "B", "C", "D")
  )
  cols <- popgenVCF:::pca_metadata_colour_columns(
    metadata, metadata$sample, min_group = 3L, max_levels = 12L
  )
  expect_identical(cols, "rare")

  metadata2 <- data.table::data.table(
    sample = paste0("s", 1:8),
    mostly_unique = c("A", "A", "A", "B", "C", "D", "E", "F")
  )
  cols2 <- popgenVCF:::pca_metadata_colour_columns(
    metadata2, metadata2$sample, min_group = 3L, max_levels = 12L
  )
  expect_length(cols2, 0L)
})

test_that("pca_metadata_colour_columns respects max_levels and ignores missing values", {
  metadata <- data.table::data.table(
    sample = paste0("s", 1:9),
    high_cardinality = as.character(1:9),
    with_missing = c(rep("X", 3L), rep("Y", 3L), NA, NA, NA)
  )
  cols <- popgenVCF:::pca_metadata_colour_columns(
    metadata, metadata$sample, min_group = 3L, max_levels = 3L
  )
  expect_identical(cols, "with_missing")
})

test_that("pca_metadata_display_name title-cases underscore-separated column names", {
  expect_identical(popgenVCF:::pca_metadata_display_name("sex"), "Sex")
  expect_identical(popgenVCF:::pca_metadata_display_name("collection_date"), "Collection Date")
})

test_that("plot_pca draws one PC1/PC2 panel per qualifying metadata column, skipping population/thin columns", {
  plots <- list()
  local_mocked_bindings(
    save_plot = function(p, stem, ...) {
      plots[[stem]] <<- p
      invisible(TRUE)
    },
    .package = "popgenVCF"
  )
  dirs <- list(figures = tempdir())
  cfg <- list(
    output = list(figure_formats = "pdf", dpi = 150L, label_samples = "none"),
    analyses = list(
      pca_metadata_color = TRUE, pca_metadata_color_min_group = 3L,
      pca_metadata_color_max_levels = 12L
    )
  )
  scores <- data.table::data.table(
    sample = paste0("s", 1:12), vcf_sample = paste0("s", 1:12),
    PC1 = rnorm(12), PC2 = rnorm(12),
    population = rep(c("popA", "popB"), each = 6)
  )
  pca <- list(
    scores = scores,
    variance = data.table::data.table(PC = c("PC1", "PC2"), proportion = c(0.6, 0.4), percent = c(60, 40)),
    loadings = NULL
  )
  metadata <- data.table::data.table(
    sample = paste0("s", 1:12),
    population = rep(c("popA", "popB"), each = 6),
    sex = rep(c("M", "F"), 6),
    species = "onlyone"
  )

  popgenVCF:::plot_pca(pca, cfg, dirs, metadata)

  expect_true("07_PCA_PC1_PC2" %in% names(plots))
  expect_true("07b_PCA_PC1_PC2_by_sex" %in% names(plots))
  expect_false("07b_PCA_PC1_PC2_by_population" %in% names(plots))
  expect_false("07b_PCA_PC1_PC2_by_species" %in% names(plots))

  by_sex <- plots[["07b_PCA_PC1_PC2_by_sex"]]
  expect_identical(by_sex$labels$colour, "Sex")
  expect_identical(by_sex$labels$title, "Principal component analysis, coloured by sex")
})

test_that("plot_pca skips metadata colouring entirely when disabled or no metadata is supplied", {
  plots <- list()
  local_mocked_bindings(
    save_plot = function(p, stem, ...) {
      plots[[stem]] <<- p
      invisible(TRUE)
    },
    .package = "popgenVCF"
  )
  dirs <- list(figures = tempdir())
  scores <- data.table::data.table(
    sample = paste0("s", 1:6), vcf_sample = paste0("s", 1:6),
    PC1 = rnorm(6), PC2 = rnorm(6)
  )
  pca <- list(
    scores = scores,
    variance = data.table::data.table(PC = c("PC1", "PC2"), proportion = c(0.6, 0.4), percent = c(60, 40)),
    loadings = NULL
  )
  metadata <- data.table::data.table(
    sample = paste0("s", 1:6), sex = rep(c("M", "F"), 3L)
  )

  cfg_disabled <- list(
    output = list(figure_formats = "pdf", dpi = 150L, label_samples = "none"),
    analyses = list(pca_metadata_color = FALSE, pca_metadata_color_min_group = 3L, pca_metadata_color_max_levels = 12L)
  )
  popgenVCF:::plot_pca(pca, cfg_disabled, dirs, metadata)
  expect_false(any(grepl("_by_", names(plots))))

  cfg_enabled <- list(
    output = list(figure_formats = "pdf", dpi = 150L, label_samples = "none"),
    analyses = list(pca_metadata_color = TRUE, pca_metadata_color_min_group = 3L, pca_metadata_color_max_levels = 12L)
  )
  popgenVCF:::plot_pca(pca, cfg_enabled, dirs, metadata = NULL)
  expect_false(any(grepl("_by_", names(plots))))
})
