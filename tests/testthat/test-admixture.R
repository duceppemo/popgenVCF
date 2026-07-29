test_that("ADMIXTURE CV output parses", {
  x <- popgenVCF:::parse_admixture_cv("CV error (K=4): 0.123456")
  expect_equal(x$K, 4L)
  expect_equal(x$cv_error, 0.123456)
})

test_that("ADMIXTURE Q matrices join retained metadata explicitly", {
  root <- tempfile("admixture-q-")
  dir.create(root)
  q_file <- file.path(root, "cohort.2.Q")
  sample_file <- file.path(root, "samples.txt")

  writeLines(c("0.8 0.2", "0.1 0.9"), q_file)
  writeLines(c("sample_2", "sample_1"), sample_file)
  metadata <- data.table::data.table(
    sample = c("sample_1", "sample_2"),
    population = c("population_A", "population_B")
  )

  q <- popgenVCF:::read_admixture_q(q_file, sample_file, metadata)

  expect_equal(q$sample, c("sample_2", "sample_1"))
  expect_equal(q$population, c("population_B", "population_A"))
  expect_equal(
    rowSums(as.matrix(q[, c("cluster_1", "cluster_2"), with = FALSE])),
    c(1, 1)
  )
})

test_that("ADMIXTURE Q metadata joins reject duplicate identities", {
  root <- tempfile("admixture-q-duplicate-")
  dir.create(root)
  q_file <- file.path(root, "cohort.2.Q")
  sample_file <- file.path(root, "samples.txt")

  writeLines(c("0.8 0.2", "0.1 0.9"), q_file)
  writeLines(c("sample_1", "sample_2"), sample_file)
  metadata <- data.table::data.table(
    sample = c("sample_1", "sample_1"),
    population = c("population_A", "population_B")
  )

  expect_error(
    popgenVCF:::read_admixture_q(q_file, sample_file, metadata),
    "duplicate sample identifiers"
  )
})

test_that("membership figures label every bar with its public sample name", {
  captured <- NULL
  local_mocked_bindings(
    save_plot = function(p, ...) {
      captured <<- p
      invisible(TRUE)
    },
    .package = "popgenVCF"
  )
  q <- data.table::data.table(
    sample = c("raw_a", "raw_b", "raw_c"),
    population = c("north", "north", "south"),
    cluster_1 = c(0.9, 0.7, 0.2),
    cluster_2 = c(0.1, 0.3, 0.8)
  )
  labels <- c("Sample A", "Sample B", "Sample C")

  popgenVCF:::plot_q_matrix(
    q, 2L, default_config(), list(figures = tempdir()),
    sample_labels = labels
  )

  x_scale <- captured$scales$get_scales("x")
  expect_setequal(x_scale$labels, labels)
  expect_length(x_scale$breaks, nrow(q))
  expect_s3_class(captured$theme$axis.text.x, "element_text")
  expect_identical(captured$theme$axis.text.x$angle, 90)

  popgenVCF:::plot_q_matrix(
    q, 2L, default_config(), list(figures = tempdir()),
    prefix = "fastStructure_Q", sample_labels = labels
  )
  expect_match(captured$labels$title, "fastStructure", fixed = TRUE)

  popgenVCF:::plot_q_matrix(
    q, 2L, default_config(), list(figures = tempdir()),
    prefix = "sNMF_Q", sample_labels = labels
  )
  expect_match(captured$labels$title, "Sparse non-negative matrix factorization", fixed = TRUE)
})

test_that("membership output includes a population-free data-driven view", {
  plots <- list()
  local_mocked_bindings(
    save_plot = function(p, stem, ...) {
      plots[[stem]] <<- p
      invisible(TRUE)
    },
    .package = "popgenVCF"
  )
  q <- data.table::data.table(
    sample = c("weak_1", "strong_2", "strong_1", "weak_2"),
    population = c("z_population", "a_population", "z_population", "a_population"),
    cluster_1 = c(0.6, 0.05, 0.95, 0.4),
    cluster_2 = c(0.4, 0.95, 0.05, 0.6)
  )
  labels <- c("Weak 1", "Strong 2", "Strong 1", "Weak 2")

  popgenVCF:::plot_q_matrix_views(
    q, 2L, default_config(), list(figures = tempdir()),
    sample_labels = labels
  )

  expect_setequal(
    names(plots),
    c("14_ADMIXTURE_Q_K2", "14_ADMIXTURE_Q_data_driven_K2")
  )
  population_plot <- plots[["14_ADMIXTURE_Q_K2"]]
  data_plot <- plots[["14_ADMIXTURE_Q_data_driven_K2"]]
  expect_s3_class(population_plot$facet, "FacetGrid")
  expect_s3_class(data_plot$facet, "FacetNull")
  expect_identical(
    data_plot$scales$get_scales("x")$labels,
    c("Strong 1", "Weak 1", "Strong 2", "Weak 2")
  )
  expect_match(data_plot$labels$title, "data-driven cluster order", fixed = TRUE)
  built <- ggplot2::ggplot_build(data_plot)
  separator_layer <- which(vapply(
    data_plot$layers, function(layer) inherits(layer$geom, "GeomVline"), logical(1L)
  ))
  expect_equal(built$data[[separator_layer]]$xintercept, 2.5)
})

test_that("data-driven membership plots do not require population metadata", {
  local_mocked_bindings(
    save_plot = function(...) invisible(TRUE),
    .package = "popgenVCF"
  )
  q <- data.table::data.table(
    sample = c("sample_1", "sample_2"),
    cluster_1 = c(0.8, 0.2),
    cluster_2 = c(0.2, 0.8)
  )
  expect_no_error(popgenVCF:::plot_q_matrix(
    q, 2L, default_config(), list(figures = tempdir()),
    order_mode = "data_driven"
  ))
})

test_that("large membership figures retain labels within device limits", {
  dimensions <- NULL
  captured <- NULL
  local_mocked_bindings(
    save_plot = function(p, stem, dirs, formats, width, height, dpi) {
      captured <<- p
      dimensions <<- c(width = width, height = height)
      invisible(TRUE)
    },
    .package = "popgenVCF"
  )
  n <- 700L
  q <- data.table::data.table(
    sample = sprintf("raw_%03d", seq_len(n)),
    population = rep(c("north", "south"), each = n / 2L),
    cluster_1 = seq(0.05, 0.95, length.out = n)
  )
  q[, cluster_2 := 1 - cluster_1]
  labels <- sprintf("Sample %03d", seq_len(n))

  popgenVCF:::plot_q_matrix(
    q, 2L, default_config(), list(figures = tempdir()),
    sample_labels = labels
  )

  expect_lte(dimensions[["width"]], 48)
  expect_length(captured$scales$get_scales("x")$labels, n)
  expect_gt(captured$theme$axis.text.x$size, 0)
})

test_that("membership figures reject ambiguous sample labels", {
  q <- data.table::data.table(
    sample = c("raw_a", "raw_b"),
    population = c("north", "south"),
    cluster_1 = c(0.9, 0.2),
    cluster_2 = c(0.1, 0.8)
  )

  expect_error(
    popgenVCF:::plot_q_matrix(
      q, 2L, default_config(), list(figures = tempdir()),
      sample_labels = c("same", "same")
    ),
    "unique and non-empty"
  )
})
