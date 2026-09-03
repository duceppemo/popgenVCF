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

test_that("ADMIXTURE Q matrices are produced without a population column when metadata has none", {
  # A real production incident: a real 50-sample cohort with no population
  # metadata crashed the whole pipeline here with "ADMIXTURE metadata
  # requires sample and population columns", even though ADMIXTURE itself
  # (an unsupervised method) had already computed successfully.
  root <- tempfile("admixture-q-no-population-")
  dir.create(root)
  q_file <- file.path(root, "cohort.2.Q")
  sample_file <- file.path(root, "samples.txt")

  writeLines(c("0.8 0.2", "0.1 0.9"), q_file)
  writeLines(c("sample_2", "sample_1"), sample_file)
  metadata <- data.table::data.table(sample = c("sample_1", "sample_2"))

  q <- popgenVCF:::read_admixture_q(q_file, sample_file, metadata)

  expect_false("population" %in% names(q))
  expect_equal(q$sample, c("sample_2", "sample_1"))
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

test_that("population-organized membership plots shrink the facet strip text so a small population's own label isn't clipped by its narrow panel", {
  # Reported directly against a real production report: facet_grid(...,
  # space = "free_x") gives each population's strip exactly its own
  # panel's width, proportional to its sample count -- a 2-sample
  # population's strip rendered as just "o2" instead of its real label
  # "Ro2-3", silently clipped rather than wrapped or shrunk.
  captured <- NULL
  local_mocked_bindings(
    save_plot = function(p, ...) {
      captured <<- p
      invisible(TRUE)
    },
    .package = "popgenVCF"
  )
  # plot_width floors at 10in for any total sample count under ~125
  # (min(48, max(10, n * 0.08))), so points-per-sample -- and thus how
  # binding a small population's own strip really is -- only drops to a
  # realistic level once the total sample count is large enough (as in a
  # real multi-population cohort); a too-small fixture stays comfortably
  # within the floor's slack and never reproduces the clipping.
  q <- data.table::data.table(
    sample = c(sprintf("wide_%02d", 1:97), sprintf("narrow_%d", 1:3)),
    population = c(rep("MostlyThisPopulation", 97L), rep("Ro2-3", 3L)),
    cluster_1 = c(rep(0.9, 97L), rep(0.1, 3L)),
    cluster_2 = c(rep(0.1, 97L), rep(0.9, 3L))
  )
  popgenVCF:::plot_q_matrix(q, 2L, default_config(), list(figures = tempdir()))

  expect_s3_class(captured$theme$strip.text, "element_text")
  expect_lt(captured$theme$strip.text$size, popgenVCF:::figure_base_size(default_config()))
  expect_gte(captured$theme$strip.text$size, 4)

  # Control: evenly-sized, generously-wide populations need no shrinking --
  # the strip text stays at the theme's own base size.
  even_captured <- NULL
  local_mocked_bindings(
    save_plot = function(p, ...) {
      even_captured <<- p
      invisible(TRUE)
    },
    .package = "popgenVCF"
  )
  q_even <- data.table::data.table(
    sample = sprintf("s_%02d", 1:20),
    population = rep(c("north", "south"), each = 10L),
    cluster_1 = rep(c(0.9, 0.1), each = 10L),
    cluster_2 = rep(c(0.1, 0.9), each = 10L)
  )
  popgenVCF:::plot_q_matrix(q_even, 2L, default_config(), list(figures = tempdir()))
  expect_equal(even_captured$theme$strip.text$size, popgenVCF:::figure_base_size(default_config()))

  # data_driven mode has no population facet at all -- strip text size is
  # simply irrelevant there, confirmed by not erroring.
  local_mocked_bindings(save_plot = function(...) invisible(TRUE), .package = "popgenVCF")
  expect_no_error(popgenVCF:::plot_q_matrix(
    q, 2L, default_config(), list(figures = tempdir()), order_mode = "data_driven"
  ))
})

test_that("population_ancestry_similarity_order clusters similar populations adjacent, not alphabetically", {
  # A_pop and C_pop share a near-identical, cluster-1-dominant ancestry
  # profile; B_pop is the opposite (cluster-2-dominant). Alphabetical order
  # (A_pop, B_pop, C_pop) is the worst possible arrangement here, sandwiching
  # the two similar populations apart. The similarity order should place
  # A_pop and C_pop next to each other.
  x <- data.table::data.table(
    population = c("A_pop", "A_pop", "B_pop", "B_pop", "C_pop", "C_pop"),
    cluster_1 = c(0.9, 0.85, 0.1, 0.15, 0.88, 0.82),
    cluster_2 = c(0.1, 0.15, 0.9, 0.85, 0.12, 0.18)
  )
  ord <- popgenVCF:::population_ancestry_similarity_order(x, c("cluster_1", "cluster_2"))
  expect_setequal(ord, c("A_pop", "B_pop", "C_pop"))
  a_pos <- which(ord == "A_pop"); c_pos <- which(ord == "C_pop")
  expect_equal(abs(a_pos - c_pos), 1L)
})

test_that("population_ancestry_similarity_order falls back to alphabetical order with fewer than two populations", {
  x <- data.table::data.table(population = "solo", cluster_1 = 0.5, cluster_2 = 0.5)
  expect_identical(popgenVCF:::population_ancestry_similarity_order(x, c("cluster_1", "cluster_2")), "solo")
})

test_that("plot_q_matrix's population-organized view orders and facets populations by ancestry similarity, not alphabetically", {
  captured <- NULL
  local_mocked_bindings(
    save_plot = function(p, ...) {
      captured <<- p
      invisible(TRUE)
    },
    .package = "popgenVCF"
  )
  q <- data.table::data.table(
    sample = c("s1", "s2", "s3", "s4", "s5", "s6"),
    population = c("A_pop", "A_pop", "B_pop", "B_pop", "C_pop", "C_pop"),
    cluster_1 = c(0.9, 0.85, 0.1, 0.15, 0.88, 0.82),
    cluster_2 = c(0.1, 0.15, 0.9, 0.85, 0.12, 0.18)
  )
  popgenVCF:::plot_q_matrix(q, 2L, default_config(), list(figures = tempdir()))

  expect_s3_class(captured$data$population, "factor")
  built_populations <- levels(captured$data$population)
  expect_setequal(built_populations, c("A_pop", "B_pop", "C_pop"))
  a_pos <- which(built_populations == "A_pop"); c_pos <- which(built_populations == "C_pop")
  expect_equal(abs(a_pos - c_pos), 1L)
  expect_false(identical(built_populations, c("A_pop", "B_pop", "C_pop")))
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
