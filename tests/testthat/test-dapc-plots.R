dapc_plot_fixture <- function(rmse = 0.01, with_replicates = TRUE, with_loadings = FALSE) {
  coordinates <- data.table::data.table(
    sample = c("sample_1", "sample_2", "sample_3", "sample_4"),
    population = c("A", "A", "B", "B"),
    cluster = c("1", "1", "2", "2"),
    LD1 = c(-2, -1, 1, 2),
    LD2 = c(-0.5, 0.5, -0.5, 0.5)
  )
  membership <- matrix(
    c(0.9, 0.1, 0.8, 0.2, 0.2, 0.8, 0.1, 0.9),
    nrow = 4L,
    byrow = TRUE,
    dimnames = list(coordinates$sample, c("cluster_1", "cluster_2"))
  )
  loadings <- if (with_loadings) {
    data.table::data.table(
      axis = rep(c("LD1", "LD2"), each = 4L),
      snp_id = rep(c("snp_1", "snp_2", "snp_3", "snp_4"), 2L),
      chromosome = rep(c("1", "1", "2", "2"), 2L),
      position = rep(c(100, 200, 50, 900), 2L),
      contribution = c(0.4, 0.1, 0.3, 0.2, 0.05, 0.6, 0.15, 0.2)
    )
  } else {
    NULL
  }
  list(
    models = list(
      `2` = list(
        coordinates = coordinates,
        membership = membership,
        reproducibility = if (with_replicates) list(metrics = data.frame(rmse = rmse)) else NULL,
        loadings = loadings
      )
    ),
    diagnostics = data.table::data.table(K = 2L, replicate_max_rmse = rmse)
  )
}

test_that("DAPC figures report stable replicate membership RMSE", {
  plots <- list()
  local_mocked_bindings(
    save_plot = function(p, stem, ...) {
      plots[[stem]] <<- p
      invisible(TRUE)
    },
    .package = "popgenVCF"
  )
  cfg <- default_config()
  plot_dapc(dapc_plot_fixture(rmse = 0.01), cfg, list(figures = tempdir()))

  expect_setequal(
    names(plots),
    c("11_DAPC_K2", "14_DAPC_membership_K2", "14_DAPC_membership_data_driven_K2")
  )
  subtitles <- vapply(plots, function(p) p$labels$subtitle, character(1))
  expect_true(all(grepl("RMSE = 0.01", subtitles, fixed = TRUE)))
  expect_true(all(grepl("threshold = 0.05", subtitles, fixed = TRUE)))
  expect_false(any(grepl("WARNING", subtitles, fixed = TRUE)))
  membership_plot <- plots[["14_DAPC_membership_K2"]]
  data_plot <- plots[["14_DAPC_membership_data_driven_K2"]]
  expect_identical(data_plot$labels$y, "Posterior membership probability")
  expect_s3_class(data_plot$facet, "FacetNull")
  scatter_palette <- plots[["11_DAPC_K2"]]$scales$get_scales("colour")$palette(2L)
  expected_palette <- popgenVCF:::population_palette(c("A", "B"))
  expect_identical(scatter_palette, expected_palette)
  expect_identical(
    plots[["11_DAPC_K2"]]$labels$title,
    "Discriminant analysis of principal components (K = 2)"
  )
  expect_identical(
    membership_plot$labels$title,
    paste(
      "Discriminant analysis of principal components membership",
      "probabilities (K = 2)"
    )
  )
  expect_setequal(
    membership_plot$scales$get_scales("x")$labels,
    dapc_plot_fixture()$models[["2"]]$coordinates$sample
  )
})

test_that("DAPC loading plots are drawn only when a loadings table is present", {
  plots <- list()
  local_mocked_bindings(
    save_plot = function(p, stem, ...) {
      plots[[stem]] <<- p
      invisible(TRUE)
    },
    .package = "popgenVCF"
  )
  cfg <- default_config()
  plot_dapc(dapc_plot_fixture(with_loadings = TRUE), cfg, list(figures = tempdir()))

  expect_setequal(
    names(plots),
    c(
      "11_DAPC_K2", "14_DAPC_membership_K2", "14_DAPC_membership_data_driven_K2",
      "15_DAPC_loadings_manhattan_K2", "16_DAPC_loadings_ranked_K2"
    )
  )
  manhattan <- plots[["15_DAPC_loadings_manhattan_K2"]]
  ranked <- plots[["16_DAPC_loadings_ranked_K2"]]
  expect_s3_class(manhattan$facet, "FacetWrap")
  expect_s3_class(ranked$facet, "FacetWrap")
  expect_identical(manhattan$labels$x, "Chromosome")
  expect_identical(ranked$labels$x, "SNP rank (descending contribution)")
  expect_identical(
    manhattan$labels$title,
    "Discriminant analysis SNP loadings (K = 2)"
  )
  expect_identical(
    ranked$labels$title,
    "Discriminant analysis SNP loadings, ranked (K = 2)"
  )
})

test_that("DAPC loading plots are absent when no loadings table is supplied", {
  plots <- list()
  local_mocked_bindings(
    save_plot = function(p, stem, ...) {
      plots[[stem]] <<- p
      invisible(TRUE)
    },
    .package = "popgenVCF"
  )
  plot_dapc(dapc_plot_fixture(with_loadings = FALSE), default_config(), list(figures = tempdir()))

  expect_false(any(grepl("DAPC_loadings", names(plots))))
})

test_that("unstable DAPC figures warn against interpreting assignments", {
  plots <- list()
  local_mocked_bindings(
    save_plot = function(p, stem, ...) {
      plots[[stem]] <<- p
      invisible(TRUE)
    },
    .package = "popgenVCF"
  )
  cfg <- default_config()
  cfg$analyses$structure$reproducibility_rmse <- 0.05
  plot_dapc(dapc_plot_fixture(rmse = 0.12), cfg, list(figures = tempdir()))

  subtitles <- vapply(plots, function(p) p$labels$subtitle, character(1))
  expect_true(all(grepl("WARNING", subtitles, fixed = TRUE)))
  expect_true(all(grepl("RMSE = 0.12 > 0.05", subtitles, fixed = TRUE)))
  expect_true(all(grepl("Avoid interpreting these assignments", subtitles, fixed = TRUE)))
  expect_true(all(vapply(
    plots,
    function(p) identical(p$theme$plot.subtitle$colour, "#B2182B"),
    logical(1)
  )))
})

test_that("DAPC figures distinguish unavailable replicate RMSE", {
  annotation <- popgenVCF:::dapc_reproducibility_annotation(
    dapc_plot_fixture(rmse = 0, with_replicates = FALSE),
    2L,
    default_config()
  )

  expect_match(annotation$text, "RMSE not estimated")
  expect_false(annotation$unstable)
})

test_that("DAPC validation handles entirely missing replicate RMSE", {
  result <- dapc_plot_fixture(rmse = NA_real_, with_replicates = FALSE)
  validation <- expect_silent(popgenVCF:::validate_dapc_result(
    result,
    analysis = NULL,
    context = list(cfg = default_config())
  ))

  expect_true(validation$valid)
  expect_length(validation$warnings, 0L)
  expect_true(is.na(validation$metrics$maximum_replicate_rmse))
})
