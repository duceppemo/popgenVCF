dapc_plot_fixture <- function(rmse = 0.01, with_replicates = TRUE) {
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
  list(
    models = list(
      `2` = list(
        coordinates = coordinates,
        membership = membership,
        reproducibility = if (with_replicates) list(metrics = data.frame(rmse = rmse)) else NULL
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

  expect_setequal(names(plots), c("11_DAPC_K2", "14_DAPC_membership_K2"))
  subtitles <- vapply(plots, function(p) p$labels$subtitle, character(1))
  expect_true(all(grepl("RMSE = 0.01", subtitles, fixed = TRUE)))
  expect_true(all(grepl("threshold = 0.05", subtitles, fixed = TRUE)))
  expect_false(any(grepl("WARNING", subtitles, fixed = TRUE)))
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
