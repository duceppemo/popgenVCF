# plot_dapc_xval() renders the diagnostic curve behind DAPC's existing,
# already-on-by-default cross-validated PC-count selection
# (analyses.dapc_cross_validation, run_dapc_k_task()'s adegenet::xvalDapc()
# call) -- adegenet::xvalDapc(..., xval.plot = FALSE) computes this curve
# every run already, but the curve itself was discarded, only the final
# selected count kept. This draws it directly from the retained `cv` object,
# with no new statistical computation of its own. Fixture mirrors the real
# structure of an adegenet::xvalDapc() return value (confirmed directly
# against a real call), not a guessed shape.
dapc_xval_fixture <- function() {
  success <- c(`2` = 0.40, `4` = 0.62, `6` = 0.78, `8` = 0.81, `10` = 0.75)
  structure(
    list(
      `Mean Successful Assignment by Number of PCs of PCA` = success,
      `Median and Confidence Interval for Random Chance` = c(
        `2.5%` = 0.30, `50%` = 0.34, `97.5%` = 0.38
      ),
      `Number of PCs Achieving Highest Mean Success` = "8"
    ),
    class = "list"
  )
}

test_that("plot_dapc_xval draws the cross-validation curve from the retained cv object", {
  cv <- dapc_xval_fixture()
  profile <- popgenVCF:::figure_style_profile("accessibility-first")
  cfg <- list(output = list(figure_formats = "pdf", dpi = 150L))
  dirs <- list(figures = withr::local_tempdir())

  p <- popgenVCF:::plot_dapc_xval(cv, "3", cfg, dirs, profile)

  expect_s3_class(p, "ggplot")
  expect_identical(nrow(p$data), 5L)
  expect_identical(p$data$n_pca, c(2L, 4L, 6L, 8L, 10L))
  expect_identical(p$labels$subtitle, "Selected 8 PC(s) by highest mean assignment success")
  expect_true(file.exists(file.path(dirs$figures, "12b_DAPC_xval_K3.pdf")))
})

test_that("plot_dapc_xval does nothing when cross-validation did not run or produced no curve", {
  profile <- popgenVCF:::figure_style_profile("accessibility-first")
  cfg <- list(output = list(figure_formats = "pdf", dpi = 150L))
  dirs <- list(figures = withr::local_tempdir())

  expect_null(popgenVCF:::plot_dapc_xval(NULL, "3", cfg, dirs, profile))
  expect_length(list.files(dirs$figures), 0L)

  empty_cv <- list(`Mean Successful Assignment by Number of PCs of PCA` = numeric())
  expect_null(popgenVCF:::plot_dapc_xval(empty_cv, "3", cfg, dirs, profile))
  expect_length(list.files(dirs$figures), 0L)
})

test_that("plot_dapc calls plot_dapc_xval once per K using each model's own retained cv", {
  calls <- list()
  local_mocked_bindings(
    plot_dapc_xval = function(cv, k, cfg, dirs, profile) {
      calls[[k]] <<- cv
      NULL
    },
    save_plot = function(...) invisible(NULL),
    plot_q_matrix_views = function(...) invisible(NULL),
    .package = "popgenVCF"
  )
  coordinates <- data.table::data.table(
    sample = c("sample_1", "sample_2", "sample_3", "sample_4"),
    population = c("A", "A", "B", "B"),
    cluster = c("1", "1", "2", "2"),
    LD1 = c(-2, -1, 1, 2),
    LD2 = c(-0.5, 0.5, -0.5, 0.5)
  )
  membership <- matrix(
    c(0.9, 0.1, 0.8, 0.2, 0.2, 0.8, 0.1, 0.9),
    nrow = 4L, byrow = TRUE,
    dimnames = list(coordinates$sample, c("cluster_1", "cluster_2"))
  )
  fixture <- list(
    models = list(`2` = list(
      coordinates = coordinates, membership = membership,
      reproducibility = NULL, loadings = NULL, cv = dapc_xval_fixture()
    )),
    diagnostics = data.table::data.table(K = 2L, replicate_max_rmse = NA_real_)
  )
  cfg <- list(output = list(figure_formats = "pdf", dpi = 150L, figure_style = "accessibility-first"),
              analyses = list(structure = list(reproducibility_rmse = 0.05)))

  popgenVCF:::plot_dapc(fixture, cfg, list(figures = withr::local_tempdir()))

  expect_identical(names(calls), "2")
  expect_identical(calls[["2"]], dapc_xval_fixture())
})
