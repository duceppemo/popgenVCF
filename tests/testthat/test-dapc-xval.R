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
  expect_identical(p$labels$x, "Number of PCA axes retained")
  expect_identical(p$labels$y, "Proportion of successful outcome prediction")
  expect_true(file.exists(file.path(dirs$figures, "12b_DAPC_xval_K3.pdf")))
})

test_that("plot_dapc_xval overlays every individual bootstrap replicate and the full random-chance band when the raw per-replicate results are present", {
  # Real motivation: a mean-only curve can look like a clean, confident peak
  # even when the underlying n.rep replicates are highly variable -- adegenet's
  # own retained `cv$\`Cross-Validation Results\`` (one row per replicate per
  # n.pca, confirmed against a real xvalDapc() call) is the raw data a user
  # actually needs to judge whether the auto-selected PC count is a robust
  # signal or noise, not just adegenet's own summarized mean.
  cv <- dapc_xval_fixture()
  cv[["Cross-Validation Results"]] <- data.frame(
    n.pca = rep(c(2, 4, 6, 8, 10), each = 3L),
    success = c(
      0.35, 0.40, 0.45, 0.55, 0.62, 0.69, 0.70, 0.78, 0.86,
      0.75, 0.81, 0.87, 0.65, 0.75, 0.85
    )
  )
  profile <- popgenVCF:::figure_style_profile("accessibility-first")
  cfg <- list(output = list(figure_formats = "pdf", dpi = 150L))
  dirs <- list(figures = withr::local_tempdir())

  p <- popgenVCF:::plot_dapc_xval(cv, "3", cfg, dirs, profile)

  jitter_layer <- Filter(
    function(l) inherits(l$geom, "GeomPoint") && is.data.frame(l$data), p$layers
  )
  expect_length(jitter_layer, 1L)
  expect_identical(nrow(jitter_layer[[1L]]$data), 15L)

  hline_layers <- Filter(function(l) inherits(l$geom, "GeomHline"), p$layers)
  expect_length(hline_layers, 3L)
  intercepts <- sort(unname(vapply(hline_layers, function(l) l$data$yintercept, numeric(1L))))
  expect_equal(intercepts, c(0.30, 0.34, 0.38))
})

test_that("plot_dapc_xval's jittered raw-replicate scatter is reproducible across repeated renders of the same data", {
  # Real regression: ggplot2::geom_jitter()'s default position_jitter()
  # draws a fresh random offset on every render (seed = NA), so the exact
  # same retained `cv` object produced a visibly different point cloud each
  # time the report was regenerated -- found comparing two real renders of
  # this package's own example report byte-for-byte, unrelated to any
  # actual change in the underlying cross-validation data. Fixed with a
  # fixed jitter seed.
  cv <- dapc_xval_fixture()
  cv[["Cross-Validation Results"]] <- data.frame(
    n.pca = rep(c(2, 4, 6, 8, 10), each = 3L),
    success = c(
      0.35, 0.40, 0.45, 0.55, 0.62, 0.69, 0.70, 0.78, 0.86,
      0.75, 0.81, 0.87, 0.65, 0.75, 0.85
    )
  )
  profile <- popgenVCF:::figure_style_profile("accessibility-first")
  cfg <- list(output = list(figure_formats = "pdf", dpi = 150L))

  build_jitter_x <- function() {
    p <- popgenVCF:::plot_dapc_xval(cv, "3", cfg, list(figures = withr::local_tempdir()), profile)
    idx <- which(vapply(
      p$layers, function(l) inherits(l$geom, "GeomPoint") && is.data.frame(l$data), logical(1L)
    ))[1L]
    ggplot2::ggplot_build(p)$data[[idx]]$x
  }

  expect_identical(build_jitter_x(), build_jitter_x())
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

test_that("dapc_eigenvalues_inset builds a minimal ggplot bar chart with no axis titles, no legend, and each bar's % contribution", {
  model <- list(eig = c(12.4, 5.1, 1.8, 0.3))
  profile <- popgenVCF:::figure_style_profile("accessibility-first")

  p <- popgenVCF:::dapc_eigenvalues_inset(model, profile)

  expect_s3_class(p, "ggplot")
  expect_identical(nrow(p$data), 4L)
  expect_identical(as.character(p$data$axis), as.character(1:4))
  expect_equal(p$data$eigenvalue, model$eig)
  expect_equal(p$data$contribution_pct, 100 * model$eig / sum(model$eig))
  # theme_void() base -- no axis titles/text/ticks at inset size.
  expect_true(inherits(p$theme$axis.title, "element_blank"))
  expect_true(inherits(p$theme$axis.text, "element_blank"))
})

test_that("dapc_eigenvalues_inset returns NULL when the model has no eigenvalues", {
  profile <- popgenVCF:::figure_style_profile("accessibility-first")
  expect_null(popgenVCF:::dapc_eigenvalues_inset(NULL, profile))
  expect_null(popgenVCF:::dapc_eigenvalues_inset(list(eig = numeric()), profile))
})

test_that("add_dapc_eigenvalues_inset places the inset in whichever quadrant has the fewest points", {
  inset <- ggplot2::ggplot()
  # Points in top-left, top-right, and bottom-right (2 each); bottom-left is
  # the only unambiguously empty quadrant (a diagonal line of points, tried
  # first, ties two corners at 0 and doesn't pin down which one is picked).
  x <- c(-7, -6, 7, 8, 7, 8); y <- c(7, 8, 7, 8, -7, -8)
  p <- ggplot2::ggplot(data.frame(x = x, y = y), ggplot2::aes(x, y)) + ggplot2::geom_point()

  result <- popgenVCF:::add_dapc_eigenvalues_inset(p, inset, x, y)
  built <- ggplot2::ggplot_build(result)
  custom_layer <- Filter(function(l) inherits(l$geom, "GeomCustomAnn"), result$layers)
  expect_length(custom_layer, 1L)
  grob_params <- custom_layer[[1L]]$geom_params
  xmid <- mean(range(x)); ymid <- mean(range(y))
  # bottom-left of the data range: xmax/ymax should sit at or below the
  # midpoint (allowing for the inset's own small size), not above it.
  expect_lt(grob_params$xmin, xmid)
  expect_lt(grob_params$ymin, ymid)
})

test_that("add_dapc_eigenvalues_inset returns the original plot unchanged when there is no inset or no finite coordinates", {
  p <- ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) + ggplot2::geom_point()
  expect_identical(popgenVCF:::add_dapc_eigenvalues_inset(p, NULL, 1, 1), p)
  expect_identical(
    popgenVCF:::add_dapc_eigenvalues_inset(p, ggplot2::ggplot(), NA_real_, NA_real_), p
  )
})

test_that("plot_dapc embeds each K's eigenvalues inset into its own saved scatter plot", {
  calls <- list()
  local_mocked_bindings(
    plot_dapc_xval = function(...) invisible(NULL),
    save_plot = function(p, stem, ...) {
      calls[[stem]] <<- p
      invisible(NULL)
    },
    plot_q_matrix_views = function(...) invisible(NULL),
    .package = "popgenVCF"
  )
  model_for <- function(k) {
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
    list(
      model = list(eig = seq_len(k - 1L)), coordinates = coordinates,
      membership = membership, reproducibility = NULL, loadings = NULL, cv = NULL
    )
  }
  fixture <- list(
    models = list(`9` = model_for(9L), `2` = model_for(2L), `4` = model_for(4L)),
    diagnostics = data.table::data.table(
      K = c(9L, 2L, 4L), replicate_max_rmse = c(NA_real_, NA_real_, NA_real_)
    )
  )
  cfg <- list(output = list(figure_formats = "pdf", dpi = 150L, figure_style = "accessibility-first"),
              analyses = list(structure = list(reproducibility_rmse = 0.05)))

  popgenVCF:::plot_dapc(fixture, cfg, list(figures = withr::local_tempdir()))

  for (k in c("9", "2", "4")) {
    p <- calls[[sprintf("11_DAPC_K%s", k)]]
    custom_layer <- Filter(function(l) inherits(l$geom, "GeomCustomAnn"), p$layers)
    expect_length(custom_layer, 1L)
  }
  # No more standalone 12c_DAPC_eigenvalues_K<k> figure.
  expect_false(any(grepl("^12c_DAPC_eigenvalues", names(calls))))
  expect_setequal(names(calls), c("11_DAPC_K9", "11_DAPC_K2", "11_DAPC_K4"))
})
