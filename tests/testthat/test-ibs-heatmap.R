ibs_heatmap_fixture <- function() {
  matrix(
    c(
      0, 0.1, 0.7, 0.8,
      0.1, 0, 0.6, 0.7,
      0.7, 0.6, 0, 0.2,
      0.8, 0.7, 0.2, 0
    ),
    nrow = 4L,
    byrow = TRUE,
    dimnames = list(
      c("Sample A", "Sample B", "Sample C", "Sample D"),
      c("Sample A", "Sample B", "Sample C", "Sample D")
    )
  )
}

test_that("IBS dendrogram leaves align with clustered heatmap rows", {
  distance <- ibs_heatmap_fixture()
  tree <- stats::hclust(stats::as.dist(distance), method = "average")
  segments <- popgenVCF:::hclust_dendrogram_segments(tree)

  expect_equal(nrow(segments), 3L * (nrow(distance) - 1L))
  expect_setequal(segments[height == 0, y], seq_len(nrow(distance)))
  expect_equal(max(segments$height_end), max(tree$height))
})

test_that("IBS heatmap includes sample names and a row dendrogram", {
  distance <- ibs_heatmap_fixture()
  p <- popgenVCF:::ibs_heatmap_plot(distance)
  x_scale <- p$scales$get_scales("x")
  y_scale <- p$scales$get_scales("y")

  expect_identical(x_scale$labels, y_scale$labels)
  expect_setequal(x_scale$labels, rownames(distance))
  expect_identical(y_scale$position, "right")
  expect_length(x_scale$breaks, nrow(distance))
  expect_length(y_scale$breaks, nrow(distance))
  expect_length(p$layers, 2L)
  expect_s3_class(p$layers[[2L]]$geom, "GeomSegment")
  expect_equal(nrow(p$layers[[2L]]$data), 3L * (nrow(distance) - 1L))
  expect_identical(p$labels$title, "Pairwise identity-by-state distance")
})

test_that("plot_ibs saves the labeled dendrogram heatmap", {
  plots <- list()
  dimensions <- list()
  local_mocked_bindings(
    save_plot = function(p, stem, dirs, formats, width, height, dpi) {
      plots[[stem]] <<- p
      dimensions[[stem]] <<- c(width = width, height = height)
      invisible(TRUE)
    },
    .package = "popgenVCF"
  )
  distance <- ibs_heatmap_fixture()
  ibs <- list(
    mds = data.table::data.table(
      sample = rownames(distance),
      MDS1 = c(-1, -0.8, 0.8, 1),
      MDS2 = c(0.2, -0.2, 0.2, -0.2)
    ),
    distance = distance
  )
  cfg <- default_config()

  popgenVCF:::plot_ibs(ibs, cfg, list(figures = tempdir()))

  expect_setequal(names(plots), c("08_IBS_MDS", "09_IBS_heatmap"))
  expect_gt(dimensions[["09_IBS_heatmap"]][["width"]],
            dimensions[["09_IBS_heatmap"]][["height"]])
  expect_match(
    plots[["09_IBS_heatmap"]]$labels$subtitle,
    "dendrogram",
    ignore.case = TRUE
  )
  expect_identical(
    plots[["08_IBS_MDS"]]$labels$title,
    "Multidimensional scaling of identity-by-state distance"
  )
})

test_that("IBS heatmap rejects inconsistent sample names", {
  distance <- ibs_heatmap_fixture()
  colnames(distance)[[1L]] <- "mismatch"

  expect_error(
    popgenVCF:::ibs_heatmap_plot(distance),
    "row and column sample names"
  )
})
