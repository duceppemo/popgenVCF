# Real production report: a non-model reference genome assembled into dozens
# of short contigs with long accession-style names (e.g. "JAEVLN010000001.1")
# rendered every manhattan_chromosome_row()-based figure (PCA/DAPC loading
# manhattan plots, genome scan manhattan plots, pcadapt manhattan plot)
# completely unreadable: horizontal chromosome-name labels centered on
# narrow contigs collided into an illegible smear. These tests exercise the
# geometric fit/rotation/thinning logic directly, against both synthetic
# fixtures and the exact real contig names/lengths from the reporting user's
# dataset (JAEVLN010000001.1-JAEVLN010000088.1, a real WGS assembly).

test_that("manhattan_layout reports each chromosome's own physical span", {
  chromosome <- c("1", "1", "1", "2", "2")
  position <- c(100L, 500L, 900L, 50L, 250L)
  layout <- popgenVCF:::manhattan_layout(chromosome, position)

  expect_setequal(names(layout$ticks), c("chromosome", "center", "width"))
  expect_equal(layout$ticks$width[layout$ticks$chromosome == "1"], 800)
  expect_equal(layout$ticks$width[layout$ticks$chromosome == "2"], 200)
})

test_that("manhattan_chromosome_row keeps horizontal labels when they comfortably fit", {
  ticks <- data.frame(chromosome = c("chr1", "chr2"), center = c(50, 150), width = c(100, 100))
  p0 <- ggplot2::ggplot(data.frame(x = c(1, 190), y = c(-1, 1)), ggplot2::aes(x, y)) +
    ggplot2::geom_point()

  p <- popgenVCF:::manhattan_chromosome_row(p0, ticks, c(-1, 1), 11, plot_width_in = 10)
  layer <- p$layers[[length(p$layers)]]

  expect_identical(layer$aes_params$angle, 0)
  expect_identical(layer$aes_params$hjust, 0.5)
  expect_identical(nrow(layer$data), 2L)
  expect_identical(sort(layer$data$label), c("chr1", "chr2"))
  # A short label row leaves the plot's own x-axis title untouched.
  expect_null(p$theme$axis.title.x)
})

test_that("manhattan_chromosome_row switches to vertical, non-overlapping labels when they do not fit", {
  names60 <- sprintf("JAEVLN01%07d.1", seq_len(60))
  ticks <- data.frame(
    chromosome = names60, center = seq(50, by = 100, length.out = 60), width = rep(100, 60)
  )
  p0 <- ggplot2::ggplot(data.frame(x = c(1, 6000), y = c(-1, 1)), ggplot2::aes(x, y)) +
    ggplot2::geom_point()

  p <- popgenVCF:::manhattan_chromosome_row(p0, ticks, c(-1, 1), 11, plot_width_in = 10)
  layer <- p$layers[[length(p$layers)]]

  expect_identical(layer$aes_params$angle, 90)
  expect_identical(layer$aes_params$hjust, 1)
  expect_identical(layer$aes_params$vjust, 0.5)
  # Vertical mode uses a materially smaller, fixed font (7pt) independent of
  # base_size -- once forced vertical, exact legibility is secondary (real
  # identities belong in the accompanying TSVs), and a smaller font keeps
  # the row's own physical footprint small regardless of label count.
  expect_equal(layer$aes_params$size, 7 / 2.845276)
  # The smaller vertical-mode font needs less physical spacing per label,
  # so this particular 60-label/100-unit-spacing scenario fits without any
  # thinning at all -- a real, intended effect of shrinking the font (less
  # data discarded, not a regression). test-manhattan-chromosome-row.R's
  # "never drops every label, even under extreme crowding" test below
  # covers a scenario tight enough to still require thinning.
  expect_true(nrow(layer$data) >= 1L)
  expect_true(nrow(layer$data) <= 60L)
  # The redundant "Chromosome position" title is dropped once labels
  # themselves carry that meaning and would otherwise collide with it.
  expect_s3_class(p$theme$axis.title.x, "element_blank")
  # Real regression: an earlier version of this function grew the label
  # anchor's data-space offset (`pad`) to guarantee vertical clearance,
  # which visibly crushed the plotted data into a sliver of the panel to
  # make room. `pad` must stay exactly the same small, fixed 14%-of-range
  # value used in horizontal mode -- all real clearance for the (now
  # smaller) rotated labels belongs in plot.margin, not the data scale.
  expect_equal(layer$data$y[1], -1 - diff(c(-1, 1)) * 0.14)
})

test_that("manhattan_chromosome_row never drops every label, even under extreme crowding", {
  names500 <- sprintf("scaffold_%08d_pilon_v2.1", seq_len(500))
  ticks <- data.frame(
    chromosome = names500, center = seq(10, by = 20, length.out = 500), width = rep(20, 500)
  )
  p0 <- ggplot2::ggplot(data.frame(x = c(1, 10000), y = c(-1, 1)), ggplot2::aes(x, y)) +
    ggplot2::geom_point()

  p <- popgenVCF:::manhattan_chromosome_row(p0, ticks, c(-1, 1), 11, plot_width_in = 10)
  layer <- p$layers[[length(p$layers)]]

  expect_true(nrow(layer$data) >= 1L)
  expect_true(nrow(layer$data) < 500L)
  # Thinning always keeps the first candidate and proceeds by minimum
  # physical spacing -- never silently reorders or duplicates labels.
  expect_true(all(layer$data$label %in% names500))
  expect_false(anyDuplicated(layer$data$label) > 0)
})

test_that("manhattan_chromosome_row defaults to the original horizontal behavior when plot_width_in is omitted", {
  names60 <- sprintf("JAEVLN01%07d.1", seq_len(60))
  ticks <- data.frame(
    chromosome = names60, center = seq(50, by = 100, length.out = 60), width = rep(100, 60)
  )
  p0 <- ggplot2::ggplot(data.frame(x = c(1, 6000), y = c(-1, 1)), ggplot2::aes(x, y)) +
    ggplot2::geom_point()

  p <- popgenVCF:::manhattan_chromosome_row(p0, ticks, c(-1, 1), 11)
  layer <- p$layers[[length(p$layers)]]

  expect_identical(layer$aes_params$angle, 0)
  expect_identical(nrow(layer$data), 60L)
  expect_null(p$theme$axis.title.x)
})

test_that("manhattan_chromosome_row leaves a real physical gap between every kept label, using the exact real PCA-loadings ticks that first exposed a naive-fit false positive", {
  # This is the real manhattan_layout()$ticks table computed from the
  # reporting user's actual 31_PCA_loadings.tsv (88-contig WGS assembly).
  # An earlier version of the fit check compared each label's width against
  # `ticks$width / total_width * plot_width_in` directly -- treating the
  # figure's full saved width as if all of it maps to plotted data. It does
  # not: the y-axis number/title column and ggplot2's default 5% scale
  # expansion on each side both eat into the panel that actually renders
  # `total_width` worth of data, so real available space per label is
  # meaningfully less than that naive estimate. Concretely, contigs
  # JAEVLN010000016.1/017.1 measured as fitting apart by a ~20% margin under
  # the naive estimate but rendered fully overlapping in the real saved
  # figure -- confirmed directly against that PNG before this test was
  # written. The fix folds in a conservative safety factor; this test pins
  # the exact real ticks that exposed the gap so a future regression here
  # would be caught by a real dataset shape, not just a synthetic one.
  ticks <- structure(list(chromosome = c(
    "JAEVLN010000001.1", "JAEVLN010000002.1", "JAEVLN010000003.1", "JAEVLN010000004.1",
    "JAEVLN010000005.1", "JAEVLN010000006.1", "JAEVLN010000007.1", "JAEVLN010000008.1",
    "JAEVLN010000009.1", "JAEVLN010000010.1", "JAEVLN010000011.1", "JAEVLN010000012.1",
    "JAEVLN010000015.1", "JAEVLN010000016.1", "JAEVLN010000017.1", "JAEVLN010000018.1",
    "JAEVLN010000019.1", "JAEVLN010000020.1", "JAEVLN010000021.1", "JAEVLN010000023.1",
    "JAEVLN010000024.1", "JAEVLN010000025.1", "JAEVLN010000026.1", "JAEVLN010000027.1",
    "JAEVLN010000028.1", "JAEVLN010000029.1", "JAEVLN010000030.1", "JAEVLN010000031.1",
    "JAEVLN010000033.1", "JAEVLN010000034.1", "JAEVLN010000035.1", "JAEVLN010000036.1",
    "JAEVLN010000037.1", "JAEVLN010000039.1", "JAEVLN010000043.1", "JAEVLN010000046.1",
    "JAEVLN010000059.1", "JAEVLN010000065.1"
  ), center = c(
    3768255.5, 10033885.5, 14684435.5, 18379998, 20998054.5, 25643484, 30160053.5,
    32045007, 33967015, 36577372, 38351206, 39844642.5, 41987351.5, 43877362.5,
    44583040.5, 45878503, 46766438, 47472784.5, 48450575, 49318405, 49695490,
    49989904, 50546502, 51288771, 51699556, 52365419, 53223826, 53385888, 53676095,
    53919885.5, 54166435, 54283619.5, 54484134, 55006468, 55006576, 55073055,
    55129032, 55147235
  ), width = c(
    5359323, 4311191, 3888225, 599942, 3982249, 3481042, 2505241, 56082, 695894,
    1732450, 33948, 1560711, 1981195, 34633, 268319, 1007278, 0, 817457, 575472,
    0, 0, 0, 0, 0, 696834, 460012, 13458, 0, 0, 1905, 0, 106969, 288226, 0, 0,
    16502, 0, 0
  )), class = "data.frame", row.names = c(NA, -38L))

  p0 <- ggplot2::ggplot(
    data.frame(x = range(ticks$center), y = c(-0.02, 0.02)), ggplot2::aes(x, y)
  ) + ggplot2::geom_point()
  p <- popgenVCF:::manhattan_chromosome_row(p0, ticks, c(-0.02, 0.02), 11, plot_width_in = 10)
  kept <- p$layers[[length(p$layers)]]$data
  kept <- kept[order(kept$x), ]

  label_pt <- 7 # the fixed vertical-mode font size (see manhattan_chromosome_row())
  required_in <- (label_pt * 1.2) / 72
  usable_width_in <- 10 * 0.7
  gaps_in <- (diff(kept$x) / sum(ticks$width)) * usable_width_in

  expect_true(all(gaps_in >= required_in - 1e-9))
  # The specific real pair that originally exposed the naive-fit false
  # positive (horizontal mode) now comfortably survives together in
  # vertical mode at the smaller, fixed 7pt font -- the general physical-
  # spacing invariant above is what must hold, not this specific pair's
  # fate, which is expected to change as the label font shrinks.
  expect_true(all(c("JAEVLN010000016.1", "JAEVLN010000017.1") %in% kept$label))
})

test_that("manhattan_chromosome_row renders without error against the exact real contig names/lengths that motivated this fix", {
  real_names <- sprintf("JAEVLN01%07d.1", c(1L, 9:88))
  real_lengths <- c(6746087L, sample(5000:3400000, 80))
  ticks <- data.frame(
    chromosome = real_names, center = cumsum(real_lengths) - real_lengths / 2, width = real_lengths
  )
  p0 <- ggplot2::ggplot(data.frame(x = c(1, sum(real_lengths)), y = c(-0.02, 0.02)), ggplot2::aes(x, y)) +
    ggplot2::geom_point()

  p <- popgenVCF:::manhattan_chromosome_row(p0, ticks, c(-0.02, 0.02), 11, plot_width_in = 10)
  built <- ggplot2::ggplot_build(p)

  expect_s3_class(built, "ggplot_built")
  layer <- p$layers[[length(p$layers)]]
  expect_identical(layer$aes_params$angle, 90)
})

test_that("manhattan_chromosome_row does not squeeze a narrow-y-range panel to make room for vertical labels", {
  # Real regression #1: a real PCA-loadings figure had its rotated
  # contig-name labels overlapping the plotted panel itself (running back
  # up through the axis and data points) rather than clearing it, on a
  # panel with a genuinely narrow y-range (this real PC10 facet,
  # contribution values 0.0196-0.0231 from the reporting user's own
  # truncated top-20-loadings table). Fixed at first by growing `pad` (the
  # label anchor's data-space offset) to guarantee clearance -- which
  # created real regression #2: since `pad` is a value in the plotted
  # data's own units, growing it to fit a whole rotated label's length
  # pulled that much of the panel's own y-scale down with it, visibly
  # crushing the actual plotted data into a sliver at the top of the
  # figure (found regenerating the same real figure again). The correct
  # fix keeps `pad` fixed at the same small 14%-of-range value always used,
  # and reserves real clearance in `plot.margin` (a physical-device
  # quantity, entirely outside the data coordinate system) instead.
  ticks <- structure(list(chromosome = c(
    "JAEVLN010000001.1", "JAEVLN010000002.1", "JAEVLN010000003.1", "JAEVLN010000004.1",
    "JAEVLN010000005.1", "JAEVLN010000006.1", "JAEVLN010000007.1", "JAEVLN010000008.1",
    "JAEVLN010000009.1", "JAEVLN010000010.1", "JAEVLN010000011.1", "JAEVLN010000012.1",
    "JAEVLN010000015.1", "JAEVLN010000016.1", "JAEVLN010000017.1", "JAEVLN010000018.1",
    "JAEVLN010000019.1", "JAEVLN010000020.1", "JAEVLN010000021.1", "JAEVLN010000023.1",
    "JAEVLN010000024.1", "JAEVLN010000025.1", "JAEVLN010000026.1", "JAEVLN010000027.1",
    "JAEVLN010000028.1", "JAEVLN010000029.1", "JAEVLN010000030.1", "JAEVLN010000031.1",
    "JAEVLN010000033.1", "JAEVLN010000034.1", "JAEVLN010000035.1", "JAEVLN010000036.1",
    "JAEVLN010000037.1", "JAEVLN010000039.1", "JAEVLN010000043.1", "JAEVLN010000046.1",
    "JAEVLN010000059.1", "JAEVLN010000065.1"
  ), center = c(
    3768255.5, 10033885.5, 14684435.5, 18379998, 20998054.5, 25643484, 30160053.5,
    32045007, 33967015, 36577372, 38351206, 39844642.5, 41987351.5, 43877362.5,
    44583040.5, 45878503, 46766438, 47472784.5, 48450575, 49318405, 49695490,
    49989904, 50546502, 51288771, 51699556, 52365419, 53223826, 53385888, 53676095,
    53919885.5, 54166435, 54283619.5, 54484134, 55006468, 55006576, 55073055,
    55129032, 55147235
  ), width = c(
    5359323, 4311191, 3888225, 599942, 3982249, 3481042, 2505241, 56082, 695894,
    1732450, 33948, 1560711, 1981195, 34633, 268319, 1007278, 0, 817457, 575472,
    0, 0, 0, 0, 0, 696834, 460012, 13458, 0, 0, 1905, 0, 106969, 288226, 0, 0,
    16502, 0, 0
  )), class = "data.frame", row.names = c(NA, -38L))

  y_range <- c(0.0195876, 0.02306113) # the real PC10 facet's own contribution range

  p0 <- ggplot2::ggplot(data.frame(x = range(ticks$center), y = y_range), ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  p <- popgenVCF:::manhattan_chromosome_row(p0, ticks, y_range, 11, plot_width_in = 10)
  label_layer <- p$layers[[length(p$layers)]]

  # No squeeze: pad is exactly the same fixed 14%-of-range value used in
  # horizontal mode, regardless of how long the (now smaller-font) rotated
  # labels are.
  expect_equal(label_layer$data$y[1], y_range[1] - diff(y_range) * 0.14)

  # Real clearance instead comes entirely from plot.margin (device space),
  # sized from the smaller vertical-mode font's real label width.
  label_width_in <- popgenVCF:::manhattan_label_width_in(label_layer$data$label, 7)
  margin_bottom_pt <- as.numeric(p$theme$plot.margin)[3]
  expect_true(margin_bottom_pt >= max(label_width_in) * 72)
})

test_that("manhattan_chromosome_row's vertical-mode margin grows the saved canvas instead of eating a short, single-panel plot's height", {
  # Real regression #3 in the same family as the squeeze test above, found
  # on the real pcadapt figure (a compact single-panel 10x4.5in save, unlike
  # the tall multi-facet PCA/DAPC-loadings figures the margin factor was
  # first tuned against): plot.margin is a fixed point quantity that does
  # not scale with the caller's chosen plot height. On that 4.5in (324pt)
  # canvas, the vertical-mode margin computed to ~164pt -- over half the
  # entire canvas -- squeezing the panel itself down to a sliver and
  # visually crushing all 560k real data points into a single-pixel-tall
  # smear, the exact squeeze regression this function exists to avoid, just
  # triggered by a short canvas instead of an inflated pad. The fix has
  # manhattan_chromosome_row() report how much extra canvas height the
  # label margin actually needs via an attribute, which save_plot() (tested
  # separately below) adds to the caller's requested height so the panel
  # keeps its intended size.
  names60 <- sprintf("JAEVLN01%07d.1", seq_len(60))
  ticks <- data.frame(
    chromosome = names60, center = seq(50, by = 100, length.out = 60), width = rep(100, 60)
  )
  p0 <- ggplot2::ggplot(data.frame(x = c(1, 6000), y = c(0, 300)), ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  p_vertical <- popgenVCF:::manhattan_chromosome_row(p0, ticks, c(0, 300), 11, plot_width_in = 10)

  extra_in <- attr(p_vertical, "manhattan_extra_height_in", exact = TRUE)
  label_width_in <- popgenVCF:::manhattan_label_width_in(names60, 7)
  expect_equal(extra_in, max(label_width_in) * 2)
  expect_gt(extra_in, 0)

  # Horizontal mode (labels already fit) needs no extra canvas height.
  narrow_ticks <- data.frame(chromosome = c("chr1", "chr2"), center = c(50, 150), width = c(100, 100))
  p_horizontal <- popgenVCF:::manhattan_chromosome_row(p0, narrow_ticks, c(0, 300), 11, plot_width_in = 10)
  expect_equal(attr(p_horizontal, "manhattan_extra_height_in", exact = TRUE), 0)
})

test_that("save_plot grows the saved height by manhattan_chromosome_row's reported extra canvas need", {
  names60 <- sprintf("JAEVLN01%07d.1", seq_len(60))
  ticks <- data.frame(
    chromosome = names60, center = seq(50, by = 100, length.out = 60), width = rep(100, 60)
  )
  p0 <- ggplot2::ggplot(data.frame(x = c(1, 6000), y = c(0, 300)), ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  p <- popgenVCF:::manhattan_chromosome_row(p0, ticks, c(0, 300), 11, plot_width_in = 10)
  extra_in <- attr(p, "manhattan_extra_height_in", exact = TRUE)
  expect_gt(extra_in, 0) # sanity: this fixture must actually trigger vertical mode

  captured <- list()
  local_mocked_bindings(
    ggsave = function(filename, plot, width, height, ...) {
      captured$height <<- height
      invisible(NULL)
    },
    .package = "ggplot2"
  )
  dirs <- withr::local_tempdir()
  popgenVCF:::save_plot(p, "stem", list(figures = dirs), formats = "pdf", width = 10, height = 4.5, dpi = 600)

  expect_equal(captured$height, 4.5 + extra_in)
})

test_that("manhattan_chromosome_row's vertical-mode font/margin logic does not change horizontal-mode behavior", {
  ticks <- data.frame(chromosome = c("chr1", "chr2"), center = c(50, 150), width = c(100, 100))
  p0 <- ggplot2::ggplot(data.frame(x = c(1, 190), y = c(-1, 1)), ggplot2::aes(x, y)) +
    ggplot2::geom_point()

  p <- popgenVCF:::manhattan_chromosome_row(p0, ticks, c(-1, 1), 11, plot_width_in = 10)
  layer <- p$layers[[length(p$layers)]]

  expect_identical(layer$aes_params$angle, 0)
  expect_equal(layer$aes_params$size, 11 * 0.32) # base_size-derived, not the fixed vertical-mode 7pt
  expect_equal(layer$data$y[1], -1 - diff(c(-1, 1)) * 0.14)
})

test_that("manhattan_chromosome_row pushes a plot.caption below vertical labels instead of leaving it to collide", {
  # Real regression: pcadapt_scan.R's genomic-inflation-factor caption
  # rendered on top of the rotated contig-name row instead of below it --
  # ggplot2's plot.caption sits in its own layout row close to the panel,
  # unrelated to how far this function's own data-space annotation extends
  # into the margin below.
  names60 <- sprintf("JAEVLN01%07d.1", seq_len(60))
  ticks <- data.frame(
    chromosome = names60, center = seq(50, by = 100, length.out = 60), width = rep(100, 60)
  )
  p_with_caption <- ggplot2::ggplot(data.frame(x = c(1, 6000), y = c(-1, 1)), ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    ggplot2::labs(caption = "Genomic inflation factor (gif) = 1.000")
  p_without_caption <- ggplot2::ggplot(data.frame(x = c(1, 6000), y = c(-1, 1)), ggplot2::aes(x, y)) +
    ggplot2::geom_point()

  p1 <- popgenVCF:::manhattan_chromosome_row(p_with_caption, ticks, c(-1, 1), 11, plot_width_in = 10)
  p2 <- popgenVCF:::manhattan_chromosome_row(p_without_caption, ticks, c(-1, 1), 11, plot_width_in = 10)

  expect_true(as.numeric(p1$theme$plot.caption$margin)[1] > 0)
  # A plot with no caption at all must not gain a caption theme override
  # it doesn't need.
  expect_null(p2$theme$plot.caption)
})
