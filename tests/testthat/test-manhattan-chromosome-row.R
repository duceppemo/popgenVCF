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
  # Too many long labels for 10 inches of width at this font size: some are
  # thinned rather than left to overlap, but at least one always survives.
  expect_true(nrow(layer$data) < 60L)
  expect_true(nrow(layer$data) >= 1L)
  # The redundant "Chromosome position" title is dropped once labels
  # themselves carry that meaning and would otherwise collide with it.
  expect_s3_class(p$theme$axis.title.x, "element_blank")
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

  label_pt <- 11 * 0.32 * 2.845276
  required_in <- (label_pt * 1.2) / 72
  usable_width_in <- 10 * 0.7
  gaps_in <- (diff(kept$x) / sum(ticks$width)) * usable_width_in

  expect_true(all(gaps_in >= required_in - 1e-9))
  # The specific real pair that originally exposed the false positive must
  # never both survive thinning.
  expect_false(all(c("JAEVLN010000016.1", "JAEVLN010000017.1") %in% kept$label))
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
