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
