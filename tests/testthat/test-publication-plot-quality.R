test_that("diversity figures preserve deterministic points and bootstrap intervals", {
  plots <- list()
  local_mocked_bindings(
    save_plot = function(p, stem, ...) {
      plots[[stem]] <<- p
      invisible(TRUE)
    },
    .package = "popgenVCF"
  )
  cfg <- default_config()
  cfg$compute$seed <- 917L
  div <- list(
    sample = data.table::data.table(
      population = rep(c("A", "B"), each = 3L),
      observed_heterozygosity = c(.10, .12, .14, .20, .22, .24)
    ),
    population = data.table::data.table(
      population = c("A", "B"),
      observed_heterozygosity = c(.12, .22),
      expected_heterozygosity = c(.15, .25)
    )
  )
  intervals <- data.table::data.table(
    population = rep(c("A", "B"), each = 2L),
    metric = rep(c("Ho", "He"), 2L),
    estimate = c(.12, .15, .22, .25),
    lower = c(.10, .13, .20, .23),
    upper = c(.14, .17, .24, .27)
  )

  plot_diversity(div, intervals, cfg, list(figures = tempdir()))

  sample_plot <- plots[["05_sample_heterozygosity"]]
  diversity_plot <- plots[["06_population_diversity"]]
  expect_identical(sample_plot$layers[[2L]]$position$seed, 917L)
  expect_match(diversity_plot$labels$subtitle, "95%")
  built <- ggplot2::ggplot_build(diversity_plot)
  expect_true(any(is.finite(built$data[[1L]]$ymin)))
  expect_true(any(is.finite(built$data[[1L]]$ymax)))
})

test_that("plot_diversity wraps the Hardy-Weinberg subtitle instead of letting it run off the plot", {
  # Reported directly against a real production HTML report: the
  # Hardy-Weinberg equilibrium p-value histogram's subtitle ran off the plot
  # uncorrected -- it was never passed through wrap_plot_text(), unlike
  # plot.subtitle elsewhere in this package.
  plots <- list()
  local_mocked_bindings(
    save_plot = function(p, stem, ...) {
      plots[[stem]] <<- p
      invisible(TRUE)
    },
    .package = "popgenVCF"
  )
  cfg <- default_config()
  div <- list(
    sample = data.table::data.table(
      population = rep(c("A", "B"), each = 3L),
      observed_heterozygosity = c(.10, .12, .14, .20, .22, .24)
    ),
    population = data.table::data.table(
      population = c("A", "B"),
      observed_heterozygosity = c(.12, .22),
      expected_heterozygosity = c(.15, .25),
      private_allele_loci = c(0L, 0L)
    ),
    locus = data.table::data.table(
      population = rep(c("A", "B"), each = 3L),
      hwe_pvalue = c(.01, .2, .5, .3, .7, .9)
    )
  )
  plot_diversity(div, data.table::data.table(), cfg, list(figures = tempdir()))
  hwe_plot <- plots[["19_HWE_pvalues"]]
  expect_false(is.null(hwe_plot))
  expect_match(hwe_plot$labels$subtitle, "\n", fixed = TRUE)
  expect_true(all(nchar(strsplit(hwe_plot$labels$subtitle, "\n", fixed = TRUE)[[1L]]) <= 90))
})

test_that("publication theme defines print-safe hierarchy", {
  theme <- popgenVCF:::theme_publication(12)
  expect_identical(theme$text$family, "sans")
  expect_identical(theme$plot.title$face, "bold")
  expect_equal(theme$plot.title$size, 14.5)
  expect_identical(theme$axis.text$colour, "#1A1A1A")
  expect_true(inherits(
    theme$plot.margin, c("margin", "ggplot2::margin")
  ))
})
