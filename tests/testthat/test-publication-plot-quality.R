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
