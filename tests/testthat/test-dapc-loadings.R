dapc_loading_fixture <- function() {
  set.seed(101)
  n <- 18L
  groups <- rep(c("A", "B", "C"), each = 6L)
  genotype <- matrix(stats::rbinom(n * 90L, 2L, 0.15), nrow = n)
  genotype[groups == "A", 1:30] <- 2L
  genotype[groups == "B", 31:60] <- 2L
  genotype[groups == "C", 61:90] <- 2L
  sample_ids <- paste0("sample_", seq_len(n))
  metadata <- popgenVCF:::normalize_sample_aliases(data.table::data.table(
    sample = sample_ids, population = groups
  ))
  list(genotype = genotype, sample_ids = sample_ids, metadata = metadata)
}

test_that("manhattan_layout computes cumulative chromosome offsets and tick centers", {
  chromosome <- c("1", "1", "2", "2", "2")
  position <- c(100, 500, 50, 300, 900)
  layout <- popgenVCF:::manhattan_layout(chromosome, position)

  offset_chr2 <- max(position[chromosome == "1"]) + 1
  expect_equal(layout$x, c(100, 500, 50 + offset_chr2, 300 + offset_chr2, 900 + offset_chr2))
  expect_identical(layout$ticks$chromosome, c("1", "2"))
  expect_equal(layout$ticks$center[1], mean(range(c(100, 500))))
  expect_equal(layout$ticks$center[2], mean(range(c(50, 300, 900))) + offset_chr2)
})

test_that("manhattan_layout orders chromosomes naturally, matching R/chromosome.R", {
  # A plain sort() on these strings would give "1", "10", "2" -- wrong for
  # real multi-chromosome data. natural_sort_key() treats the embedded
  # number numerically instead, matching R/chromosome.R's chromosome_summary
  # ordering (run_chromosome_analyses(), which uses the same helper).
  chromosome <- c("2", "10", "1")
  position <- c(10, 10, 10)
  layout <- popgenVCF:::manhattan_layout(chromosome, position)
  expect_identical(layout$ticks$chromosome, c("1", "2", "10"))
})

test_that("manhattan_layout orders non-numeric chromosome names (X, Y) after numeric ones", {
  chromosome <- c("Y", "2", "X", "10")
  position <- c(10, 10, 10, 10)
  layout <- popgenVCF:::manhattan_layout(chromosome, position)
  expect_identical(layout$ticks$chromosome, c("2", "10", "X", "Y"))
})

test_that("dapc_loading_table joins var.contr to chromosome/position and sorts by contribution", {
  contr <- matrix(
    c(0.1, 0.5, 0.4, 0.05, 0.15, 0.8),
    nrow = 3L, ncol = 2L,
    dimnames = list(c("snp_a", "snp_b", "snp_c"), c("LD1", "LD2"))
  )
  model <- list(var.contr = contr)
  snp_ids <- c("snp_a", "snp_b", "snp_c")
  chromosome <- c("1", "1", "2")
  position <- c(100, 200, 300)

  out <- popgenVCF:::dapc_loading_table(model, snp_ids, chromosome, position)

  expect_setequal(names(out), c("axis", "snp_id", "chromosome", "position", "contribution"))
  expect_equal(nrow(out), 6L)

  ld1 <- out[out$axis == "LD1", ]
  expect_identical(ld1$snp_id, c("snp_b", "snp_c", "snp_a"))
  expect_identical(ld1$chromosome[ld1$snp_id == "snp_b"], "1")
  expect_identical(ld1$position[ld1$snp_id == "snp_c"], 300)

  ld2 <- out[out$axis == "LD2", ]
  expect_identical(ld2$snp_id, c("snp_c", "snp_b", "snp_a"))
})

test_that("dapc_loading_table orders axis blocks naturally (LD2 before LD10, not lexicographically)", {
  contr <- matrix(
    c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6),
    nrow = 3L, ncol = 2L,
    dimnames = list(c("snp_a", "snp_b", "snp_c"), c("LD10", "LD2"))
  )
  model <- list(var.contr = contr)
  out <- popgenVCF:::dapc_loading_table(
    model, c("snp_a", "snp_b", "snp_c"), c("1", "1", "1"), c(100, 200, 300)
  )
  expect_identical(rle(out$axis)$values, c("LD2", "LD10"))
})

test_that("plot_dapc_loading_manhattan and plot_dapc_loading_ranked facet axes naturally (K10 loadings)", {
  loadings <- data.table::data.table(
    axis = rep(c("LD10", "LD2", "LD9"), each = 2L),
    snp_id = rep(c("snp_a", "snp_b"), 3L),
    chromosome = "1",
    position = c(100, 200, 100, 200, 100, 200),
    contribution = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6)
  )
  root <- withr::local_tempdir()
  dirs <- list(figures = root)
  dir.create(dirs$figures, showWarnings = FALSE)
  profile <- popgenVCF:::figure_style_profile("accessibility-first")
  cfg <- list(output = list(figure_formats = "png", dpi = 100))

  p1 <- popgenVCF:::plot_dapc_loading_manhattan(loadings, "10", cfg, dirs, profile)
  expect_identical(levels(p1$data$axis), c("LD2", "LD9", "LD10"))

  p2 <- popgenVCF:::plot_dapc_loading_ranked(loadings, "10", cfg, dirs, profile)
  expect_identical(levels(p2$data$axis), c("LD2", "LD9", "LD10"))
})

test_that("run_dapc_analysis produces per-K loadings only when snp_ids/chromosome/position are supplied", {
  fixture <- dapc_loading_fixture()
  n_snps <- ncol(fixture$genotype)
  snp_ids <- paste0("snp_", seq_len(n_snps))
  chromosome <- rep(c("1", "2", "3"), length.out = n_snps)
  position <- seq_len(n_snps) * 100L

  with_loadings <- popgenVCF:::run_dapc_analysis(
    fixture$genotype, fixture$sample_ids, fixture$metadata,
    k_values = 2L, seed = 42L, cross_validate = FALSE,
    snp_ids = snp_ids, chromosome = chromosome, position = position
  )
  loadings <- with_loadings$models[["2"]]$loadings
  expect_false(is.null(loadings))
  n_da <- length(unique(loadings$axis))
  expect_equal(nrow(loadings), n_snps * n_da)
  expect_true(all(c("axis", "snp_id", "chromosome", "position", "contribution") %in% names(loadings)))
  expect_true(all(loadings$snp_id %in% snp_ids))
  expect_true(all(is.finite(loadings$contribution)))

  without_loadings <- popgenVCF:::run_dapc_analysis(
    fixture$genotype, fixture$sample_ids, fixture$metadata,
    k_values = 2L, seed = 42L, cross_validate = FALSE
  )
  expect_null(without_loadings$models[["2"]]$loadings)
})
