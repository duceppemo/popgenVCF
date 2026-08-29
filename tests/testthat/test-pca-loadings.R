test_that("pca_loading_table joins snploading to chromosome/position, ranks by magnitude", {
  loading <- list(
    snp.id = c(10L, 30L),
    snploading = matrix(
      c(0.5, -0.8, 0.1, 0.05),
      nrow = 2L, ncol = 2L
    )
  )
  ids <- list(
    snp = c(10L, 20L, 30L),
    chromosome = c("1", "1", "2"),
    position = c(100L, 200L, 50L)
  )

  out <- popgenVCF:::pca_loading_table(loading, ids)

  expect_setequal(names(out), c("axis", "snp_id", "chromosome", "position", "contribution", "magnitude"))
  expect_equal(nrow(out), 4L)
  expect_identical(sort(unique(out$axis)), c("PC1", "PC2"))

  pc1 <- out[out$axis == "PC1", ]
  expect_identical(pc1$snp_id, c("10", "30"))
  expect_equal(pc1$magnitude, abs(pc1$contribution))
  expect_identical(pc1$chromosome[pc1$snp_id == "30"], "2")
  expect_identical(pc1$position[pc1$snp_id == "10"], 100L)
})

test_that("pca_loading_table drops loci absent from snploading (e.g. removed monomorphic SNPs)", {
  loading <- list(
    snp.id = c(10L),
    snploading = matrix(0.3, nrow = 1L, ncol = 1L)
  )
  ids <- list(
    snp = c(10L, 20L),
    chromosome = c("1", "2"),
    position = c(100L, 200L)
  )
  out <- popgenVCF:::pca_loading_table(loading, ids)
  expect_equal(nrow(out), 1L)
  expect_identical(out$snp_id, "10")
})

test_that("run_pca produces loadings only when ids is supplied, and renumbers retained components", {
  skip_if(Sys.which("bcftools") == "", "bcftools is not available")
  paths <- popgenVCF:::validation_fixture_paths()
  gds <- popgenVCF:::prepare_gds(paths$vcf, tempfile(fileext = ".gds"))
  on.exit(SNPRelate::snpgdsClose(gds))
  ids <- popgenVCF:::get_gds_ids(gds)
  metadata <- popgenVCF:::metadata_from_samples(ids$sample)

  with_loadings <- popgenVCF:::run_pca(gds, ids$sample, ids$snp, metadata, 3L, 1L, ids = ids)
  expect_false(is.null(with_loadings$loadings))
  expect_setequal(
    unique(with_loadings$loadings$axis),
    with_loadings$variance$PC
  )
  expect_true(all(with_loadings$loadings$snp_id %in% as.character(ids$snp)))
  expect_true(all(is.finite(with_loadings$loadings$contribution)))

  without_loadings <- popgenVCF:::run_pca(gds, ids$sample, ids$snp, metadata, 3L, 1L)
  expect_null(without_loadings$loadings)
})

test_that("PCA loading plots are drawn only when a loadings table is present", {
  plots <- list()
  local_mocked_bindings(
    save_plot = function(p, stem, ...) {
      plots[[stem]] <<- p
      invisible(TRUE)
    },
    .package = "popgenVCF"
  )
  dirs <- list(figures = tempdir())
  cfg <- list(output = list(figure_formats = "pdf", dpi = 150L, label_samples = "none"))

  pca <- list(
    scores = data.table::data.table(sample = c("s1", "s2", "s3"), PC1 = c(-1, 0, 1), PC2 = c(0.5, -0.5, 0)),
    variance = data.table::data.table(PC = c("PC1", "PC2"), proportion = c(0.6, 0.4), percent = c(60, 40)),
    loadings = data.table::data.table(
      axis = rep(c("PC1", "PC2"), each = 3L),
      snp_id = rep(c("1", "2", "3"), 2L),
      chromosome = rep(c("1", "1", "2"), 2L),
      position = rep(c(100L, 500L, 50L), 2L),
      contribution = c(0.6, -0.2, 0.1, -0.5, 0.3, 0.05),
      magnitude = c(0.6, 0.2, 0.1, 0.5, 0.3, 0.05)
    )
  )
  popgenVCF:::plot_pca(pca, cfg, dirs)

  expect_true("07_PCA_PC1_PC2" %in% names(plots))
  expect_true("17_PCA_loadings_manhattan" %in% names(plots))
  expect_true("18_PCA_loadings_ranked" %in% names(plots))

  manhattan <- plots[["17_PCA_loadings_manhattan"]]
  ranked <- plots[["18_PCA_loadings_ranked"]]
  expect_s3_class(manhattan$facet, "FacetWrap")
  expect_s3_class(ranked$facet, "FacetWrap")
  expect_identical(manhattan$labels$title, "Principal component analysis SNP loadings")
  expect_identical(ranked$labels$title, "Principal component analysis SNP loadings, ranked")
})

test_that("PCA loading plots facet axes naturally (PC2 before PC10, not lexicographically)", {
  plots <- list()
  local_mocked_bindings(
    save_plot = function(p, stem, ...) {
      plots[[stem]] <<- p
      invisible(TRUE)
    },
    .package = "popgenVCF"
  )
  dirs <- list(figures = tempdir())
  cfg <- list(output = list(figure_formats = "pdf", dpi = 150L, label_samples = "none"))

  axes <- c("PC1", "PC10", "PC2")
  pca <- list(
    scores = data.table::data.table(sample = c("s1", "s2", "s3"), PC1 = c(-1, 0, 1), PC2 = c(0.5, -0.5, 0)),
    variance = data.table::data.table(PC = c("PC1", "PC2"), proportion = c(0.6, 0.4), percent = c(60, 40)),
    loadings = data.table::data.table(
      axis = rep(axes, each = 2L),
      snp_id = rep(c("1", "2"), 3L),
      chromosome = "1",
      position = c(100L, 500L, 100L, 500L, 100L, 500L),
      contribution = c(0.6, -0.2, 0.1, -0.5, 0.3, 0.05),
      magnitude = c(0.6, 0.2, 0.1, 0.5, 0.3, 0.05)
    )
  )
  popgenVCF:::plot_pca(pca, cfg, dirs)

  manhattan <- plots[["17_PCA_loadings_manhattan"]]
  ranked <- plots[["18_PCA_loadings_ranked"]]
  expect_identical(levels(manhattan$data$axis), c("PC1", "PC2", "PC10"))
  expect_identical(levels(ranked$data$axis), c("PC1", "PC2", "PC10"))

  # Real regression: manhattan_chromosome_row()'s label layer previously
  # carried its facet column as a plain character rather than a factor
  # sharing the main data's levels, which silently reset facet_wrap()'s
  # rendered panel order to alphabetical (PC1, PC10, PC2) once that layer
  # was added -- the levels()-only checks above would not have caught this,
  # since they inspect the *source* data, not the actually rendered panels.
  built <- ggplot2::ggplot_build(manhattan)
  panel_axis_order <- as.character(built$layout$layout$axis[order(built$layout$layout$PANEL)])
  expect_identical(panel_axis_order, c("PC1", "PC2", "PC10"))
})

test_that("PCA loading plots position the chromosome-label row below the real rendered scale even when geom_hline(yintercept = 0) silently widens it", {
  # Real regression: plot_pca_loading_manhattan() computed the y_range it
  # handed to manhattan_chromosome_row() from range(contribution) alone for
  # the last facet -- but that facet's plot already carries
  # geom_hline(yintercept = 0), and ggplot2 trains a panel's rendered
  # y-scale on every layer's data, including annotation layers, not just
  # the primary geom's. On a real low-variance PC whose loadings cluster
  # tightly far from zero (contribution 0.0196-0.0231, this test's PC2
  # deliberately shaped the same way), the hline silently pulled the
  # rendered scale down to include 0 while the externally-computed pad/
  # label position still assumed the narrow raw-data range -- so the label,
  # anchored just below what it *thought* was the panel's bottom, actually
  # landed inside the now much taller real panel instead of below it,
  # clipped by the panel's own boundary rather than bleeding into the
  # margin as intended. The fix (range(c(contribution, 0), ...)) keeps the
  # externally-computed range in sync with what the hline will do to the
  # scale, so the label lands below the true rendered range on every axis,
  # not just the ones whose raw data already happens to span zero.
  plots <- list()
  local_mocked_bindings(
    save_plot = function(p, stem, ...) {
      plots[[stem]] <<- p
      invisible(TRUE)
    },
    .package = "popgenVCF"
  )
  dirs <- list(figures = tempdir())
  cfg <- list(output = list(figure_formats = "pdf", dpi = 150L, label_samples = "none"))

  axes <- c("PC1", "PC2")
  pca <- list(
    scores = data.table::data.table(sample = c("s1", "s2", "s3"), PC1 = c(-1, 0, 1), PC2 = c(0.5, -0.5, 0)),
    variance = data.table::data.table(PC = axes, proportion = c(0.6, 0.4), percent = c(60, 40)),
    loadings = data.table::data.table(
      axis = rep(axes, each = 3L),
      snp_id = rep(c("1", "2", "3"), 2L),
      chromosome = "1",
      position = c(100L, 500L, 900L, 100L, 500L, 900L),
      contribution = c(0.6, -0.2, 0.1, 0.0231, 0.0196, 0.0221), # PC2 (last axis): tight, far from 0
      magnitude = c(0.6, 0.2, 0.1, 0.0231, 0.0196, 0.0221)
    )
  )
  popgenVCF:::plot_pca(pca, cfg, dirs)

  manhattan <- plots[["17_PCA_loadings_manhattan"]]
  built <- ggplot2::ggplot_build(manhattan)
  last_panel <- as.integer(built$layout$layout$PANEL[built$layout$layout$axis == "PC2"])
  true_range <- built$layout$panel_scales_y[[last_panel]]$range$range
  # The hline really did widen this panel's own rendered scale to include 0,
  # confirming the test fixture reproduces the mechanism this regression
  # depends on.
  expect_true(true_range[1] <= 0)

  # Pins the actual fix (the call site's own y_range computation), not just
  # its downstream effect: with the bug (y_range from range(contribution)
  # alone, e.g. c(0.0196, 0.0231)), pad would be a tiny fraction of that
  # narrow 0.0035 span and the label would land at y ~= 0.019 -- comfortably
  # *inside* the true rendered range asserted above once the hline pulls it
  # down to include 0, instead of below it. Asserting the exact expected
  # value (computed the same way the fix computes it) fails loudly if
  # range(c(contribution, 0), ...) regresses back to range(contribution).
  expected_y_range <- range(c(0.0231, 0.0196, 0.0221, 0))
  expected_label_y <- expected_y_range[1] - diff(expected_y_range) * 0.14

  label_layer <- manhattan$layers[[length(manhattan$layers)]]
  label_y <- label_layer$data$y[label_layer$data$axis == "PC2"][1]
  expect_equal(label_y, expected_label_y)
  expect_true(label_y <= true_range[1])
})

test_that("PCA loading plots are absent when no loadings table is supplied", {
  plots <- list()
  local_mocked_bindings(
    save_plot = function(p, stem, ...) {
      plots[[stem]] <<- p
      invisible(TRUE)
    },
    .package = "popgenVCF"
  )
  dirs <- list(figures = tempdir())
  cfg <- list(output = list(figure_formats = "pdf", dpi = 150L, label_samples = "none"))
  pca <- list(
    scores = data.table::data.table(sample = c("s1", "s2", "s3"), PC1 = c(-1, 0, 1), PC2 = c(0.5, -0.5, 0)),
    variance = data.table::data.table(PC = c("PC1", "PC2"), proportion = c(0.6, 0.4), percent = c(60, 40))
  )
  popgenVCF:::plot_pca(pca, cfg, dirs)

  expect_false(any(grepl("PCA_loadings", names(plots))))
})
