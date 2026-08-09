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
