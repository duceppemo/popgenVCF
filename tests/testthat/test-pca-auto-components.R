test_that("pca_component_count(\"auto\", ...) requests the generous 100-component cap, not a fixed small default", {
  # n_samples - 1 must itself exceed 100 for the cap to bind -- 20 samples
  # alone would cap available at 19, testing "capped by the data" instead.
  many_sample_ids <- paste0("s", 1:200)
  snp_ids <- 1:1000
  expect_identical(popgenVCF:::pca_component_count("auto", many_sample_ids, snp_ids), 100L)

  # Capped by what the data can actually support when fewer are available.
  sample_ids <- paste0("s", 1:20)
  small_snp_ids <- 1:5
  expect_identical(
    popgenVCF:::pca_component_count("auto", sample_ids, small_snp_ids), 5L
  )
})

test_that("pca_component_count still rejects a non-\"auto\", non-numeric n_pcs and values below 2", {
  sample_ids <- paste0("s", 1:20)
  snp_ids <- 1:300
  expect_error(
    suppressWarnings(popgenVCF:::pca_component_count("bogus", sample_ids, snp_ids)),
    "auto"
  )
  expect_error(
    popgenVCF:::pca_component_count(1L, sample_ids, snp_ids),
    "at least two"
  )
})

test_that("pca_significant_component_count runs LEA's real Tracy-Widom routine and returns a valid component count", {
  skip_if_not(requireNamespace("LEA", quietly = TRUE), "LEA is not installed")

  # A steep, clearly-structured spectrum: two large eigenvalues (real
  # signal) followed by a long flat tail of near-equal small ones (noise) --
  # the sequential test should stop well before the tail, not include all
  # of it, and never return fewer than 2.
  eig <- c(2000, 1500, rep(10, 30))
  n <- popgenVCF:::pca_significant_component_count(eig)

  expect_true(is.numeric(n))
  expect_gte(n, 2L)
  expect_lt(n, length(eig))
})

test_that("pca_significant_component_count errors clearly when LEA is not available", {
  local_mocked_bindings(requireNamespace = function(...) FALSE, .package = "base")
  expect_error(
    popgenVCF:::pca_significant_component_count(c(100, 50, 10, 5)),
    "LEA"
  )
})

test_that("run_pca(n_pcs = \"auto\") trims the reported components to the Tracy-Widom significant count", {
  skip_if_not(requireNamespace("LEA", quietly = TRUE), "LEA is not installed")
  set.seed(3L)
  n_samples <- 20L; n_snps <- 300L
  sample_id <- paste0("s", seq_len(n_samples))
  snp_id <- seq_len(n_snps)
  genmat <- matrix(
    sample(0:2, n_samples * n_snps, replace = TRUE, prob = c(0.25, 0.5, 0.25)),
    nrow = n_samples, ncol = n_snps
  )
  gds_path <- tempfile(fileext = ".gds")
  SNPRelate::snpgdsCreateGeno(
    gds_path, genmat = genmat, sample.id = sample_id, snp.id = snp_id,
    snp.chromosome = rep(1L, n_snps), snp.position = seq_len(n_snps),
    snp.allele = rep("A/G", n_snps), snpfirstdim = FALSE
  )
  gds <- SNPRelate::snpgdsOpen(gds_path)
  on.exit(SNPRelate::snpgdsClose(gds), add = TRUE)
  metadata <- popgenVCF:::normalize_sample_aliases(data.table::data.table(
    sample = sample_id, population = rep("PopA", n_samples)
  ))

  full <- popgenVCF:::run_pca(gds, sample_id, snp_id, metadata, 19L, 1L)
  expected_n <- popgenVCF:::pca_significant_component_count(full$eigenvalues[full$eigenvalues > 0])

  auto <- popgenVCF:::run_pca(gds, sample_id, snp_id, metadata, "auto", 1L)

  expect_identical(nrow(auto$variance), expected_n)
  expect_identical(ncol(auto$scores) - (2L + ("population" %in% names(auto$scores))), expected_n)
  expect_lte(expected_n, 19L)
})

test_that("pca_tracy_widom_table returns the full LEA statistics table, and run_pca(n_pcs = \"auto\") carries it through with a matching significant count", {
  skip_if_not(requireNamespace("LEA", quietly = TRUE), "LEA is not installed")

  eig <- c(2000, 1500, rep(10, 30))
  tw <- popgenVCF:::pca_tracy_widom_table(eig)
  expect_s3_class(tw, "data.table")
  expect_identical(nrow(tw), length(eig))
  expect_true(all(c("N", "eigenvalues", "twstats", "pvalues", "effectn", "percentage") %in% names(tw)))
  expect_identical(tw$N, seq_along(eig))
  expect_equal(sum(tw$percentage), 1, tolerance = 1e-3)

  significant <- popgenVCF:::pca_tracy_widom_significant_count(tw)
  expect_identical(significant, popgenVCF:::pca_significant_component_count(eig))

  set.seed(5L)
  n_samples <- 20L; n_snps <- 300L
  sample_id <- paste0("s", seq_len(n_samples))
  snp_id <- seq_len(n_snps)
  genmat <- matrix(
    sample(0:2, n_samples * n_snps, replace = TRUE, prob = c(0.25, 0.5, 0.25)),
    nrow = n_samples, ncol = n_snps
  )
  gds_path <- tempfile(fileext = ".gds")
  SNPRelate::snpgdsCreateGeno(
    gds_path, genmat = genmat, sample.id = sample_id, snp.id = snp_id,
    snp.chromosome = rep(1L, n_snps), snp.position = seq_len(n_snps),
    snp.allele = rep("A/G", n_snps), snpfirstdim = FALSE
  )
  gds <- SNPRelate::snpgdsOpen(gds_path)
  on.exit(SNPRelate::snpgdsClose(gds), add = TRUE)
  metadata <- popgenVCF:::normalize_sample_aliases(data.table::data.table(
    sample = sample_id, population = rep("PopA", n_samples)
  ))

  auto <- popgenVCF:::run_pca(gds, sample_id, snp_id, metadata, "auto", 1L)
  expect_s3_class(auto$tracy_widom, "data.table")
  expect_identical(auto$tracy_widom_significant, nrow(auto$variance))
  expect_identical(auto$retained_components, auto$tracy_widom_significant)
  expect_identical(auto$tracy_widom_alpha, 0.05)

  # Default (always_tracy_widom = FALSE): a fixed n_pcs neither widens the
  # eigendecomposition nor runs the comparison test -- existing callers
  # (e.g. the per-chromosome PCA in run_chromosome_analyses()) keep their
  # original, narrower, cheaper behaviour unchanged.
  fixed <- popgenVCF:::run_pca(gds, sample_id, snp_id, metadata, 10L, 1L)
  expect_null(fixed$tracy_widom)
  expect_null(fixed$tracy_widom_significant)
  expect_identical(fixed$retained_components, 10L)

  # always_tracy_widom = TRUE: a fixed n_pcs still retains exactly what was
  # requested, but the comparison table/count are now populated too, from a
  # widened eigendecomposition (up to the same generous 100-component cap
  # "auto" uses) -- letting the user see how their fixed choice compares.
  fixed_tw <- popgenVCF:::run_pca(
    gds, sample_id, snp_id, metadata, 10L, 1L, always_tracy_widom = TRUE
  )
  expect_s3_class(fixed_tw$tracy_widom, "data.table")
  expect_gt(nrow(fixed_tw$tracy_widom), 10L)
  expect_identical(fixed_tw$retained_components, 10L)
  expect_identical(nrow(fixed_tw$variance), 10L)
})

test_that("always_tracy_widom = TRUE degrades gracefully (no error, no figure) instead of failing the whole PCA when LEA is unavailable", {
  set.seed(6L)
  n_samples <- 20L; n_snps <- 300L
  sample_id <- paste0("s", seq_len(n_samples))
  snp_id <- seq_len(n_snps)
  genmat <- matrix(
    sample(0:2, n_samples * n_snps, replace = TRUE, prob = c(0.25, 0.5, 0.25)),
    nrow = n_samples, ncol = n_snps
  )
  gds_path <- tempfile(fileext = ".gds")
  SNPRelate::snpgdsCreateGeno(
    gds_path, genmat = genmat, sample.id = sample_id, snp.id = snp_id,
    snp.chromosome = rep(1L, n_snps), snp.position = seq_len(n_snps),
    snp.allele = rep("A/G", n_snps), snpfirstdim = FALSE
  )
  gds <- SNPRelate::snpgdsOpen(gds_path)
  on.exit(SNPRelate::snpgdsClose(gds), add = TRUE)
  metadata <- popgenVCF:::normalize_sample_aliases(data.table::data.table(
    sample = sample_id, population = rep("PopA", n_samples)
  ))

  local_mocked_bindings(requireNamespace = function(...) FALSE, .package = "base")
  result <- popgenVCF:::run_pca(
    gds, sample_id, snp_id, metadata, 10L, 1L, always_tracy_widom = TRUE
  )
  expect_null(result$tracy_widom)
  expect_null(result$tracy_widom_significant)
  expect_identical(result$retained_components, 10L)
  expect_identical(nrow(result$variance), 10L)
})

test_that("plot_pca_tracy_widom draws percent-of-variance vs. component index, highlighting the Tracy-Widom-significant components", {
  tw <- data.table::data.table(
    N = 1:6, eigenvalues = c(50, 40, 30, 9, 9, 9),
    twstats = c(4.3, 7.2, 13.8, NaN, NaN, NaN),
    pvalues = c(1e-4, 1e-7, 1e-8, 1, 1, 1),
    effectn = rep(100, 6), percentage = c(0.30, 0.24, 0.18, 0.10, 0.10, 0.08)
  )
  profile <- popgenVCF:::figure_style_profile("accessibility-first")
  cfg <- list(output = list(figure_formats = "pdf", dpi = 150L))

  # retained == significant (the "auto" case): one reference line, no
  # "dotted line" callout needed since there is nothing else to compare to.
  dirs_auto <- list(figures = withr::local_tempdir())
  p_auto <- popgenVCF:::plot_pca_tracy_widom(tw, 3L, 0.05, 3L, cfg, dirs_auto, profile)
  expect_s3_class(p_auto, "ggplot")
  expect_identical(p_auto$data$index, 1:6)
  expect_identical(p_auto$data$retained_significant, c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_identical(p_auto$labels$x, "Principal component")
  expect_identical(p_auto$labels$y, "Percent of total variance explained (%)")
  expect_match(p_auto$labels$subtitle, "3 of 6 computed component")
  expect_match(p_auto$labels$subtitle, "matching the significant count")
  expect_length(p_auto$layers, 3L) # line + significance vline + points, no separate retained vline
  expect_true(file.exists(file.path(dirs_auto$figures, "06b_PCA_Tracy_Widom_test.pdf")))

  # retained != significant (a fixed n_pcs the user chose independently):
  # a second, distinctly-styled vline marks the user's own choice.
  dirs_fixed <- list(figures = withr::local_tempdir())
  p_fixed <- popgenVCF:::plot_pca_tracy_widom(tw, 3L, 0.05, 5L, cfg, dirs_fixed, profile)
  expect_match(p_fixed$labels$subtitle, "5 retained \\(dotted line\\)")
  expect_length(p_fixed$layers, 4L) # + the retained-count vline
  expect_true(file.exists(file.path(dirs_fixed$figures, "06b_PCA_Tracy_Widom_test.pdf")))
})

test_that("plot_pca_tracy_widom is a no-op when there is no Tracy-Widom result to show", {
  cfg <- list(output = list(figure_formats = "pdf", dpi = 150L))
  dirs <- list(figures = withr::local_tempdir())
  expect_null(popgenVCF:::plot_pca_tracy_widom(NULL, NULL, 0.05, NULL, cfg, dirs, popgenVCF:::figure_style_profile("accessibility-first")))
  expect_identical(list.files(dirs$figures), character())
})

test_that("plot_pca() only writes the Tracy-Widom figure when a Tracy-Widom result is actually present on the pca object", {
  skip_if_not(requireNamespace("LEA", quietly = TRUE), "LEA is not installed")
  set.seed(7L)
  n_samples <- 20L; n_snps <- 300L
  sample_id <- paste0("s", seq_len(n_samples))
  snp_id <- seq_len(n_snps)
  genmat <- matrix(
    sample(0:2, n_samples * n_snps, replace = TRUE, prob = c(0.25, 0.5, 0.25)),
    nrow = n_samples, ncol = n_snps
  )
  gds_path <- tempfile(fileext = ".gds")
  SNPRelate::snpgdsCreateGeno(
    gds_path, genmat = genmat, sample.id = sample_id, snp.id = snp_id,
    snp.chromosome = rep(1L, n_snps), snp.position = seq_len(n_snps),
    snp.allele = rep("A/G", n_snps), snpfirstdim = FALSE
  )
  gds <- SNPRelate::snpgdsOpen(gds_path)
  on.exit(SNPRelate::snpgdsClose(gds), add = TRUE)
  metadata <- popgenVCF:::normalize_sample_aliases(data.table::data.table(
    sample = sample_id, population = rep("PopA", n_samples)
  ))
  cfg <- list(output = list(
    figure_formats = "pdf", dpi = 150L, label_samples = "auto"
  ), analyses = list(pca_metadata_color = FALSE))

  auto <- popgenVCF:::run_pca(gds, sample_id, snp_id, metadata, "auto", 1L)
  dirs_auto <- list(figures = withr::local_tempdir())
  popgenVCF:::plot_pca(auto, cfg, dirs_auto, metadata)
  expect_true(file.exists(file.path(dirs_auto$figures, "06b_PCA_Tracy_Widom_test.pdf")))

  # Fixed n_pcs, default (no comparison requested): no figure, matching
  # run_module_pca()'s own opt-in call (always_tracy_widom = TRUE) not
  # having been used here.
  fixed <- popgenVCF:::run_pca(gds, sample_id, snp_id, metadata, 10L, 1L)
  dirs_fixed <- list(figures = withr::local_tempdir())
  popgenVCF:::plot_pca(fixed, cfg, dirs_fixed, metadata)
  expect_false(file.exists(file.path(dirs_fixed$figures, "06b_PCA_Tracy_Widom_test.pdf")))

  # Fixed n_pcs, with the comparison requested (production's actual default
  # via run_module_pca()): the figure is written, with the fixed 10-PC
  # choice compared against whatever Tracy-Widom recommends.
  fixed_tw <- popgenVCF:::run_pca(
    gds, sample_id, snp_id, metadata, 10L, 1L, always_tracy_widom = TRUE
  )
  dirs_fixed_tw <- list(figures = withr::local_tempdir())
  popgenVCF:::plot_pca(fixed_tw, cfg, dirs_fixed_tw, metadata)
  expect_true(file.exists(file.path(dirs_fixed_tw$figures, "06b_PCA_Tracy_Widom_test.pdf")))
})

test_that("validate_config accepts analyses.n_pcs = \"auto\" and rejects other strings", {
  cfg <- popgenVCF::default_config()
  cfg$input$vcf <- tempfile(fileext = ".vcf")
  file.create(cfg$input$vcf)
  cfg$output$directory <- tempfile("popgenvcf-output-")
  cfg$analyses$n_pcs <- "auto"
  validated <- popgenVCF:::validate_config(cfg)
  expect_identical(validated$analyses$n_pcs, "auto")

  cfg$analyses$n_pcs <- "AUTO"
  expect_error(popgenVCF:::validate_config(cfg), "auto")

  cfg$analyses$n_pcs <- "bogus"
  expect_error(popgenVCF:::validate_config(cfg), "auto")
})
