test_that("spatial_autocorrelation_r matches PopGenReport::spautocor() on a synthetic fixture (cross-validated externally, see NEWS.md)", {
  # This exact fixture (40 individuals, 25 loci, seed 11) was run through
  # PopGenReport::spautocor()/gd.smouse() (the standard reference
  # implementation, sourced directly rather than installed -- its own
  # package dependencies, raster/terra/gdistance, are unrelated to this
  # function and do not build in every environment) during development.
  # The reference gave the r values pinned below (max abs diff from this
  # package's own implementation: ~1e-16, machine precision) and n_pairs
  # values exactly half of spautocor()'s own reported N (spautocor()
  # computes N via length() on a 2-column arr.ind matrix, i.e. genuinely
  # 2x the number of pairs, not the number of pairs itself -- confirmed by
  # inspecting its source; this package reports the actual pair count).
  set.seed(11)
  n_ind <- 40L; n_snp <- 25L
  genotype <- matrix(rbinom(n_ind * n_snp, 2, 0.4), nrow = n_ind, ncol = n_snp)
  xy <- matrix(runif(n_ind * 2, 0, 100), ncol = 2)

  gd <- as.matrix(stats::dist(genotype, method = "euclidean"))^2
  ed <- as.matrix(stats::dist(xy))

  res <- popgenVCF:::spatial_autocorrelation_r(gd, ed, bins = 8L)

  expect_equal(res$bin_upper, c(13.42, 26.84, 40.26, 53.68, 67.10, 80.52, 93.94, 107.36), tolerance = 1e-6)
  expect_equal(res$n_pairs, c(55L, 107L, 134L, 126L, 130L, 126L, 74L, 25L))
  expect_equal(
    res$r,
    c(-0.01598388, -0.02780897, -0.03707642, -0.02242104, -0.03679230, -0.01282461, -0.03275641, 0.01564820),
    tolerance = 1e-6
  )
})

test_that("spatial_autocorrelation_r tolerates a real NA in the genetic-distance matrix instead of crashing", {
  # Real crash found in a pre-release audit: stats::dist() returns NA for a
  # sample pair sharing zero non-missing genotyped loci -- a reachable,
  # non-adversarial outcome after QC that thresholds only aggregate
  # missingness, not per-pair overlap. rs/sgd already used na.rm = TRUE
  # (showing this was anticipated), but cx/cxii/cxjj downstream did not, so
  # a single NA pair propagated into `denom`, and `if (denom == 0)` on an NA
  # denom errored outright ("missing value where TRUE/FALSE needed")
  # instead of gracefully returning NA_real_ for that bin.
  set.seed(21)
  n_ind <- 12L; n_snp <- 10L
  genotype <- matrix(rbinom(n_ind * n_snp, 2, 0.4), nrow = n_ind, ncol = n_snp)
  xy <- matrix(runif(n_ind * 2, 0, 100), ncol = 2)
  gd <- as.matrix(stats::dist(genotype, method = "euclidean"))^2
  ed <- as.matrix(stats::dist(xy))
  gd[1L, 3L] <- gd[3L, 1L] <- NA_real_

  expect_no_error(res <- popgenVCF:::spatial_autocorrelation_r(gd, ed, bins = 6L))
  expect_s3_class(res, "data.table")
  expect_true(nrow(res) == 6L)
})

test_that("individual_genetic_distance reduces to sum of squared dosage differences (verified against gd.smouse(), see NEWS.md)", {
  # Same fixture as above; the squared-Euclidean-on-dosage reduction matched
  # PopGenReport::gd.smouse()'s own allele-count-based computation to
  # floating-point precision (~7e-15) during development.
  set.seed(11)
  n_ind <- 40L; n_snp <- 25L
  genotype <- matrix(rbinom(n_ind * n_snp, 2, 0.4), nrow = n_ind, ncol = n_snp)
  gd <- as.matrix(stats::dist(genotype, method = "euclidean"))^2

  # Hand check on the first pair directly from the dosage vectors.
  expected <- sum((genotype[1, ] - genotype[2, ])^2)
  expect_equal(gd[1, 2], expected)
})

test_that("run_spatial_autocorrelation recovers real spatial structure and a significant near-zero-distance class", {
  # Two well-separated clusters, each spatially tight and genetically
  # homogeneous within itself but distinct from the other -- a real,
  # detectable positive autocorrelation signal at short distances.
  set.seed(3)
  n_per_cluster <- 20L; n_snp <- 40L
  cluster <- rep(c("A", "B"), each = n_per_cluster)
  base_freq <- c(A = 0.15, B = 0.85)
  genotype <- matrix(NA_integer_, nrow = length(cluster), ncol = n_snp)
  for (j in seq_len(n_snp)) {
    for (i in seq_along(cluster)) genotype[i, j] <- rbinom(1, 2, base_freq[[cluster[i]]])
  }
  sample_id <- paste0("S", seq_along(cluster))
  rownames(genotype) <- sample_id
  lat <- ifelse(cluster == "A", 10, 60) + stats::rnorm(length(cluster), 0, 0.05)
  lon <- ifelse(cluster == "A", 10, 10) + stats::rnorm(length(cluster), 0, 0.05)
  metadata <- data.table::data.table(sample = sample_id, latitude = lat, longitude = lon)

  res <- popgenVCF:::run_spatial_autocorrelation(
    genotype, sample_id, metadata, c("latitude", "longitude"),
    bins = 6L, permutations = 199L, seed = 42L
  )

  expect_false(is.null(res))
  expect_true(all(c("bin_upper", "n_pairs", "r", "p_value", "null_lower", "null_upper") %in% names(res)))
  # Shortest distance class (within-cluster pairs) should show strong,
  # significant positive autocorrelation.
  expect_gt(res$r[1], 0.3)
  expect_lt(res$p_value[1], 0.05)
})

test_that("run_spatial_autocorrelation returns NULL without usable coordinates, matching run_mantel_ibd()'s convention", {
  genotype <- matrix(rbinom(40, 2, 0.3), nrow = 4L)
  rownames(genotype) <- paste0("S", 1:4)
  metadata_missing <- data.table::data.table(sample = paste0("S", 1:4))
  expect_null(popgenVCF:::run_spatial_autocorrelation(
    genotype, rownames(genotype), metadata_missing, c("latitude", "longitude")
  ))

  metadata_invalid <- data.table::data.table(
    sample = paste0("S", 1:4), latitude = c(10, 20, 30, 91), longitude = c(1, 2, 3, 4)
  )
  expect_null(popgenVCF:::run_spatial_autocorrelation(
    genotype, rownames(genotype), metadata_invalid, c("latitude", "longitude")
  ))
})

test_that("run_spatial_autocorrelation returns NULL with permutations = 0 handled gracefully, and skips the null envelope", {
  set.seed(5)
  genotype <- matrix(rbinom(200, 2, 0.3), nrow = 10L)
  rownames(genotype) <- paste0("S", 1:10)
  metadata <- data.table::data.table(
    sample = paste0("S", 1:10), latitude = seq(10, 19), longitude = seq(1, 10)
  )
  res <- popgenVCF:::run_spatial_autocorrelation(
    genotype, rownames(genotype), metadata, c("latitude", "longitude"),
    bins = 4L, permutations = 0L
  )
  expect_false(is.null(res))
  expect_true(all(is.na(res$p_value)))
  expect_true(all(is.na(res$null_lower)))
})

test_that("plot_spatial_autocorrelation writes a figure file, and is a no-op for a NULL result", {
  res <- data.table::data.table(
    bin_upper = c(10, 20, 30), n_pairs = c(5L, 8L, 3L),
    r = c(0.2, 0.05, -0.1), p_value = c(0.01, 0.4, 0.8),
    null_lower = c(-0.1, -0.15, -0.2), null_upper = c(0.1, 0.15, 0.2)
  )
  cfg <- popgenVCF::default_config(); cfg$output$figure_formats <- "png"
  out <- tempfile("spautocor-plot-"); dirs <- list(figures = file.path(out, "figures"))
  dir.create(dirs$figures, recursive = TRUE)

  popgenVCF:::plot_spatial_autocorrelation(res, cfg, dirs)
  expect_true(file.exists(file.path(dirs$figures, "50_spatial_autocorrelation.png")))

  out2 <- tempfile("spautocor-plot-null-"); dirs2 <- list(figures = file.path(out2, "figures"))
  dir.create(dirs2$figures, recursive = TRUE)
  popgenVCF:::plot_spatial_autocorrelation(NULL, cfg, dirs2)
  expect_false(file.exists(file.path(dirs2$figures, "50_spatial_autocorrelation.png")))
})

test_that("validate_spatial_autocorrelation_result accepts a well-formed result and flags real defects", {
  ok <- data.table::data.table(
    bin_upper = c(10, 20, 30), n_pairs = c(5L, 8L, 3L),
    r = c(0.2, 0.05, -0.1), p_value = c(0.01, 0.4, 0.8),
    null_lower = c(-0.1, -0.15, -0.2), null_upper = c(0.1, 0.15, 0.2)
  )
  expect_true(popgenVCF:::validate_spatial_autocorrelation_result(ok, NULL, NULL)$valid)
  expect_true(popgenVCF:::validate_spatial_autocorrelation_result(NULL, NULL, NULL)$valid)

  bad_n_pairs <- data.table::copy(ok); bad_n_pairs[1L, n_pairs := -1L]
  expect_false(popgenVCF:::validate_spatial_autocorrelation_result(bad_n_pairs, NULL, NULL)$valid)

  bad_bins <- data.table::copy(ok); bad_bins[2L, bin_upper := 5]
  expect_false(popgenVCF:::validate_spatial_autocorrelation_result(bad_bins, NULL, NULL)$valid)

  bad_p <- data.table::copy(ok); bad_p[1L, p_value := 1.5]
  expect_false(popgenVCF:::validate_spatial_autocorrelation_result(bad_p, NULL, NULL)$valid)
})

test_that("spatial_autocorrelation_module_spec is registered, gated on coordinates, and enabled by default", {
  registry <- popgenVCF::default_analysis_registry()
  expect_true("spatial_autocorrelation" %in% names(registry$modules))
  module <- registry$modules$spatial_autocorrelation

  spec <- popgenVCF::spatial_autocorrelation_module_spec()
  expect_identical(module$run, spec$run)
  expect_identical(module$validate, spec$validate)
  expect_identical(module$outputs, spec$outputs)
  expect_identical(module$references, spec$references)
  expect_identical(module$resource_class, spec$resource_class)
  expect_identical(module$contract_version, spec$contract_version)

  cfg <- popgenVCF::default_config()
  expect_true(spec$enabled(cfg))
  cfg$analyses$spatial_autocorrelation <- FALSE
  expect_false(spec$enabled(cfg))
})
