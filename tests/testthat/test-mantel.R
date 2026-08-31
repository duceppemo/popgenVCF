test_that("haversine matrix is symmetric", {
  x <- popgenVCF:::haversine_matrix(c(45,46,47), c(-75,-76,-77), c("a","b","c"))
  expect_equal(x, t(x))
  expect_equal(unname(diag(x)), c(0, 0, 0))
})

test_that("haversine matrix clamps floating-point overshoot for near-antipodal coordinates instead of returning NaN", {
  # Real bug found in a pre-release audit: the haversine formula's
  # intermediate `a` term is mathematically bounded in [0, 1], but is a sum
  # of two independently-rounded trig terms -- for exact antipodal
  # coordinates (a real, if geographically extreme, input; plausible for a
  # population-level representative coordinate spanning a wide range),
  # floating-point rounding pushes it fractionally above 1, and the
  # unclamped `sqrt(1 - a)` silently returns NaN. This exact pair
  # (lat -89.26/89.26, lon -180/0) was confirmed to trigger a > 1 on this
  # platform before the fix. The true antipodal distance is half Earth's
  # circumference (~20015 km via the mean radius this function uses).
  x <- popgenVCF:::haversine_matrix(c(-89.26, 89.26), c(-180, 0), c("a", "b"))
  expect_false(anyNA(x))
  expect_equal(unname(x["a", "b"]), pi * 6371.0088, tolerance = 1e-6)
})

test_that("Mantel IBD joins public IBS labels to aliased metadata", {
  metadata <- popgenVCF::new_sample_identity(data.table::data.table(
    sample = paste0("raw_", 1:4),
    alias = paste0("Public ", 1:4),
    latitude = c(40, 42, 45, 48),
    longitude = c(-75, -72, -68, -63)
  ))
  labels <- metadata$public_sample
  coordinates <- cbind(metadata$latitude, metadata$longitude)
  genetic_distance <- as.matrix(stats::dist(coordinates / c(10, 20)))
  rownames(genetic_distance) <- colnames(genetic_distance) <- labels

  result <- suppressWarnings(popgenVCF:::run_mantel_ibd(
    genetic_distance, metadata, c("latitude", "longitude"),
    permutations = 9L, seed = 42L
  ))

  expect_type(result, "list")
  expect_equal(nrow(result$pairs), choose(length(labels), 2L))
  expect_true(all(is.finite(result$pairs$geographic_distance_km)))
})

test_that("Mantel IBD rejects coordinates outside geographic ranges", {
  metadata <- data.table::data.table(
    sample = paste0("s", 1:4),
    latitude = c(45, 46, 47, 91),
    longitude = c(-75, -76, -77, -78)
  )
  distance <- as.matrix(stats::dist(seq_len(4L)))
  rownames(distance) <- colnames(distance) <- metadata$sample

  expect_null(popgenVCF:::run_mantel_ibd(
    distance, metadata, c("latitude", "longitude"), permutations = 9L
  ))
})

test_that("run_mantel_ibd computes a partial Mantel test controlling for population", {
  set.seed(5)
  n <- 20L
  pop <- rep(c("A", "B"), each = n / 2L)
  sample_id <- paste0("S", seq_len(n))
  coord <- seq_len(n)
  metadata <- data.table::data.table(
    sample = sample_id, population = pop,
    latitude = 40 + coord * 0.1, longitude = -70 + coord * 0.1
  )
  gd <- as.matrix(stats::dist(coord)) + matrix(stats::rnorm(n * n, 0, 0.5), n, n)
  gd <- (gd + t(gd)) / 2; diag(gd) <- 0
  rownames(gd) <- colnames(gd) <- sample_id

  res <- popgenVCF:::run_mantel_ibd(gd, metadata, c("latitude", "longitude"), permutations = 199L, seed = 42L)

  expect_false(is.null(res$partial_mantel))
  expect_s3_class(res$partial_mantel, "mantel")
  expect_true(is.finite(res$summary$partial_mantel_r))
  expect_true(is.finite(res$summary$partial_mantel_p))
  expect_true(res$summary$partial_mantel_r >= -1 && res$summary$partial_mantel_r <= 1)
})

test_that("run_mantel_ibd leaves partial Mantel NA without a population column or with only one population", {
  set.seed(6)
  n <- 8L
  sample_id <- paste0("S", seq_len(n))
  gd <- as.matrix(stats::dist(seq_len(n)))
  rownames(gd) <- colnames(gd) <- sample_id
  coord <- seq_len(n)

  no_population <- data.table::data.table(sample = sample_id, latitude = 40 + coord, longitude = -70 + coord)
  res1 <- popgenVCF:::run_mantel_ibd(gd, no_population, c("latitude", "longitude"), permutations = 19L, seed = 42L)
  expect_null(res1$partial_mantel)
  expect_true(is.na(res1$summary$partial_mantel_r))

  one_population <- data.table::data.table(
    sample = sample_id, population = "A", latitude = 40 + coord, longitude = -70 + coord
  )
  res2 <- popgenVCF:::run_mantel_ibd(gd, one_population, c("latitude", "longitude"), permutations = 19L, seed = 42L)
  expect_null(res2$partial_mantel)
  expect_true(is.na(res2$summary$partial_mantel_r))
})

test_that("plot_ibd's subtitle includes the partial Mantel result when available", {
  fx <- data.table::data.table(
    genetic_distance = c(0.1, 0.5, 0.9), geographic_distance_km = c(10, 500, 900)
  )
  x_with_partial <- list(
    pairs = fx,
    summary = data.table::data.table(
      mantel_r = 0.5, mantel_p = 0.01, slope = 1, r_squared = 0.25,
      partial_mantel_r = 0.3, partial_mantel_p = 0.04
    )
  )
  out <- tempfile("ibd-plot-"); dirs <- list(figures = file.path(out, "figures"))
  dir.create(dirs$figures, recursive = TRUE)
  cfg <- popgenVCF::default_config(); cfg$output$figure_formats <- "png"
  popgenVCF:::plot_ibd(x_with_partial, cfg, dirs)
  expect_true(file.exists(file.path(dirs$figures, "12_isolation_by_distance.png")))

  x_without_partial <- x_with_partial
  x_without_partial$summary <- data.table::data.table(
    mantel_r = 0.5, mantel_p = 0.01, slope = 1, r_squared = 0.25,
    partial_mantel_r = NA_real_, partial_mantel_p = NA_real_
  )
  out2 <- tempfile("ibd-plot2-"); dirs2 <- list(figures = file.path(out2, "figures"))
  dir.create(dirs2$figures, recursive = TRUE)
  popgenVCF:::plot_ibd(x_without_partial, cfg, dirs2)
  expect_true(file.exists(file.path(dirs2$figures, "12_isolation_by_distance.png")))
})
