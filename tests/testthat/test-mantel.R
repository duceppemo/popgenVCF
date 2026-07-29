test_that("haversine matrix is symmetric", {
  x <- popgenVCF:::haversine_matrix(c(45,46,47), c(-75,-76,-77), c("a","b","c"))
  expect_equal(x, t(x))
  expect_equal(unname(diag(x)), c(0, 0, 0))
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
