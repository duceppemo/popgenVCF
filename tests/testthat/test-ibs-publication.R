test_that("compute_ibs_mds is deterministic and sign-stable", {
  distance <- matrix(c(
    0, 1, 2, 3,
    1, 0, 1, 2,
    2, 1, 0, 1,
    3, 2, 1, 0
  ), nrow = 4, byrow = TRUE,
  dimnames = list(LETTERS[1:4], LETTERS[1:4]))

  first <- compute_ibs_mds(distance, k = 2)
  second <- compute_ibs_mds(distance, k = 2)

  expect_identical(first$coordinates, second$coordinates)
  expect_identical(first$eigenvalues, second$eigenvalues)
  expect_identical(first$coordinates$sample_id, LETTERS[1:4])
  expect_true(all(is.finite(first$coordinates$MDS1)))
  expect_true(all(is.finite(first$coordinates$MDS2)))
  expect_gte(first$coordinates$MDS1[which.max(abs(first$coordinates$MDS1))], 0)
  expect_gte(first$coordinates$MDS2[which.max(abs(first$coordinates$MDS2))], 0)
})

test_that("compute_ibs_mds fails closed on invalid distances", {
  expect_error(compute_ibs_mds(matrix(1:6, nrow = 2)), "square")

  asymmetric <- matrix(c(0, 0.1, 0.2, 0), 2, 2)
  expect_error(compute_ibs_mds(asymmetric, k = 1), "symmetric")

  negative <- matrix(c(0, -0.1, -0.1, 0), 2, 2)
  expect_error(compute_ibs_mds(negative, k = 1), "negative")
})

test_that("IBS publication artifacts are complete and reproducible", {
  similarity <- matrix(c(
    1.0, 0.9, 0.7, 0.6,
    0.9, 1.0, 0.8, 0.7,
    0.7, 0.8, 1.0, 0.9,
    0.6, 0.7, 0.9, 1.0
  ), nrow = 4, byrow = TRUE,
  dimnames = list(paste0("s", 1:4), paste0("s", 1:4)))
  distance <- 1 - similarity
  metadata <- data.frame(
    sample_id = paste0("s", 1:4),
    population = c("north", "north", "south", "south")
  )

  out <- tempfile("ibs-publication-")
  manifest <- write_ibs_publication_artifacts(
    similarity = similarity,
    distance = distance,
    metadata = metadata,
    output_dir = out
  )

  expect_s3_class(manifest, "PopgenVCFArtifactManifest")
  table <- artifact_manifest_table(manifest)
  expect_equal(nrow(table), 11L)
  expect_true(all(file.exists(table$path)))
  expect_true(all(c("similarity", "distance", "mds_coordinates", "mds_eigenvalues",
                    "mds_pdf", "mds_svg", "mds_png", "methods", "caption",
                    "validation", "figure_source") %in% table$name))

  coordinates <- data.table::fread(file.path(out, "tables", "IBS_MDS_coordinates.tsv"))
  expect_identical(coordinates$sample_id, paste0("s", 1:4))
  expect_identical(coordinates$population, c("north", "north", "south", "south"))

  validation <- data.table::fread(file.path(out, "validation", "IBS_MDS_validation.tsv"))
  expect_true(all(validation$passed))
})

test_that("IBS publication validation rejects inconsistent matrices", {
  similarity <- diag(3)
  distance <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  bad_similarity <- similarity
  bad_similarity[1, 2] <- 0.5

  expect_error(
    write_ibs_publication_artifacts(bad_similarity, distance, output_dir = tempfile()),
    "symmetric"
  )
})
