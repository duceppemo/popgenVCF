test_that("typed core results validate and expose stable tables", {
  ids <- c("s1", "s2", "s3")
  pca <- new_pca_result(
    data.frame(sample_id = ids, PC1 = c(-1, 0, 1), PC2 = c(.2, -.1, -.1)),
    c(2, 1), parameters = list(center = TRUE),
    provenance = list(package = "SNPRelate")
  )
  expect_s3_class(pca, "PopgenVCFPCAResult")
  expect_s3_class(pca, "PopgenVCFCoreResult")
  expect_equal(core_result_table(pca)$sample_id, ids)

  sim <- diag(3); sim[lower.tri(sim)] <- c(.8, .7, .9); sim[upper.tri(sim)] <- t(sim)[upper.tri(sim)]
  rownames(sim) <- colnames(sim) <- ids
  ibs <- new_ibs_result(sim)
  expect_s3_class(ibs, "PopgenVCFIBSResult")
  ibs_table <- core_result_table(ibs)
  expect_equal(nrow(ibs_table), 9L)
  expect_equal(unique(ibs_table$sample_1), ids)
  expect_equal(unique(ibs_table$sample_2), ids)

  unnamed_ibs <- new_ibs_result(diag(2))
  unnamed_table <- core_result_table(unnamed_ibs)
  expect_equal(unnamed_table$sample_1, c("1", "2", "1", "2"))
  expect_equal(unnamed_table$sample_2, c("1", "1", "2", "2"))

  diversity <- new_diversity_result(data.frame(population = c("A", "B"), Ho = c(.2, .3)))
  fst <- new_fst_result(.12, data.frame(population_1 = "A", population_2 = "B", fst = .12))
  amova <- new_amova_result(data.frame(component = c("among", "within"), variance = c(.2, .8)))
  dapc <- new_dapc_result(data.frame(sample_id = ids, LD1 = c(-2, 0, 2)))
  ibd <- new_ibd_result(data.frame(genetic_distance = c(.1, .2), geographic_distance = c(10, 20)), .7, .01, 999)

  expect_equal(vapply(list(diversity, fst, amova, dapc, ibd), function(z) z$schema_version, character(1)), rep("1.0", 5))
  expect_equal(fst$payload$global_fst, .12)
})

test_that("legacy adapters and serialization round-trip deterministically", {
  coords <- data.frame(sample_id = c("a", "b"), PC1 = c(-1, 1), PC2 = c(0, 0))
  x <- as_core_result("pca", list(coordinates = coords, eigenvalues = c(2, 1)),
                      provenance = list(commit = "abc"))
  path <- tempfile(fileext = ".rds")
  save_core_result(x, path)
  y <- read_core_result(path)
  expect_identical(x, y)
  expect_equal(y$provenance$commit, "abc")
})

test_that("core results reject malformed schemas and failed validation", {
  expect_error(new_core_result("pca", list()), "payload must be a named list")
  expect_error(
    new_pca_result(data.frame(x = 1), 1),
    "sample_id"
  )
  expect_error(
    new_diversity_result(data.frame(population = "A"), validation = data.frame(check = "scientific", passed = FALSE)),
    "failed checks"
  )
  expect_error(
    new_ibs_result(matrix(c(1, .2, .3, 1), 2)),
    "symmetric"
  )
})

test_that("all supported core analysis classes are available", {
  sim <- diag(2)
  objects <- list(
    new_pca_result(data.frame(sample_id = c("a", "b"), PC1 = c(-1, 1), PC2 = 0), c(1, .5)),
    new_ibs_result(sim),
    new_tree_result("(a:1,b:1);"),
    new_diversity_result(data.frame(population = "A", Ho = .2)),
    new_fst_result(.1, data.frame(population_1 = "A", population_2 = "B", fst = .1)),
    new_amova_result(data.frame(component = "within", variance = 1)),
    new_dapc_result(data.frame(sample_id = c("a", "b"), LD1 = c(-1, 1))),
    new_ibd_result(data.frame(genetic_distance = .1, geographic_distance = 10), .5)
  )
  expect_true(all(vapply(objects, inherits, logical(1), "PopgenVCFCoreResult")))
  expect_equal(vapply(objects, `[[`, character(1), "analysis"),
               c("pca", "ibs", "tree", "diversity", "fst", "amova", "dapc", "ibd"))
})
