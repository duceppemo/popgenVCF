dapc_parallel_fixture <- function() {
  set.seed(101)
  n <- 18L
  groups <- rep(c("A", "B", "C"), each = 6L)
  genotype <- matrix(stats::rbinom(n * 90L, 2L, 0.15), nrow = n)
  genotype[groups == "A", 1:30] <- 2L
  genotype[groups == "B", 31:60] <- 2L
  genotype[groups == "C", 61:90] <- 2L
  sample_ids <- paste0("sample_", seq_len(n))
  metadata <- popgenVCF:::normalize_sample_aliases(data.table::data.table(
    sample = sample_ids,
    population = groups
  ))
  list(genotype = genotype, sample_ids = sample_ids, metadata = metadata)
}

test_that("DAPC worker count is bounded by K values and platform support", {
  expect_identical(
    popgenVCF:::dapc_worker_count(2:8, 64L, fork_available = TRUE),
    7L
  )
  expect_identical(
    popgenVCF:::dapc_worker_count(2:8, 3L, fork_available = TRUE),
    3L
  )
  expect_identical(
    popgenVCF:::dapc_worker_count(2:8, 64L, fork_available = FALSE),
    1L
  )
  expect_identical(
    popgenVCF:::dapc_worker_count(2:8, NA_integer_, fork_available = TRUE),
    1L
  )
})

test_that("parallel DAPC matches serial output and computes one PCA per run", {
  fixture <- dapc_parallel_fixture()
  original_compute <- popgenVCF:::compute_dapc_shared_pca
  pca_calls <- 0L
  local_mocked_bindings(
    compute_dapc_shared_pca = function(...) {
      pca_calls <<- pca_calls + 1L
      original_compute(...)
    },
    .package = "popgenVCF"
  )

  run <- function(threads) popgenVCF:::run_dapc_analysis(
    fixture$genotype, fixture$sample_ids, fixture$metadata,
    k_values = 2:3, seed = 42L, cross_validate = FALSE,
    replicate_seeds = 42:43, threads = threads
  )
  serial <- run(1L)
  parallel <- run(2L)

  gl <- popgenVCF:::genlight_from_gds(
    fixture$genotype, fixture$sample_ids, fixture$metadata
  )
  set.seed(44L)
  uncached_cluster <- adegenet::find.clusters(
    gl, n.pca = 17L, n.clust = 2L, choose.n.clust = FALSE
  )
  uncached_model <- adegenet::dapc(
    gl, pop = uncached_cluster$grp, n.pca = 14L, n.da = 1L
  )
  uncached_membership <- popgenVCF:::extract_dapc_membership(
    uncached_model,
    popgenVCF:::public_sample_ids(fixture$metadata, fixture$sample_ids)
  )

  expect_identical(pca_calls, 2L)
  expect_identical(names(serial$models), c("2", "3"))
  expect_identical(names(parallel$models), names(serial$models))
  expect_equal(parallel$diagnostics, serial$diagnostics, tolerance = 1e-12)
  expect_true(all(c("BIC", "mean_success", "calinski_harabasz", "davies_bouldin") %in%
                  names(serial$diagnostics)))
  expect_true(all(is.finite(serial$diagnostics$calinski_harabasz)))
  expect_true(all(is.na(serial$diagnostics$mean_success)))
  expect_equal(
    serial$models[["2"]]$membership, uncached_membership,
    tolerance = 1e-12
  )
  for (k in names(serial$models)) {
    expect_equal(
      parallel$models[[k]]$membership,
      serial$models[[k]]$membership,
      tolerance = 1e-12
    )
    expect_equal(
      parallel$models[[k]]$reproducibility$metrics,
      serial$models[[k]]$reproducibility$metrics,
      tolerance = 1e-12
    )
    expect_match(
      paste(deparse(serial$models[[k]]$model$call), collapse = " "),
      "glPca = shared_pca", fixed = TRUE
    )
  }
})

test_that("DAPC avoids PCA work when no requested K is valid", {
  fixture <- dapc_parallel_fixture()
  result <- popgenVCF:::run_dapc_analysis(
    fixture$genotype, fixture$sample_ids, fixture$metadata,
    k_values = 1L, seed = 42L, threads = 4L
  )
  expect_length(result$models, 0L)
  expect_equal(nrow(result$diagnostics), 0L)
})
