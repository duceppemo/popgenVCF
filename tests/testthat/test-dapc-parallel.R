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

test_that("a worker killed outright (NULL, not a try-error) is reported loudly, not silently dropped", {
  # Real regression found in a pre-release audit: a forked worker killed by
  # a signal (OOM, segfault) returns NULL for that slot in mclapply()'s
  # results, not a "try-error" -- execute_dapc_k_tasks() only checked for
  # "try-error", so the caller's `vapply(results, `[[`, ..., "key")` would
  # error confusingly (or, if a NULL happened to survive further, silently
  # drop that K value's model with no error at all). run_dapc_k_task() is
  # mocked to return NULL (what mclapply's own results list actually
  # contains for a killed worker) rather than trying to really kill a
  # worker process.
  local_mocked_bindings(run_dapc_k_task = function(...) NULL, .package = "popgenVCF")
  fixture <- dapc_parallel_fixture()
  expect_error(
    popgenVCF:::run_dapc_analysis(
      fixture$genotype, fixture$sample_ids, fixture$metadata,
      k_values = 2:4, seed = 42L, cross_validate = FALSE,
      replicate_seeds = 42:44, threads = 2L
    ),
    "terminated abnormally"
  )
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

test_that("run_dapc_k_task's xvalDapc call never requests boot::boot()'s parallel bootstrap", {
  # Deliberately NOT wired up: adegenet::xvalDapc() forwards `...` into
  # boot::boot(sim = "parametric", ...), which per boot's own documentation
  # resamples inside the worker processes with each choosing its own
  # separate, non-reproducible seed. Confirmed directly on this package's
  # real quickstart dataset: identical data/seed gave n.pca = 10 (replicate
  # RMSE ~0) serial vs. n.pca = 40 (RMSE 0.076, exceeding the stability
  # threshold) with parallel = "multicore" enabled -- a materially
  # different, less reproducible result, not just a faster one. Every other
  # parallel path in this codebase is verified byte-identical regardless of
  # thread count; this call must stay serial to match that guarantee.
  fixture <- dapc_parallel_fixture()
  gl <- popgenVCF:::genlight_from_gds(fixture$genotype, fixture$sample_ids, fixture$metadata)
  public_ids <- popgenVCF:::public_sample_ids(fixture$metadata, fixture$sample_ids)
  shared_pca <- popgenVCF:::compute_dapc_shared_pca(gl, 10L)
  truth <- fixture$metadata$population[match(fixture$sample_ids, fixture$metadata$sample)]
  stub_cv <- list(
    `Number of PCs Achieving Highest Mean Success` = "5",
    `Mean Successful Assignment by Number of PCs of PCA` = c(`5` = 0.8)
  )

  captured <- list()
  local_mocked_bindings(
    xvalDapc = function(...) {
      captured[[length(captured) + 1L]] <<- list(...)
      stub_cv
    },
    .package = "adegenet"
  )

  popgenVCF:::run_dapc_k_task(
    2L, gl = gl, shared_pca = shared_pca, max_pca = 10L,
    sample_ids = fixture$sample_ids, public_ids = public_ids,
    metadata = fixture$metadata, truth = truth,
    cross_validate = TRUE, replicate_seeds = 42L
  )
  expect_null(captured[[1L]]$parallel)
  expect_null(captured[[1L]]$ncpus)
})

test_that("xvalDapc's search ceiling is the real max_pca, not the small no-cross-validation fallback", {
  # Real bug: n.pca.max was set to the same conservative fallback value
  # (0.1 * n_samples) meant only for when cross-validation doesn't run at
  # all -- confirmed directly on real production data, this made the
  # cross-validation search cover only n.pca 1-5 for a 50-sample K=3 model,
  # and "5" (the search ceiling itself) came back as the winner: a better
  # value above 5 was never even tested, and the resulting xval curve had
  # no room to show a decline past its own peak.
  fixture <- dapc_parallel_fixture()
  gl <- popgenVCF:::genlight_from_gds(fixture$genotype, fixture$sample_ids, fixture$metadata)
  public_ids <- popgenVCF:::public_sample_ids(fixture$metadata, fixture$sample_ids)
  shared_pca <- popgenVCF:::compute_dapc_shared_pca(gl, 10L)
  truth <- fixture$metadata$population[match(fixture$sample_ids, fixture$metadata$sample)]
  stub_cv <- list(
    `Number of PCs Achieving Highest Mean Success` = "5",
    `Mean Successful Assignment by Number of PCs of PCA` = c(`5` = 0.8)
  )
  captured <- list()
  local_mocked_bindings(
    xvalDapc = function(...) {
      captured[[length(captured) + 1L]] <<- list(...)
      stub_cv
    },
    .package = "adegenet"
  )

  popgenVCF:::run_dapc_k_task(
    2L, gl = gl, shared_pca = shared_pca, max_pca = 10L,
    sample_ids = fixture$sample_ids, public_ids = public_ids,
    metadata = fixture$metadata, truth = truth,
    cross_validate = TRUE, replicate_seeds = 42L
  )
  expect_identical(captured[[1L]]$n.pca.max, 10L)
})

test_that("DAPC's cross-validation fallback n.pca uses 10% of sample count, not 80%", {
  # Real production regression: xvalDapc() was silently failing on every K
  # (see the missing-genotype test below), so every model fell back to this
  # formula. It used to be floor(n_samples * 0.8) -- confirmed directly
  # against a real 50-sample report, that put 40 PCA axes into a DAPC with
  # as few as 2 groups, and the LD1/LD2 scatter showed the classic
  # over-fitting signature (nearly every sample collapsing onto one point).
  fixture <- dapc_parallel_fixture()
  result <- popgenVCF:::run_dapc_analysis(
    fixture$genotype, fixture$sample_ids, fixture$metadata,
    k_values = 2L, seed = 42L, cross_validate = FALSE,
    replicate_seeds = 42L, threads = 1L
  )
  expect_equal(result$diagnostics$n_pca[[1L]], 2)
  expect_lt(result$diagnostics$n_pca[[1L]], floor(length(fixture$sample_ids) * .8))
})

test_that("DAPC cross-validation runs on a mean-imputed matrix, tolerating missing genotypes that would otherwise crash xvalDapc's own PCA step", {
  # Real production bug: adegenet::xvalDapc() (unlike glPca(), used
  # everywhere else in this file) calls ade4::dudi.pca() directly on
  # whatever it's given, which hard-errors on any NA ("na entries in
  # table"). A real QC-passing marker set legitimately contains missing
  # calls (QC bounds missingness, it doesn't eliminate it), so every K's
  # cross-validation was failing -- confirmed directly by reproducing the
  # exact error against real production data.
  fixture <- dapc_parallel_fixture()
  fixture$genotype[1L, 1L] <- NA_integer_
  gl <- popgenVCF:::genlight_from_gds(fixture$genotype, fixture$sample_ids, fixture$metadata)
  # tab()'s own default (NA.method = "mean") already imputes -- "asis" is
  # needed to see that the raw genlight really does carry the NA through.
  expect_true(anyNA(adegenet::tab(gl, NA.method = "asis")))

  captured_x <- NULL
  local_mocked_bindings(
    xvalDapc = function(x, ...) {
      captured_x <<- x
      list(
        `Number of PCs Achieving Highest Mean Success` = "2",
        `Mean Successful Assignment by Number of PCs of PCA` = c(`2` = 0.8)
      )
    },
    .package = "adegenet"
  )
  result <- popgenVCF:::run_dapc_analysis(
    fixture$genotype, fixture$sample_ids, fixture$metadata,
    k_values = 2L, seed = 42L, cross_validate = TRUE,
    replicate_seeds = 42L, threads = 1L
  )

  expect_false(is.null(captured_x))
  expect_false(anyNA(captured_x))
  expect_false(is.null(result$models[["2"]]$cv))
})

test_that("a DAPC cross-validation failure is logged with the real error, not silently swallowed", {
  fixture <- dapc_parallel_fixture()
  local_mocked_bindings(
    xvalDapc = function(...) stop("simulated xvalDapc failure"),
    .package = "adegenet"
  )

  expect_output(
    result <- popgenVCF:::run_dapc_analysis(
      fixture$genotype, fixture$sample_ids, fixture$metadata,
      k_values = 2L, seed = 42L, cross_validate = TRUE,
      replicate_seeds = 42L, threads = 1L
    ),
    "DAPC cross-validation failed for K.*2.*simulated xvalDapc failure"
  )
  expect_null(result$models[["2"]]$cv)
  expect_equal(result$diagnostics$n_pca[[1L]], 2)
})

test_that("single-replicate DAPC records unestimated RMSE as missing", {
  fixture <- dapc_parallel_fixture()
  result <- popgenVCF:::run_dapc_analysis(
    fixture$genotype, fixture$sample_ids, fixture$metadata,
    k_values = 2L, seed = 42L, cross_validate = FALSE,
    replicate_seeds = 42L, threads = 1L
  )

  expect_null(result$models[["2"]]$reproducibility)
  expect_true(is.na(result$diagnostics$replicate_max_rmse[[1L]]))
})
