test_that("reference adapter contracts and status are deterministic", {
  adapter <- new_reference_adapter(
    id = "mock_pca", analysis = "pca", tool = "stats",
    kind = "package", dependency = "stats", mode = "subspace",
    observed = function(x) x$observed$pca,
    reference = function(x) x$references$pca
  )
  expect_s3_class(adapter, "PopgenVCFReferenceAdapter")
  status <- reference_adapter_status(adapter)
  expect_true(status$available)
  expect_equal(status$analysis, "pca")
  expect_true(nzchar(status$version))
})

test_that("adapter specifications run through external-reference contracts", {
  basis <- cbind(c(-1, 0, 1), c(0, 1, -1))
  rotated <- basis %*% matrix(c(0, -1, 1, 0), 2)
  adapter <- new_reference_adapter(
    "mock_subspace", "pca", "stats", "package", "stats", "subspace", "equivalence",
    observed = function(x) x$observed$pca,
    reference = function(x) x$references$pca,
    absolute_tolerance = 1e-10
  )
  result <- run_external_reference(
    reference_adapter_spec(adapter),
    list(observed = list(pca = rotated), references = list(pca = basis))
  )
  expect_equal(result$status, "passed")
  expect_true(all(result$comparisons$passed))
})

test_that("unavailable adapters skip transparently", {
  adapter <- new_reference_adapter(
    "missing_tool", "pca", "missing", "executable",
    "popgenvcf-definitely-missing-executable", "numeric", "equivalence",
    observed = function(x) 1, reference = function(x) 1
  )
  status <- reference_adapter_status(adapter)
  expect_false(status$available)
  expect_match(status$reason, "not available")
  result <- run_external_reference(reference_adapter_spec(adapter), list())
  expect_equal(result$status, "skipped")
})

test_that("default registry declares established tools and scientific roles", {
  registry <- default_reference_adapter_registry()
  expect_true(all(c(
    "snprelate_pca", "snprelate_ibs", "plink2_pca", "hierfstat_fst",
    "adegenet_dapc", "poppr_amova", "pegas_amova", "vegan_mantel",
    "admixture_q", "faststructure_q", "lea_snmf_q"
  ) %in% names(registry)))
  expect_equal(registry$snprelate_ibs$role, "equivalence")
  expect_equal(registry$hierfstat_fst$role, "diagnostic")
  expect_equal(registry$admixture_q$mode, "q_matrix")
})

test_that("run_reference_adapters filters analyses and preserves diagnostics", {
  equivalence <- new_reference_adapter(
    "mock_equivalence", "ibs", "stats", "package", "stats", "matrix", "equivalence",
    observed = function(x) x$observed$ibs,
    reference = function(x) x$references$ibs
  )
  diagnostic <- new_reference_adapter(
    "mock_diagnostic", "fst", "stats", "package", "stats", "numeric", "diagnostic",
    observed = function(x) x$observed$fst,
    reference = function(x) x$references$fst,
    absolute_tolerance = 0
  )
  payload <- list(
    observed = list(ibs = diag(2), fst = 0.1),
    references = list(ibs = diag(2), fst = 0.2)
  )
  run <- run_reference_adapters(payload, list(equivalence = equivalence, diagnostic = diagnostic))
  expect_equal(run$results$equivalence$status, "passed")
  expect_equal(run$results$diagnostic$status, "passed")
  expect_false(run$results$diagnostic$comparisons$passed)
  only_fst <- run_reference_adapters(payload, list(equivalence = equivalence, diagnostic = diagnostic), "fst")
  expect_equal(names(only_fst$results), "diagnostic")
})

test_that("malformed adapter contracts are rejected", {
  expect_error(
    new_reference_adapter("x", "pca", "tool", "package", "stats", observed = 1, reference = identity),
    "must be functions"
  )
  x <- structure(list(schema_version = "1.0"), class = "PopgenVCFReferenceAdapter")
  expect_error(validate_reference_adapter(x))
})
