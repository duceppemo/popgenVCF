test_that("ancestry backend registry reports availability", {
  available <- new_ancestry_backend(
    "snmf",
    availability = function() list(available = TRUE, reason = "mock available"),
    execute = function(task) task,
    parse = function(native, task) new_ancestry_replicate(
      task$sample_ids,
      matrix(rep(c(0.8, 0.2), length(task$sample_ids)), ncol = 2, byrow = TRUE),
      "snmf", k = task$k, replicate = task$replicate, seed = task$seed
    )
  )
  unavailable <- new_ancestry_backend(
    "admixture",
    availability = function() list(available = FALSE, reason = "not installed"),
    execute = function(task) task,
    parse = function(native, task) stop("must not execute")
  )
  registry <- new_ancestry_backend_registry(list(available, unavailable))
  status <- ancestry_backend_status(registry)
  expect_equal(status$backend, c("snmf", "admixture"))
  expect_identical(status$available, c(TRUE, FALSE))
  expect_match(status$reason[[2]], "not installed")
})

test_that("run_ancestry schedules deterministic K and replicate tasks", {
  backend <- new_ancestry_backend(
    "snmf",
    availability = function() TRUE,
    execute = function(task) {
      q <- matrix(1 / task$k, nrow = length(task$sample_ids), ncol = task$k)
      list(q = q, metric = task$k + task$replicate / 100)
    },
    parse = function(native, task) new_ancestry_replicate(
      task$sample_ids, native$q, "snmf", k = task$k,
      replicate = task$replicate, seed = task$seed,
      metrics = c(cross_entropy = native$metric)
    )
  )
  registry <- new_ancestry_backend_registry(list(backend))
  out <- run_ancestry(list(), c("s1", "s2", "s3"), backend = "auto",
                      k_values = 2:3, replicates = 2, seed = 7,
                      registry = registry)
  expect_named(out$results, "snmf")
  expect_s3_class(out$results$snmf, "PopgenVCFAncestryResult")
  expect_equal(length(out$results$snmf$replicates), 4)
  expect_equal(out$records$k, c(2L, 2L, 3L, 3L))
  expect_equal(out$records$replicate, c(1L, 2L, 1L, 2L))
  expect_equal(out$records$seed, c(20008L, 20009L, 30008L, 30009L))
  expect_true(all(vapply(out$results$snmf$replicates,
                         function(x) !is.null(x$provenance$backend_plugin),
                         logical(1))))
})

test_that("run_ancestry gracefully skips unavailable backends", {
  backend <- new_ancestry_backend(
    "admixture", availability = function() FALSE,
    execute = function(task) stop("must not execute"),
    parse = function(native, task) native
  )
  registry <- new_ancestry_backend_registry(list(backend))
  out <- run_ancestry(list(), c("s1", "s2"), backend = "all", k_values = 2,
                      registry = registry, fail_if_none = FALSE)
  expect_length(out$results, 0)
  expect_equal(nrow(out$records), 0)
  expect_false(out$status$available[[1]])
  expect_error(run_ancestry(list(), c("s1", "s2"), backend = "all",
                            k_values = 2, registry = registry),
               "no requested ancestry backend")
})

test_that("backend registry rejects duplicate and invalid plugins", {
  backend <- new_ancestry_backend("snmf", function() TRUE,
                                  function(task) task,
                                  function(native, task) native)
  registry <- new_ancestry_backend_registry(list(backend))
  expect_error(register_ancestry_backend(registry, backend), "already registered")
  expect_error(new_ancestry_backend("", function() TRUE, function(task) task),
               "non-empty")
  expect_error(run_ancestry(list(), c("s1", "s1"), registry = registry),
               "unique")
  expect_error(run_ancestry(list(), c("s1", "s2"), k_values = 1,
                            registry = registry), "greater than or equal")
})