scheduler_module <- function(delay = 0, value = NULL) {
  list(
    run = local({
      wait <- delay
      result <- value
      function(analysis, context) {
        Sys.sleep(wait)
        list(analysis = analysis, context = context, result = result)
      }
    }),
    requires = character(),
    outputs = "result",
    resource_class = "light",
    parallel_safe = TRUE,
    artifacts = character(),
    artifacts_must_exist = FALSE,
    validate = function(...) list(valid = TRUE)
  )
}

test_that("multisession is a portable execution backend", {
  engine <- new_execution_engine(workers = 2L, backend = "multisession")
  expect_s3_class(engine, "PopgenVCFExecutionEngine")
  expect_identical(engine$backend, "multisession")
  expect_identical(engine$workers, 2L)
  expect_error(new_execution_engine(backend = "unknown"), "arg")
})

test_that("portable workers preserve dispatch order and expose completion order", {
  skip_on_cran()
  registry <- list(modules = list(
    slow = scheduler_module(0.35, "slow"),
    fast = scheduler_module(0.05, "fast")
  ))
  engine <- new_execution_engine(workers = 2L, backend = "multisession")
  executions <- popgenVCF:::run_execution_batch(
    c("slow", "fast"), list(), list(), registry, engine
  )

  expect_identical(vapply(executions, `[[`, character(1), "name"), c("slow", "fast"))
  completion <- popgenVCF:::scheduler_sequence(
    executions, "finished_numeric", c("slow", "fast")
  )
  expect_identical(unname(completion[["fast"]]), 1L)
  expect_identical(unname(completion[["slow"]]), 2L)
  expect_true(all(vapply(executions, `[[`, integer(1), "worker_pid") > 0L))
})

test_that("scheduler completion ties resolve in planned order", {
  executions <- list(
    list(name = "beta", finished_numeric = 1),
    list(name = "alpha", finished_numeric = 1)
  )
  sequence <- popgenVCF:::scheduler_sequence(
    executions, "finished_numeric", c("alpha", "beta")
  )
  expect_identical(unname(sequence[c("alpha", "beta")]), c(1L, 2L))
})

test_that("resource limits split deterministic scheduler batches", {
  registry <- list(modules = list(
    a = scheduler_module(),
    b = scheduler_module(),
    c = scheduler_module()
  ))
  plan <- structure(
    list(order = c("a", "b", "c"), waves = c(a = 1L, b = 1L, c = 1L)),
    class = "PopgenVCFExecutionPlan"
  )
  engine <- new_execution_engine(
    workers = 3L,
    backend = "multisession",
    resource_limits = c(light = 2L, standard = 1L, heavy = 1L, external = 1L)
  )
  batches <- popgenVCF:::execution_batches(plan, registry, engine)
  expect_identical(batches, list(c("a", "b"), "c"))
})

test_that("scheduler ledger adds deterministic provenance columns", {
  ledger <- data.table::data.table(module = c("a", "b"), status = "pending")
  ledger <- popgenVCF:::ensure_scheduler_ledger(ledger)
  expect_true(all(c(
    "dispatch_sequence", "completion_sequence", "merge_sequence", "worker_pid"
  ) %in% names(ledger)))
  expect_true(all(is.na(ledger$dispatch_sequence)))
})
