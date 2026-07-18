make_engine_analysis <- function() {
  analysis <- new_popgen_vcf_analysis(default_config())
  analysis$samples$ids <- c("a", "b")
  analysis$samples$metadata <- data.table::data.table(
    sample = c("a", "b"), population = c("x", "y")
  )
  analysis$variants$qc_ids <- 1:2
  analysis$variants$ld_ids <- 1:2
  analysis
}

result_module <- function(name) {
  force(name)
  function(analysis, context) {
    analysis <- set_analysis_result(analysis, name, list(module = name))
    list(analysis = analysis, context = context)
  }
}

test_that("execution plans group independent modules into dependency waves", {
  registry <- new_analysis_registry()
  registry <- register_analysis(registry, "import", result_module("import"))
  registry <- register_analysis(
    registry, "pca", result_module("pca"), requires = "import",
    parallel_safe = TRUE
  )
  registry <- register_analysis(
    registry, "diversity", result_module("diversity"), requires = "import",
    parallel_safe = TRUE
  )
  registry <- register_analysis(
    registry, "report", result_module("report"),
    requires = c("pca", "diversity")
  )

  plan <- plan_analysis_execution(registry, default_config())

  expect_equal(plan$order, c("import", "pca", "diversity", "report"))
  expect_equal(unname(plan$waves), c(1L, 2L, 2L, 3L))
  expect_equal(plan$table$wave, c(1L, 2L, 2L, 3L))
})

test_that("batched modules preserve every declared result", {
  registry <- new_analysis_registry()
  registry <- register_analysis(
    registry, "pca", result_module("pca"),
    resource_class = "light", parallel_safe = TRUE
  )
  registry <- register_analysis(
    registry, "diversity", result_module("diversity"),
    resource_class = "light", parallel_safe = TRUE
  )
  engine <- new_execution_engine(workers = 2L, backend = "sequential")

  executed <- execute_analysis_registry(
    make_engine_analysis(), list(), registry, engine = engine
  )

  expect_equal(executed$order, c("pca", "diversity"))
  expect_equal(executed$analysis$results$pca$module, "pca")
  expect_equal(executed$analysis$results$diversity$module, "diversity")
  expect_equal(executed$engine$waves, 1L)
  expect_equal(executed$engine$batches, list(c("pca", "diversity")))
})

test_that("resource limits split otherwise independent modules", {
  registry <- new_analysis_registry()
  registry <- register_analysis(
    registry, "fst", result_module("fst"),
    resource_class = "heavy", parallel_safe = TRUE
  )
  registry <- register_analysis(
    registry, "amova", result_module("amova"),
    resource_class = "heavy", parallel_safe = TRUE
  )
  engine <- new_execution_engine(
    workers = 4L,
    resource_limits = c(light = 4L, standard = 2L, heavy = 1L, external = 1L)
  )
  plan <- plan_analysis_execution(registry, default_config())
  batches <- popgenVCF:::execution_batches(plan, registry, engine)

  expect_equal(batches, list("fst", "amova"))
})

test_that("parallel-safe modules cannot mutate shared context", {
  registry <- new_analysis_registry()
  mutating <- function(analysis, context) {
    analysis <- set_analysis_result(analysis, "mutating", TRUE)
    context$changed <- TRUE
    list(analysis = analysis, context = context)
  }
  registry <- register_analysis(
    registry, "mutating", mutating,
    parallel_safe = TRUE
  )
  registry <- register_analysis(
    registry, "peer", result_module("peer"),
    parallel_safe = TRUE
  )

  expect_error(
    execute_analysis_registry(
      make_engine_analysis(), list(), registry,
      engine = new_execution_engine(workers = 2L)
    ),
    "modified the shared execution context"
  )
})

test_that("legacy modules remain deterministic single-module batches", {
  registry <- new_analysis_registry()
  registry <- register_analysis(registry, "one", result_module("one"))
  registry <- register_analysis(registry, "two", result_module("two"))
  plan <- plan_analysis_execution(registry, default_config())
  batches <- popgenVCF:::execution_batches(
    plan, registry, new_execution_engine(workers = 8L)
  )

  expect_equal(batches, list("one", "two"))
  expect_false(any(plan$table$parallel_safe))
})
