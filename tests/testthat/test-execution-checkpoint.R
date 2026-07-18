checkpoint_analysis <- function() {
  analysis <- new_popgen_vcf_analysis(default_config())
  analysis$samples$ids <- c("a", "b")
  analysis$samples$metadata <- data.table::data.table(
    sample = c("a", "b"), population = c("x", "y")
  )
  analysis$variants$qc_ids <- 1:2
  analysis$variants$ld_ids <- 1:2
  analysis
}

checkpoint_result_module <- function(name, counter = NULL) {
  force(name)
  force(counter)
  function(analysis, context) {
    if (!is.null(counter)) counter[[name]] <- (counter[[name]] %||% 0L) + 1L
    analysis <- set_analysis_result(analysis, name, list(module = name))
    list(analysis = analysis, context = context)
  }
}

checkpoint_registry <- function(fail_second = FALSE, counter = NULL) {
  registry <- new_analysis_registry()
  registry <- register_analysis(
    registry, "first", checkpoint_result_module("first", counter)
  )
  second <- if (fail_second) {
    function(analysis, context) stop("interrupted", call. = FALSE)
  } else {
    checkpoint_result_module("second", counter)
  }
  registry <- register_analysis(registry, "second", second, requires = "first")
  register_analysis(
    registry, "third", checkpoint_result_module("third", counter), requires = "second"
  )
}

test_that("execution checkpoints round trip with SHA-256 verification", {
  registry <- checkpoint_registry()
  executed <- execute_analysis_registry(checkpoint_analysis(), list(), registry)
  checkpoint <- new_execution_checkpoint(executed, registry)
  path <- tempfile(fileext = ".rds")

  write_execution_checkpoint(checkpoint, path)
  restored <- read_execution_checkpoint(path, registry)

  expect_s3_class(restored, "PopgenVCFExecutionCheckpoint")
  expect_equal(restored$completed, c("first", "second", "third"))
  expect_equal(restored$checkpoint_digest, checkpoint$checkpoint_digest)
  expect_error(write_execution_checkpoint(checkpoint, path), "already exists")
})

test_that("checkpoint file corruption is detected", {
  registry <- checkpoint_registry()
  checkpoint <- new_execution_checkpoint(
    execute_analysis_registry(checkpoint_analysis(), list(), registry), registry
  )
  path <- tempfile(fileext = ".rds")
  write_execution_checkpoint(checkpoint, path)
  writeBin(as.raw(c(1, 2, 3)), path)

  expect_error(read_execution_checkpoint(path, registry), "checksum mismatch")
})

test_that("resume reuses successful modules and reruns unfinished descendants", {
  failed_registry <- checkpoint_registry(fail_second = TRUE)
  interrupted <- execute_analysis_registry(
    checkpoint_analysis(), list(), failed_registry,
    engine = new_execution_engine(fail_fast = FALSE)
  )
  checkpoint <- new_execution_checkpoint(interrupted, failed_registry)
  expect_equal(checkpoint$completed, "first")

  counter <- new.env(parent = emptyenv())
  resumed <- resume_analysis_execution(
    checkpoint, checkpoint_registry(counter = counter),
    engine = new_execution_engine(fail_fast = FALSE)
  )

  expect_null(counter$first)
  expect_equal(counter$second, 1L)
  expect_equal(counter$third, 1L)
  expect_equal(resumed$order, c("first", "second", "third"))
  expect_equal(resumed$execution$status, rep("success", 3))
  expect_equal(resumed$execution$checkpoint_reused, c(TRUE, FALSE, FALSE))
  expect_true(resumed$engine$resumed_from_checkpoint)
  expect_equal(resumed$engine$reused_modules, "first")
})

test_that("complete checkpoints resume as deterministic no-ops", {
  counter <- new.env(parent = emptyenv())
  registry <- checkpoint_registry(counter = counter)
  checkpoint <- new_execution_checkpoint(
    execute_analysis_registry(checkpoint_analysis(), list(), registry), registry
  )
  counter$first <- counter$second <- counter$third <- 0L

  resumed <- resume_analysis_execution(checkpoint, registry)

  expect_equal(counter$first, 0L)
  expect_equal(counter$second, 0L)
  expect_equal(counter$third, 0L)
  expect_true(all(resumed$execution$checkpoint_reused))
  expect_equal(resumed$order, c("first", "second", "third"))
})

test_that("checkpoint validation rejects module contract drift", {
  registry <- checkpoint_registry()
  checkpoint <- new_execution_checkpoint(
    execute_analysis_registry(checkpoint_analysis(), list(), registry), registry
  )
  changed <- new_analysis_registry()
  changed <- register_analysis(changed, "first", checkpoint_result_module("first"))
  changed <- register_analysis(
    changed, "second", checkpoint_result_module("second"),
    requires = "first", contract_version = "2.0"
  )
  changed <- register_analysis(
    changed, "third", checkpoint_result_module("third"), requires = "second"
  )

  expect_error(
    validate_execution_checkpoint(checkpoint, changed),
    "incompatible with the current analysis plan"
  )
})
