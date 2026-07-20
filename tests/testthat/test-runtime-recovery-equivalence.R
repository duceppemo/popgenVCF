recovery_checkpoint_analysis <- function() {
  analysis <- new_popgen_vcf_analysis(default_config())
  analysis$samples$ids <- c("a", "b")
  analysis$samples$metadata <- data.table::data.table(
    sample = c("a", "b"), population = c("x", "y")
  )
  analysis$variants$qc_ids <- 1:2
  analysis$variants$ld_ids <- 1:2
  analysis
}

recovery_result_module <- function(name) {
  force(name)
  function(analysis, context) {
    analysis <- set_analysis_result(analysis, name, list(module = name))
    list(analysis = analysis, context = context)
  }
}

recovery_checkpoint_registry <- function(fail_second = FALSE) {
  registry <- new_analysis_registry()
  registry <- register_analysis(
    registry, "first", recovery_result_module("first")
  )
  second <- if (fail_second) {
    function(analysis, context) stop("interrupted", call. = FALSE)
  } else {
    recovery_result_module("second")
  }
  registry <- register_analysis(registry, "second", second, requires = "first")
  register_analysis(
    registry, "third", recovery_result_module("third"), requires = "second"
  )
}

test_that("resumed execution is equivalent to uninterrupted execution", {
  reference_registry <- recovery_checkpoint_registry()
  reference <- execute_analysis_registry(
    recovery_checkpoint_analysis(), list(seed = 17L), reference_registry,
    engine = new_execution_engine(fail_fast = FALSE)
  )

  interrupted_registry <- recovery_checkpoint_registry(fail_second = TRUE)
  interrupted <- execute_analysis_registry(
    recovery_checkpoint_analysis(), list(seed = 17L), interrupted_registry,
    engine = new_execution_engine(fail_fast = FALSE)
  )
  checkpoint <- new_execution_checkpoint(interrupted, interrupted_registry)
  recovered <- resume_analysis_execution(
    checkpoint, recovery_checkpoint_registry(),
    engine = new_execution_engine(fail_fast = FALSE)
  )

  report <- verify_runtime_recovery_equivalence(reference, recovered)

  expect_s3_class(report, "PopgenVCFRuntimeRecoveryEquivalence")
  expect_true(report$verified)
  expect_length(report$recovery_fingerprint, 1L)
  expect_equal(
    report$recovery_fingerprint,
    verify_runtime_recovery_equivalence(reference, recovered)$recovery_fingerprint
  )
})

test_that("recovery-only bookkeeping does not create false divergence", {
  registry <- recovery_checkpoint_registry()
  reference <- execute_analysis_registry(recovery_checkpoint_analysis(), list(), registry)
  checkpoint <- new_execution_checkpoint(reference, registry)
  recovered <- resume_analysis_execution(checkpoint, registry)

  expect_true(all(recovered$execution$checkpoint_reused))
  expect_true(recovered$engine$resumed_from_checkpoint)
  expect_true(verify_runtime_recovery_equivalence(reference, recovered)$verified)
})

test_that("scientific result drift fails closed", {
  registry <- recovery_checkpoint_registry()
  reference <- execute_analysis_registry(recovery_checkpoint_analysis(), list(), registry)
  recovered <- reference
  recovered$analysis$results$second$module <- "tampered"

  expect_error(
    verify_runtime_recovery_equivalence(reference, recovered),
    "analysis"
  )
})

test_that("context and artifact drift fail closed", {
  registry <- recovery_checkpoint_registry()
  reference <- execute_analysis_registry(
    recovery_checkpoint_analysis(), list(seed = 1L), registry
  )

  changed_context <- reference
  changed_context$context$seed <- 2L
  expect_error(
    verify_runtime_recovery_equivalence(reference, changed_context),
    "context"
  )

  with_artifact <- reference
  with_artifact$artifacts <- register_artifact(
    with_artifact$artifacts,
    new_analysis_artifact("first", "result", "data", "result.tsv", "tsv")
  )
  expect_error(
    verify_runtime_recovery_equivalence(reference, with_artifact),
    "artifacts"
  )
})

test_that("module reordering, duplication, and nonterminal states are rejected", {
  registry <- recovery_checkpoint_registry()
  reference <- execute_analysis_registry(recovery_checkpoint_analysis(), list(), registry)

  reordered <- reference
  reordered$order <- rev(reordered$order)
  expect_error(
    verify_runtime_recovery_equivalence(reference, reordered),
    "plan order"
  )

  duplicated <- reference
  duplicated$execution$module[2] <- duplicated$execution$module[1]
  expect_error(
    verify_runtime_recovery_equivalence(reference, duplicated),
    "uniquely match"
  )

  nonterminal <- reference
  nonterminal$execution$status[2] <- "running"
  expect_error(
    verify_runtime_recovery_equivalence(reference, nonterminal),
    "terminal execution states"
  )
})

test_that("terminal outcome divergence is reported", {
  registry <- recovery_checkpoint_registry()
  reference <- execute_analysis_registry(recovery_checkpoint_analysis(), list(), registry)
  recovered <- reference
  recovered$execution$status[2] <- "failed"

  expect_error(
    verify_runtime_recovery_equivalence(reference, recovered),
    "execution"
  )
})