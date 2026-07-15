test_that("module descriptors register complete contracts", {
  runner <- function(analysis, context) list(analysis = analysis, context = context)
  validator <- function(result, analysis, context) list(valid = TRUE, errors = character(), warnings = character(), metrics = list())

  module <- popgenVCF::new_analysis_module_spec(
    name = "example",
    run = runner,
    requires = "qc",
    description = "Example module",
    validate = validator,
    outputs = c("example", "example_diagnostics"),
    references = "Example 2026",
    resource_class = "heavy",
    artifacts = c("table", "figure"),
    artifacts_must_exist = TRUE
  )

  expect_s3_class(module, "PopgenVCFModuleSpec")
  expect_equal(module$name, "example")
  expect_setequal(module$artifacts, c("table", "figure"))

  registry <- popgenVCF::new_analysis_registry()
  registry <- popgenVCF::register_analysis(
    registry, "qc", runner,
    validate = validator,
    outputs = "qc"
  )
  registry <- popgenVCF::register_analysis_module(registry, module)

  registered <- registry$modules$example
  expect_equal(registered$requires, "qc")
  expect_setequal(registered$outputs, c("example", "example_diagnostics"))
  expect_setequal(registered$artifacts, c("table", "figure"))
  expect_true(registered$artifacts_must_exist)
})

test_that("module descriptors reject invalid contracts", {
  runner <- function(analysis, context) list(analysis = analysis, context = context)

  expect_error(
    popgenVCF::new_analysis_module_spec("", runner),
    "non-empty"
  )
  expect_error(
    popgenVCF::new_analysis_module_spec("bad", runner, requires = "bad"),
    "require itself"
  )
  expect_error(
    popgenVCF::new_analysis_module_spec("bad", runner, artifacts = ""),
    "non-empty"
  )
  expect_error(
    popgenVCF::register_analysis_module(popgenVCF::new_analysis_registry(), list()),
    "PopgenVCFModuleSpec"
  )
})
