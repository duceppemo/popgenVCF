test_that("DAPC module descriptor owns the complete registry contract", {
  module <- popgenVCF::dapc_module_spec()

  expect_s3_class(module, "PopgenVCFModuleSpec")
  expect_identical(module$name, "dapc")
  expect_identical(module$requires, "diversity")
  expect_true(is.function(module$enabled))
  expect_identical(module$outputs, "dapc")
  expect_identical(module$references, "Jombart et al. 2010")
  expect_identical(module$resource_class, "heavy")
  expect_identical(module$contract_version, "1.0")

  cfg <- list(analyses = list(dapc = TRUE))
  expect_true(module$enabled(cfg))
  cfg$analyses$dapc <- FALSE
  expect_false(module$enabled(cfg))
})

test_that("built-in registry reflects the DAPC descriptor", {
  registry <- popgenVCF::default_analysis_registry()
  registered <- registry$modules$dapc
  module <- popgenVCF::dapc_module_spec()

  expect_identical(registered$requires, module$requires)
  expect_identical(registered$outputs, module$outputs)
  expect_identical(registered$references, module$references)
  expect_identical(registered$resource_class, module$resource_class)
  expect_identical(registered$contract_version, module$contract_version)
  expect_true(is.function(registered$enabled))
})
