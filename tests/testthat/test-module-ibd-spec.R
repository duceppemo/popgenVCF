test_that("IBD module descriptor owns the complete registry contract", {
  module <- popgenVCF::ibd_module_spec()

  expect_s3_class(module, "PopgenVCFModuleSpec")
  expect_identical(module$name, "ibd")
  expect_identical(module$requires, "ibs")
  expect_true(is.function(module$enabled))
  expect_identical(module$outputs, "ibd")
  expect_identical(module$references, c("Mantel 1967", "Rousset 1997"))
  expect_identical(module$resource_class, "standard")
  expect_identical(module$contract_version, "1.0")

  cfg <- list(analyses = list(mantel = FALSE, isolation_by_distance = FALSE))
  expect_false(module$enabled(cfg))
  cfg$analyses$mantel <- TRUE
  expect_true(module$enabled(cfg))
  cfg$analyses$mantel <- FALSE
  cfg$analyses$isolation_by_distance <- TRUE
  expect_true(module$enabled(cfg))
})

test_that("built-in registry reflects the IBD descriptor", {
  registry <- popgenVCF::default_analysis_registry()
  registered <- registry$modules$ibd
  module <- popgenVCF::ibd_module_spec()

  expect_identical(registered$requires, module$requires)
  expect_identical(registered$outputs, module$outputs)
  expect_identical(registered$references, module$references)
  expect_identical(registered$resource_class, module$resource_class)
  expect_identical(registered$contract_version, module$contract_version)
  expect_identical(registered$run, module$run)
  expect_identical(registered$validate, module$validate)
  expect_true(is.function(registered$enabled))
})
