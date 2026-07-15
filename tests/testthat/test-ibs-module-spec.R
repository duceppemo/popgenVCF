test_that("IBS module descriptor owns the complete registry contract", {
  spec <- popgenVCF::ibs_module_spec()

  expect_s3_class(spec, "PopgenVCFModuleSpec")
  expect_identical(spec$name, "ibs")
  expect_identical(spec$requires, character())
  expect_identical(spec$outputs, "ibs")
  expect_identical(spec$references, "Zheng et al. 2012")
  expect_identical(spec$resource_class, "heavy")
  expect_identical(spec$contract_version, "1.0")
  expect_identical(spec$artifacts, character())
  expect_false(spec$artifacts_must_exist)
  expect_identical(spec$run, popgenVCF:::run_module_ibs)
  expect_identical(spec$validate, popgenVCF:::validate_ibs_result)
})

test_that("built-in registry reflects the IBS module descriptor", {
  spec <- popgenVCF::ibs_module_spec()
  registry <- popgenVCF::default_analysis_registry()
  module <- registry$modules$ibs

  expect_identical(module$name, spec$name)
  expect_identical(module$requires, spec$requires)
  expect_identical(module$outputs, spec$outputs)
  expect_identical(module$references, spec$references)
  expect_identical(module$resource_class, spec$resource_class)
  expect_identical(module$contract_version, spec$contract_version)
  expect_identical(module$artifacts, spec$artifacts)
  expect_identical(module$artifacts_must_exist, spec$artifacts_must_exist)
  expect_identical(module$run, spec$run)
  expect_identical(module$validate, spec$validate)

  expect_identical(registry$modules$tree$requires, "ibs")
  expect_identical(registry$modules$ibd$requires, "ibs")
})
