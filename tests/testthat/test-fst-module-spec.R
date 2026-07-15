test_that("FST module descriptor owns the complete registry contract", {
  spec <- popgenVCF::fst_module_spec()

  expect_s3_class(spec, "PopgenVCFModuleSpec")
  expect_identical(spec$name, "fst")
  expect_identical(spec$run, popgenVCF:::run_module_fst)
  expect_identical(spec$validate, popgenVCF:::validate_fst_result)
  expect_identical(spec$outputs, c("fst", "fst_ci"))
  expect_identical(spec$references, "Weir and Cockerham 1984")
  expect_identical(spec$resource_class, "heavy")
  expect_identical(spec$contract_version, "1.0")
  expect_length(spec$requires, 0L)
  expect_length(spec$artifacts, 0L)
})

test_that("default registry reflects the FST module descriptor", {
  spec <- popgenVCF::fst_module_spec()
  registry <- popgenVCF::default_analysis_registry()
  module <- registry$modules$fst

  expect_identical(module$run, spec$run)
  expect_identical(module$requires, spec$requires)
  expect_identical(module$validate, spec$validate)
  expect_identical(module$outputs, spec$outputs)
  expect_identical(module$references, spec$references)
  expect_identical(module$resource_class, spec$resource_class)
  expect_identical(module$contract_version, spec$contract_version)
})
