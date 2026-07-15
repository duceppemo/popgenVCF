test_that("AMOVA module descriptor owns the complete registry contract", {
  spec <- amova_module_spec()

  expect_s3_class(spec, "PopgenVCFModuleSpec")
  expect_identical(spec$name, "amova")
  expect_identical(spec$requires, "diversity")
  expect_identical(spec$outputs, "amova")
  expect_identical(spec$references, "Excoffier et al. 1992")
  expect_identical(spec$resource_class, "heavy")
  expect_identical(spec$contract_version, "1.0")

  cfg <- default_config()
  cfg$analyses$amova <- FALSE
  expect_false(spec$enabled(cfg))
  cfg$analyses$amova <- TRUE
  expect_true(spec$enabled(cfg))
})

test_that("built-in registry reflects the AMOVA descriptor", {
  spec <- amova_module_spec()
  registry <- default_analysis_registry()
  entry <- registry$modules$amova

  expect_identical(entry$requires, spec$requires)
  expect_identical(entry$outputs, spec$outputs)
  expect_identical(entry$references, spec$references)
  expect_identical(entry$resource_class, spec$resource_class)
  expect_identical(entry$contract_version, spec$contract_version)
  expect_identical(entry$run, spec$run)
  expect_identical(entry$validate, spec$validate)
})
