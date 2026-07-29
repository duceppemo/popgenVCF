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

test_that("IBD geographic-data skips satisfy the declared output contract", {
  cfg <- popgenVCF::default_config()
  analysis <- popgenVCF::new_popgen_vcf_analysis(cfg)
  distance <- matrix(
    c(0, 0.2, 0.4, 0.2, 0, 0.3, 0.4, 0.3, 0),
    nrow = 3L,
    dimnames = list(c("s1", "s2", "s3"), c("s1", "s2", "s3"))
  )
  context <- list(
    cfg = cfg,
    dirs = list(tables = tempdir(), figures = tempdir()),
    ibs = list(distance = distance),
    metadata = data.table::data.table(
      sample = c("s1", "s2", "s3"),
      population = c("A", "A", "B")
    )
  )

  output <- popgenVCF:::run_module_ibd(analysis, context)

  expect_true("ibd" %in% names(output$analysis$results))
  expect_null(output$analysis$results[["ibd"]])

  execution <- list(name = "ibd", value = output, elapsed = 0)
  validated <- popgenVCF:::validate_engine_module_output(
    execution,
    analysis,
    context,
    popgenVCF::default_analysis_registry()
  )
  expect_true(validated$validation$valid)
  expect_match(
    validated$validation$warnings,
    "geographic data were unavailable"
  )
})
