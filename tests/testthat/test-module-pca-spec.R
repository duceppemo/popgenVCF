test_that("PCA module descriptor owns the complete registry contract", {
  spec <- popgenVCF::pca_module_spec()

  expect_s3_class(spec, "PopgenVCFModuleSpec")
  expect_identical(spec$name, "pca")
  expect_identical(spec$outputs, "pca")
  expect_identical(spec$resource_class, "heavy")
  expect_identical(spec$references, "Patterson et al. 2006")
  expect_true(spec$artifacts_must_exist)
  expect_setequal(
    spec$artifacts,
    c(
      "coordinates", "variance", "pc1_pc2_pdf", "pc1_pc2_svg",
      "pc1_pc2_png", "methods", "caption", "validation", "figure_source"
    )
  )
})

test_that("default registry registers PCA through its module descriptor", {
  registry <- popgenVCF::default_analysis_registry()
  pca <- registry$modules$pca
  spec <- popgenVCF::pca_module_spec()

  expect_identical(pca$name, spec$name)
  expect_identical(pca$outputs, spec$outputs)
  expect_identical(pca$references, spec$references)
  expect_identical(pca$resource_class, spec$resource_class)
  expect_identical(pca$contract_version, spec$contract_version)
  expect_identical(pca$artifacts, spec$artifacts)
  expect_identical(pca$artifacts_must_exist, spec$artifacts_must_exist)
})
