test_that("remaining module descriptors preserve their registry contracts", {
  cases <- list(
    diversity = list(
      spec = diversity_module_spec(), requires = character(),
      outputs = c("diversity", "diversity_ci"), references = "Nei 1987",
      resource = "heavy"
    ),
    tree = list(
      spec = tree_module_spec(), requires = "ibs", outputs = "tree",
      references = "Saitou and Nei 1987", resource = "standard"
    ),
    admixture = list(
      spec = admixture_module_spec(), requires = character(),
      outputs = "admixture_cv", references = "Alexander et al. 2009",
      resource = "external"
    ),
    faststructure = list(
      spec = faststructure_module_spec(), requires = character(),
      outputs = "faststructure", references = "Raj et al. 2014",
      resource = "external"
    ),
    snmf = list(
      spec = snmf_module_spec(), requires = character(), outputs = "snmf",
      references = "Frichot et al. 2014", resource = "external"
    ),
    chromosome = list(
      spec = chromosome_module_spec(), requires = character(),
      outputs = "chromosome_summary", references = character(),
      resource = "heavy"
    )
  )

  registry <- default_analysis_registry()

  for (name in names(cases)) {
    case <- cases[[name]]
    spec <- case$spec
    registered <- registry$modules[[name]]

    expect_s3_class(spec, "PopgenVCFModuleSpec")
    expect_identical(spec$name, name)
    expect_identical(spec$requires, case$requires)
    expect_identical(spec$outputs, case$outputs)
    expect_identical(spec$references, case$references)
    expect_identical(spec$resource_class, case$resource)
    expect_identical(spec$contract_version, "1.0")

    expect_identical(registered$requires, spec$requires)
    expect_identical(registered$outputs, spec$outputs)
    expect_identical(registered$references, spec$references)
    expect_identical(registered$resource_class, spec$resource_class)
    expect_identical(registered$contract_version, spec$contract_version)
    expect_identical(registered$run, spec$run)
    expect_identical(registered$validate, spec$validate)
  }
})

test_that("external and chromosome module enablement is unchanged", {
  cfg <- default_config()

  cfg$analyses$admixture$enabled <- FALSE
  expect_false(admixture_module_spec()$enabled(cfg))
  cfg$analyses$admixture$enabled <- TRUE
  expect_true(admixture_module_spec()$enabled(cfg))

  cfg$analyses$faststructure$enabled <- FALSE
  expect_false(faststructure_module_spec()$enabled(cfg))
  cfg$analyses$faststructure$enabled <- TRUE
  expect_true(faststructure_module_spec()$enabled(cfg))

  cfg$analyses$snmf$enabled <- FALSE
  expect_false(snmf_module_spec()$enabled(cfg))
  cfg$analyses$snmf$enabled <- TRUE
  expect_true(snmf_module_spec()$enabled(cfg))

  cfg$analyses$chromosome_specific <- FALSE
  expect_false(chromosome_module_spec()$enabled(cfg))
  cfg$analyses$chromosome_specific <- TRUE
  expect_true(chromosome_module_spec()$enabled(cfg))
})

test_that("the default registry is fully descriptor-driven", {
  registry <- default_analysis_registry()
  expected <- c(
    "diversity", "pca", "ibs", "tree", "fst", "dapc", "amova", "ibd",
    "admixture", "faststructure", "snmf", "chromosome"
  )

  expect_setequal(names(registry$modules), expected)
  versions <- vapply(registry$modules, `[[`, character(1), "contract_version")
  expect_true(all(versions == "1.0"))
})
