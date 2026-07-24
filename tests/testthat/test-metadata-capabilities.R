test_that("configuration permits VCF-only mode", {
  vcf <- tempfile(fileext = ".vcf")
  writeLines("##fileformat=VCFv4.2", vcf)
  cfg <- popgenVCF::default_config()
  cfg$input$vcf <- vcf
  cfg$input$metadata <- NULL
  cfg$output$directory <- tempfile("out-")
  validated <- popgenVCF::validate_config(cfg)
  expect_null(validated$input$metadata)
})

test_that("metadata requires only a sample column", {
  path <- tempfile(fileext = ".tsv")
  writeLines(c("sample\tcollection_site", "s1\tOttawa", "s2\tMontreal"), path)
  metadata <- popgenVCF:::read_metadata(path)
  expect_equal(metadata$sample, c("s1", "s2"))
  expect_false("population" %in% names(metadata))
  expect_true("collection_site" %in% names(metadata))
})

test_that("metadata sample IDs must exactly match VCF sample IDs", {
  metadata <- data.table::data.table(
    sample = c("s2", "s1"),
    population = c("B", "A")
  )
  aligned <- popgenVCF:::validate_metadata_sample_ids(metadata, c("s1", "s2"))
  expect_equal(aligned$sample, c("s1", "s2"))
  expect_equal(aligned$population, c("A", "B"))

  expect_error(
    popgenVCF:::validate_metadata_sample_ids(
      data.table::data.table(sample = c("s1", "unknown")),
      c("s1", "s2")
    ),
    "metadata IDs absent from VCF"
  )
  expect_error(
    popgenVCF:::validate_metadata_sample_ids(
      data.table::data.table(sample = "s1"),
      c("s1", "s2")
    ),
    "VCF samples absent from metadata"
  )
})

test_that("metadata capabilities distinguish workflow modes", {
  sample_only <- data.table::data.table(sample = c("s1", "s2"))
  basic <- popgenVCF:::metadata_capabilities(sample_only, TRUE)
  expect_true(basic$metadata_supplied)
  expect_false(basic$population)
  expect_false(basic$coordinates)

  population <- data.table::data.table(sample = c("s1", "s2"), population = c("A", "B"))
  grouped <- popgenVCF:::metadata_capabilities(population, TRUE)
  expect_true(grouped$population)
  expect_false(grouped$coordinates)

  incomplete_population <- data.table::data.table(
    sample = c("s1", "s2"), population = c("A", NA_character_)
  )
  expect_false(popgenVCF:::metadata_capabilities(incomplete_population, TRUE)$population)

  spatial <- data.table::data.table(
    sample = c("s1", "s2"), population = c("A", "B"),
    latitude = c(45.4, 45.5), longitude = c(-75.7, -73.6)
  )
  full <- popgenVCF:::metadata_capabilities(spatial, TRUE)
  expect_true(full$population)
  expect_true(full$coordinates)

  incomplete_spatial <- data.table::copy(spatial)
  incomplete_spatial[2, latitude := NA_real_]
  expect_true(popgenVCF:::metadata_capabilities(incomplete_spatial, TRUE)$coordinates)

  no_usable_coordinates <- data.table::copy(spatial)
  no_usable_coordinates[, `:=`(latitude = NA_real_, longitude = NA_real_)]
  expect_false(popgenVCF:::metadata_capabilities(no_usable_coordinates, TRUE)$coordinates)

  none <- popgenVCF:::metadata_capabilities(sample_only, FALSE)
  expect_false(none$metadata_supplied)
})

test_that("capability table keeps sample analyses available without metadata", {
  runner <- function(analysis, context) list(analysis = analysis, context = context)
  registry <- popgenVCF::new_analysis_registry()
  for (name in c("pca", "ibs", "fst", "dapc", "mantel", "isolation_by_distance")) {
    registry <- popgenVCF::register_analysis(registry, name, runner)
  }

  no_metadata <- popgenVCF:::metadata_capabilities(
    data.table::data.table(sample = c("s1", "s2")), FALSE
  )
  tab <- popgenVCF:::analysis_capability_table(registry, no_metadata)
  expect_true(all(tab[module %in% c("pca", "ibs"), available]))
  expect_true(all(tab[module %in% c("pca", "ibs"), reason] == "available from VCF sample IDs"))
  expect_false(any(tab[module %in% c("fst", "dapc", "mantel", "isolation_by_distance"), available]))

  sample_only <- popgenVCF:::metadata_capabilities(
    data.table::data.table(sample = c("s1", "s2")), TRUE
  )
  tab <- popgenVCF:::analysis_capability_table(registry, sample_only)
  expect_true(all(tab[module %in% c("pca", "ibs"), available]))
  expect_false(any(tab[module %in% c("fst", "dapc", "mantel", "isolation_by_distance"), available]))

  grouped <- popgenVCF:::metadata_capabilities(
    data.table::data.table(sample = c("s1", "s2"), population = c("A", "B")), TRUE
  )
  tab <- popgenVCF:::analysis_capability_table(registry, grouped)
  expect_true(all(tab[module %in% c("pca", "ibs", "fst", "dapc"), available]))
  expect_false(any(tab[module %in% c("mantel", "isolation_by_distance"), available]))
})

test_that("pipeline module resolution honors configured enablement", {
  runner <- function(analysis, context) list(analysis = analysis, context = context)
  registry <- popgenVCF::new_analysis_registry()
  registry <- popgenVCF::register_analysis(registry, "pca", runner)
  registry <- popgenVCF::register_analysis(
    registry, "admixture", runner,
    enabled = function(cfg) isTRUE(cfg$analyses$admixture$enabled)
  )
  registry <- popgenVCF::register_analysis(
    registry, "faststructure", runner,
    enabled = function(cfg) isTRUE(cfg$analyses$faststructure$enabled)
  )
  registry <- popgenVCF::register_analysis(
    registry, "snmf", runner,
    enabled = function(cfg) isTRUE(cfg$analyses$snmf$enabled)
  )

  capabilities <- popgenVCF:::metadata_capabilities(
    data.table::data.table(sample = c("s1", "s2"), population = c("A", "B")),
    metadata_supplied = TRUE
  )
  cfg <- popgenVCF::default_config()

  expect_identical(
    popgenVCF:::resolve_pipeline_modules(registry, capabilities, cfg),
    "pca"
  )

  cfg$analyses$snmf$enabled <- TRUE
  expect_identical(
    popgenVCF:::resolve_pipeline_modules(registry, capabilities, cfg),
    c("pca", "snmf")
  )

  cfg$analyses$snmf$enabled <- FALSE
  expect_identical(
    popgenVCF:::resolve_pipeline_modules(
      registry, capabilities, cfg, selected = c("snmf", "pca")
    ),
    c("snmf", "pca")
  )

  pipeline_body <- paste(deparse(body(popgenVCF::run_pipeline)), collapse = "\n")
  expect_match(
    pipeline_body,
    "resolve_pipeline_modules\\s*\\(registry,\\s*capabilities,\\s*cfg,\\s*selected\\)"
  )
})
