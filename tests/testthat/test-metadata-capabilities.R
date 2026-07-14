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

  spatial <- data.table::data.table(
    sample = c("s1", "s2"), population = c("A", "B"),
    latitude = c(45.4, 45.5), longitude = c(-75.7, -73.6)
  )
  full <- popgenVCF:::metadata_capabilities(spatial, TRUE)
  expect_true(full$population)
  expect_true(full$coordinates)

  none <- popgenVCF:::metadata_capabilities(sample_only, FALSE)
  expect_false(none$metadata_supplied)
})

test_that("capability table disables metadata-dependent analyses", {
  runner <- function(analysis, context) list(analysis = analysis, context = context)
  registry <- popgenVCF::new_analysis_registry()
  for (name in c("pca", "ibs", "fst", "dapc", "mantel", "isolation_by_distance")) {
    registry <- popgenVCF::register_analysis(registry, name, runner)
  }

  no_metadata <- popgenVCF:::metadata_capabilities(data.table::data.table(sample = c("s1", "s2")), FALSE)
  tab <- popgenVCF:::analysis_capability_table(registry, no_metadata)
  expect_false(any(tab$available))

  sample_only <- popgenVCF:::metadata_capabilities(data.table::data.table(sample = c("s1", "s2")), TRUE)
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
