metadata_fixture_path <- function(lines) {
  path <- tempfile(fileext = ".tsv")
  writeLines(lines, path)
  path
}

test_that("normalize_metadata_name lowercases before collapsing non-alphanumerics, not after", {
  # A real, previously-undiscovered bug found writing a regression test for
  # the population_column feature: applying gsub() (whose character class is
  # deliberately lowercase-only) before tolower() means any uppercase letter
  # -- not just ones adjacent to punctuation -- fails to match and gets
  # replaced with its own "_", silently corrupting any ordinarily
  # capitalized header ("Population" -> "_opulation", not "population")
  # and breaking the built-in sample/population synonym auto-detection.
  expect_identical(popgenVCF:::normalize_metadata_name("Population"), "population")
  expect_identical(popgenVCF:::normalize_metadata_name("Sample-ID"), "sample_id")
  expect_identical(popgenVCF:::normalize_metadata_name("Patho-Type"), "patho_type")
})

test_that("a literal, ordinarily capitalized 'Population' header is still auto-detected", {
  path <- metadata_fixture_path(c(
    "Sample\tPopulation",
    "s1\tnorth",
    "s2\tsouth"
  ))
  x <- popgenVCF:::read_metadata(path)
  expect_identical(x$sample, c("s1", "s2"))
  expect_identical(x$population, c("north", "south"))
})

test_that("population_column treats a differently named column as population without renaming it", {
  path <- metadata_fixture_path(c(
    "sample\tpathotype",
    "s1\tvirulent",
    "s2\tavirulent"
  ))
  x <- popgenVCF:::read_metadata(path, population_column = "pathotype")
  expect_identical(x$population, c("virulent", "avirulent"))
  expect_false("pathotype" %in% names(x))
})

test_that("sample_column treats a differently named column as sample without renaming it", {
  path <- metadata_fixture_path(c(
    "isolate\tpopulation",
    "s1\tnorth",
    "s2\tsouth"
  ))
  x <- popgenVCF:::read_metadata(path, sample_column = "isolate")
  expect_identical(x$sample, c("s1", "s2"))
  expect_false("isolate" %in% names(x))
})

test_that("population_column is matched case- and punctuation-insensitively", {
  path <- metadata_fixture_path(c(
    "sample\tPatho-Type",
    "s1\tA",
    "s2\tB"
  ))
  # "Patho-Type" normalizes to "patho_type" (non-alphanumeric runs become a
  # single "_", not stripped); config values normalize the same way, so any
  # of these equivalent spellings match the real header.
  for (requested in c("Patho-Type", "patho_type", "PATHO_TYPE", "patho type")) {
    x <- popgenVCF:::read_metadata(path, population_column = requested)
    expect_identical(x$population, c("A", "B"), info = requested)
  }
})

test_that("an explicit population_column bypasses auto-detection of a synonym column entirely", {
  # "pop" would normally be auto-detected as population; explicitly setting
  # population_column skips that auto-detection path altogether, so "pop"
  # is left alone as an ordinary annotation column instead of being renamed.
  path <- metadata_fixture_path(c(
    "sample\tpop\tpathotype",
    "s1\teast\tvirulent",
    "s2\twest\tavirulent"
  ))
  x <- popgenVCF:::read_metadata(path, population_column = "pathotype")
  expect_identical(x$population, c("virulent", "avirulent"))
  expect_identical(x$pop, c("east", "west"))
})

test_that("population_column errors clearly when the named column does not exist", {
  path <- metadata_fixture_path(c("sample\tpopulation", "s1\tnorth"))
  expect_error(
    popgenVCF:::read_metadata(path, population_column = "pathotype"),
    "not found in metadata columns"
  )
})

test_that("population_column errors when it would collide with an existing different population column", {
  path <- metadata_fixture_path(c(
    "sample\tpopulation\tlineage",
    "s1\tnorth\tA"
  ))
  # population_column names a column other than the literal "population"
  # column that is also present under its own name -- ambiguous, so this
  # must fail loudly rather than silently duplicating a column name.
  expect_error(
    popgenVCF:::read_metadata(path, population_column = "lineage"),
    "conflicts with an existing"
  )
})

test_that("sample_column/population_column require a headered metadata file", {
  path <- metadata_fixture_path(c("s1\tnorth", "s2\tsouth"))
  expect_error(
    popgenVCF:::read_metadata(path, header = "no", population_column = "pathotype"),
    "require headered metadata"
  )
})
