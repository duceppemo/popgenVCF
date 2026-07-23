test_that("fastStructure commands are discovered on PATH", {
  root <- tempfile("faststructure-path-")
  dir.create(root)
  script <- file.path(root, "structure.py")
  writeLines("#!/usr/bin/env python\nprint('fastStructure')", script)

  resolved <- popgenVCF:::resolve_faststructure_executable(
    "structure.py",
    "structure.py",
    locator = function(command) {
      if (identical(command, "structure.py")) script else ""
    }
  )

  expect_identical(resolved, normalizePath(script))
})

test_that("configured absolute fastStructure paths remain supported", {
  root <- tempfile("faststructure-absolute-")
  dir.create(root)
  script <- file.path(root, "custom-structure.py")
  writeLines("#!/usr/bin/env python\nprint('fastStructure')", script)

  resolved <- popgenVCF:::resolve_faststructure_executable(
    script,
    "structure.py",
    locator = function(command) ""
  )

  expect_identical(resolved, normalizePath(script))
})

test_that("missing fastStructure commands produce actionable diagnostics", {
  expect_error(
    popgenVCF:::resolve_faststructure_executable(
      "structure.py",
      "structure.py",
      locator = function(command) "",
      file_exists = function(path) FALSE
    ),
    "mamba install bioconda::faststructure",
    fixed = TRUE
  )
})

test_that("the fastStructure runner reports backend failures and missing Q files", {
  body_text <- paste(
    deparse(body(popgenVCF::run_faststructure)),
    collapse = "\n"
  )

  expect_match(body_text, "resolve_faststructure_executable", fixed = TRUE)
  expect_match(body_text, "fastStructure failed for K=", fixed = TRUE)
  expect_match(body_text, "did not create", fixed = TRUE)
  expect_false(grepl("popgenvcf-faststructure", body_text, fixed = TRUE))
})

test_that("the primary Conda environment declares fastStructure", {
  environment_file <- system.file(
    "conda", "environment.yml",
    package = "popgenVCF"
  )
  if (!nzchar(environment_file)) {
    environment_file <- testthat::test_path(
      "..", "..", "inst", "conda", "environment.yml"
    )
  }

  expect_true(file.exists(environment_file))
  environment_text <- readLines(environment_file, warn = FALSE)
  expect_true(any(grepl(
    "^[[:space:]]*-[[:space:]]+faststructure[[:space:]]*$",
    environment_text
  )))
})
