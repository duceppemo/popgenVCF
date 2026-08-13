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
  expect_match(body_text, "choose_k_votes", fixed = TRUE)
  expect_match(body_text, "marginal_likelihood", fixed = TRUE)
  expect_false(grepl("popgenvcf-faststructure", body_text, fixed = TRUE))
})

test_that("marginal likelihood is parsed from fastStructure's native per-K log", {
  lines <- c(
    "Marginal likelihood with initialization (1) = -0.7844580010",
    "Iteration Marginal_Likelihood delta_Marginal_Likelihood Iteration_Time (secs)",
    "0 -0.7844525403 -- 122.254",
    "300 -0.7828180622 0.0000007100 6.208",
    "Marginal Likelihood = -0.7828180622",
    "Total time = 324.6668 seconds",
    "Total iterations = 300"
  )
  expect_equal(
    popgenVCF:::parse_faststructure_marginal_likelihood(lines),
    -0.7828180622
  )
  expect_true(is.na(popgenVCF:::parse_faststructure_marginal_likelihood(character())))
  expect_true(is.na(popgenVCF:::parse_faststructure_marginal_likelihood("no such line here")))
})

test_that("run_faststructure() kills a hung structure.py process instead of blocking forever", {
  skip_on_cran()
  skip_on_os("windows")
  root <- tempfile("faststructure-hang-")
  dir.create(root)
  prefix <- file.path(root, "cohort")
  file.create(paste0(prefix, c(".bed", ".bim", ".fam")))

  structure_executable <- file.path(root, "fake-structure-hang.py")
  writeLines(c("#!/bin/sh", "echo starting", "sleep 5"), structure_executable)
  Sys.chmod(structure_executable, mode = "0755")
  choosek_executable <- file.path(root, "fake-choosek")
  writeLines(c("#!/bin/sh", "echo unused"), choosek_executable)
  Sys.chmod(choosek_executable, mode = "0755")

  output_dir <- file.path(root, "output")
  elapsed <- system.time(
    expect_error(
      popgenVCF::run_faststructure(
        structure_executable = structure_executable,
        choosek_executable = choosek_executable,
        plink_prefix = prefix,
        k_values = 2L,
        output_dir = output_dir,
        timeout_seconds = 1
      ),
      "fastStructure failed for K=2.*exceeded timeout"
    )
  )
  expect_lt(elapsed[["elapsed"]], 4)
  expect_true(file.exists(file.path(output_dir, "fastStructure_K2.log")))
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
