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
