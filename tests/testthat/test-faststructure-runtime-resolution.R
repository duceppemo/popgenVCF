test_that("fastStructure scripts are discovered in the supported install directory", {
  root <- tempfile("faststructure-home-")
  dir.create(root)
  script <- file.path(root, "structure.py")
  writeLines("print('fastStructure')", script)

  resolved <- popgenVCF:::resolve_faststructure_script(
    "structure.py",
    "structure.py",
    install_dir = root,
    locator = function(command) ""
  )

  expect_identical(resolved$path, normalizePath(script))
  expect_identical(resolved$source, "fastStructure install directory")
  expect_false(resolved$on_path)
})

test_that("Python scripts use the isolated Conda environment from the main runtime", {
  root <- tempfile("faststructure-launcher-")
  dir.create(root)
  script_path <- file.path(root, "structure.py")
  writeLines("print('fastStructure')", script_path)
  script <- list(path = script_path, source = "configured path", on_path = FALSE)

  launcher <- popgenVCF:::resolve_faststructure_launcher(
    script,
    env_name = "popgenvcf-faststructure",
    locator = function(command) {
      if (identical(command, "conda")) "/opt/conda/bin/conda" else ""
    },
    active_env = "popgenvcf",
    conda_executable = "",
    python_executable = ""
  )

  expect_identical(launcher$command, "/opt/conda/bin/conda")
  expect_identical(
    launcher$prefix_args,
    c(
      "run", "-n", "popgenvcf-faststructure", "python",
      normalizePath(script_path)
    )
  )
  expect_identical(launcher$mode, "Conda environment popgenvcf-faststructure")
})

test_that("PATH-installed fastStructure entry points remain directly executable", {
  root <- tempfile("faststructure-path-")
  dir.create(root)
  script_path <- file.path(root, "structure.py")
  writeLines("#!/usr/bin/env python\nprint('fastStructure')", script_path)

  launcher <- popgenVCF:::resolve_faststructure_launcher(
    list(path = script_path, source = "PATH", on_path = TRUE),
    env_name = "unused",
    locator = function(command) "",
    active_env = "",
    conda_executable = "",
    python_executable = ""
  )

  expect_identical(launcher$command, normalizePath(script_path))
  expect_length(launcher$prefix_args, 0L)
  expect_identical(launcher$mode, "direct")
})

test_that("missing fastStructure installations produce actionable diagnostics", {
  root <- tempfile("missing-faststructure-")
  dir.create(root)

  expect_error(
    popgenVCF:::resolve_faststructure_script(
      "structure.py",
      "structure.py",
      install_dir = root,
      locator = function(command) "",
      file_exists = function(path) FALSE
    ),
    "faststructure-environment.yml"
  )
})

test_that("the fastStructure runner reports backend failures and missing Q files", {
  body_text <- paste(
    deparse(body(popgenVCF::run_faststructure)),
    collapse = "\n"
  )

  expect_match(body_text, "resolve_faststructure_script", fixed = TRUE)
  expect_match(body_text, "resolve_faststructure_launcher", fixed = TRUE)
  expect_match(body_text, "fastStructure failed for K=", fixed = TRUE)
  expect_match(body_text, "did not create", fixed = TRUE)
})
