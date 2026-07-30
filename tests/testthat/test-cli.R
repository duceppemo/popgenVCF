test_that("CLI recognizes documented options", {
  x <- popgenVCF:::parse_cli(c(
    "--config", "analysis.yml",
    "--vcf", "cohort.vcf.gz",
    "--metadata", "metadata.tsv",
    "--outdir", "results",
    "--threads", "8",
    "--seed", "42",
    "--maf", "0.05",
    "--max-sample-missing", "0.2",
    "--force-gds",
    "--no-report"
  ))
  expect_equal(x$config, "analysis.yml")
  expect_equal(x$vcf, "cohort.vcf.gz")
  expect_true(x$force_gds)
  expect_true(x$no_report)
})

test_that("CLI rejects unknown options", {
  expect_error(popgenVCF:::parse_cli(c("--not-real", "x")), "Unknown or incomplete")
})

test_that("parse_cli returns all documented defaults for an empty argument vector", {
  x <- popgenVCF:::parse_cli(character())
  expect_identical(x, list(
    config = NULL, write_config = NULL,
    force_gds = FALSE, no_report = FALSE, version = FALSE
  ))
})

test_that("parse_cli recognizes --version and --write-config independently", {
  x <- popgenVCF:::parse_cli(c("--version"))
  expect_true(x$version)
  expect_null(x$config)

  x <- popgenVCF:::parse_cli(c("--write-config", "template.yml"))
  expect_equal(x$write_config, "template.yml")
})

test_that("parse_cli rejects a value option with a missing trailing value", {
  expect_error(
    popgenVCF:::parse_cli(c("--config", "analysis.yml", "--vcf")),
    "Unknown or incomplete"
  )
})

# Bootstraps a fresh Rscript subprocess so it can access popgenVCF regardless
# of how *this* test process reached it: getwd() is not the source root under
# every test runner (a plain `R CMD check` on a built tarball, and covr's own
# temp-library install, both run tests from directories unrelated to the
# checkout). In dev mode (pkgload::load_all()/devtools::test()), reuse the
# exact source path already recorded on the loaded namespace; otherwise
# popgenVCF is a normally installed package, so just point the subprocess's
# library search at every path this session is already using.
popgenvcf_subprocess_bootstrap <- function() {
  is_dev <- isTRUE(tryCatch(pkgload::is_dev_package("popgenVCF"), error = function(e) FALSE))
  if (is_dev) {
    path <- getNamespaceInfo(asNamespace("popgenVCF"), "path")
    list(expr = sprintf("pkgload::load_all(%s, quiet = TRUE)", shQuote(path)), libs = NULL)
  } else {
    list(expr = "suppressPackageStartupMessages(library(popgenVCF))", libs = .libPaths())
  }
}

popgenvcf_run_rscript <- function(expr) {
  boot <- popgenvcf_subprocess_bootstrap()
  env <- Sys.getenv()
  if (!is.null(boot$libs)) {
    env["R_LIBS"] <- paste(boot$libs, collapse = .Platform$path.sep)
  }
  processx::run(
    command = file.path(R.home("bin"), "Rscript"),
    args = c("-e", paste0(boot$expr, "; ", expr)),
    env = env,
    error_on_status = FALSE
  )
}

test_that("cli_usage prints documented usage and exits with the requested status", {
  for (status in c(0L, 1L)) {
    res <- popgenvcf_run_rscript(sprintf("popgenVCF:::cli_usage(%dL)", status))
    expect_equal(res$status, status)
    expect_match(res$stdout, "popgenVCF population genomics toolkit", fixed = TRUE)
    expect_match(res$stdout, "--write-config analysis.yml", fixed = TRUE)
  }
})

test_that("parse_cli's --help routes through cli_usage and exits 0", {
  res <- popgenvcf_run_rscript("popgenVCF:::parse_cli('--help')")
  expect_equal(res$status, 0L)
  expect_match(res$stdout, "Usage:", fixed = TRUE)
})

test_that("cli_main with no config or write-config routes through cli_usage and exits 1", {
  res <- popgenvcf_run_rscript("popgenVCF::cli_main(character())")
  expect_equal(res$status, 1L)
  expect_match(res$stdout, "Usage:", fixed = TRUE)
})

test_that("write_default_config writes a loadable template and refuses to overwrite", {
  path <- withr::local_tempfile(fileext = ".yml")

  result <- popgenVCF:::write_default_config(path)

  expect_true(file.exists(path))
  expect_equal(normalizePath(result), normalizePath(path))
  written <- yaml::read_yaml(path)
  expect_equal(written, popgenVCF:::template_config())

  expect_error(
    popgenVCF:::write_default_config(path),
    "Refusing to overwrite existing file"
  )
})

test_that("write_default_config creates missing parent directories", {
  root <- withr::local_tempdir()
  path <- file.path(root, "nested", "deeper", "analysis.yml")

  popgenVCF:::write_default_config(path)

  expect_true(file.exists(path))
})

test_that("cli_main prints the package version and does not run the pipeline", {
  ran <- FALSE
  local_mocked_bindings(
    run_pipeline = function(...) { ran <<- TRUE },
    .package = "popgenVCF"
  )

  out <- withr::with_output_sink(
    tempfile(),
    popgenVCF::cli_main(c("--version"))
  )

  expect_null(out)
  expect_false(ran)
})

test_that("cli_main --write-config writes the template without running the pipeline", {
  ran <- FALSE
  local_mocked_bindings(
    run_pipeline = function(...) { ran <<- TRUE },
    .package = "popgenVCF"
  )
  path <- withr::local_tempfile(fileext = ".yml")

  result <- popgenVCF::cli_main(c("--write-config", path))

  expect_true(file.exists(path))
  expect_equal(normalizePath(result), normalizePath(path))
  expect_false(ran)
})

test_that("cli_main applies every documented override to the loaded configuration", {
  captured <- NULL
  local_mocked_bindings(
    run_pipeline = function(cfg, ...) { captured <<- cfg; "ran" },
    .package = "popgenVCF"
  )
  path <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(
    list(input = list(vcf = "orig.vcf"), output = list(directory = "orig_out")),
    path
  )

  result <- popgenVCF::cli_main(c(
    "--config", path,
    "--vcf", "override.vcf",
    "--metadata", "override_metadata.tsv",
    "--outdir", "override_out",
    "--threads", "4",
    "--seed", "7",
    "--maf", "0.1",
    "--max-sample-missing", "0.3",
    "--force-gds",
    "--no-report"
  ))

  expect_equal(result, "ran")
  expect_equal(captured$input$vcf, "override.vcf")
  expect_equal(captured$input$metadata, "override_metadata.tsv")
  expect_equal(captured$output$directory, "override_out")
  expect_identical(captured$compute$threads, 4L)
  expect_identical(captured$compute$seed, 7L)
  expect_equal(captured$qc$maf, 0.1)
  expect_equal(captured$qc$max_sample_missing, 0.3)
  expect_true(captured$compute$force_gds)
  expect_false(captured$report$enabled)
})

test_that("cli_main leaves unset optional overrides at their configured defaults", {
  captured <- NULL
  local_mocked_bindings(
    run_pipeline = function(cfg, ...) { captured <<- cfg },
    .package = "popgenVCF"
  )
  path <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(
    list(input = list(vcf = "orig.vcf"), output = list(directory = "orig_out")),
    path
  )

  popgenVCF::cli_main(c("--config", path))

  expect_equal(captured$input$vcf, "orig.vcf")
  expect_equal(captured$output$directory, "orig_out")
  expect_false(isTRUE(captured$compute$force_gds))
  expect_true(isTRUE(captured$report$enabled))
})
