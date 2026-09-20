test_that("report plans discover and order canonical analyses", {
  pca <- new_pca_result(
    data.frame(sample_id = c("a", "b"), PC1 = c(-1, 1), PC2 = c(0, 0)),
    c(2, 1)
  )
  diversity <- new_diversity_result(data.frame(population = "A", Ho = .2))
  fst <- new_fst_result(.1, data.frame(population_1 = "A", population_2 = "B", fst = .1))
  plan <- build_population_genomics_report_plan(list(fst = fst, pca = pca, diversity = diversity))
  expect_s3_class(plan, "PopgenVCFReportPlan")
  expect_equal(plan$sections$analysis, c("pca", "diversity", "fst"))
  expect_true(all(plan$sections$validation_passed))
})

test_that("report filters and invalid inputs behave transparently", {
  pca <- new_pca_result(data.frame(sample_id = "a", PC1 = 0, PC2 = 0), c(1, .5))
  diversity <- new_diversity_result(data.frame(population = "A", Ho = .2))
  plan <- build_population_genomics_report_plan(list(pca, diversity), include = "pca")
  expect_equal(plan$sections$analysis, "pca")
  expect_error(build_population_genomics_report_plan(list(pca), exclude = "pca"), "no report sections")
  expect_error(build_population_genomics_report_plan(list(list(x = 1))), "canonical result")
})

test_that("a malicious report title cannot break out of the YAML front matter", {
  # write_report_qmd() previously built its title line as
  # paste0("title: \"", gsub("\"", "'", title), "\"") -- only double quotes
  # were neutralized, so a title containing a newline could close the
  # quoted scalar, inject new YAML front-matter keys, and even open a
  # fenced ```{r} code chunk that Quarto would execute on render. Confirmed
  # directly before the fix. yaml_scalar_line() (R/utils.R) now builds this
  # line via yaml::as.yaml(), which switches to a `|-` block scalar for
  # multi-line content -- every line of the injected text stays literal,
  # indented content inside that one YAML value.
  pca <- new_pca_result(data.frame(sample_id = "a", PC1 = 0, PC2 = 0), c(1, .5))
  evil_title <- paste(
    "Evil", "---", "execute:", "  echo: true", "```{r}",
    "system(\"touch /tmp/popgenvcf-qmd-injection-marker\")", "```",
    sep = "\n"
  )
  out <- tempfile("popgen-report-injection-")
  result <- write_population_genomics_report(list(pca = pca), out, title = evil_title, render = FALSE)
  qmd_path <- result$paths[["source"]]

  front_matter_lines <- readLines(qmd_path, warn = FALSE)
  fence_idx <- which(front_matter_lines == "---")
  expect_length(fence_idx, 2L)
  front_matter <- yaml::yaml.load(paste(
    front_matter_lines[(fence_idx[[1L]] + 1L):(fence_idx[[2L]] - 1L)], collapse = "\n"
  ))
  expect_identical(front_matter$title, evil_title)
  # The template's own, legitimate "execute: echo: false" must survive
  # untouched -- if the injected "execute:\n  echo: true" had broken out of
  # the title's block scalar instead of staying literal content inside it,
  # this would read TRUE (echo turned on) or the YAML parse would have
  # failed outright on the injected ```{r} fence below it.
  expect_identical(front_matter$execute$echo, FALSE)
})

test_that("report source generation does not require Quarto", {
  pca <- new_pca_result(data.frame(sample_id = c("a", "b"), PC1 = c(-1, 1), PC2 = 0), c(1, .5))
  out <- tempfile("popgen-report-")
  result <- write_population_genomics_report(list(pca = pca), out, render = FALSE)
  expect_s3_class(result$plan, "PopgenVCFReportPlan")
  expect_s3_class(result$artifacts, "PopgenVCFArtifactManifest")
  expect_true(all(file.exists(result$paths)))
  qmd <- readLines(result$paths[["source"]], warn = FALSE)
  expect_true(any(grepl("Principal component analysis", qmd, fixed = TRUE)))
  expect_true(any(grepl("Reproducibility", qmd, fixed = TRUE)))
})

test_that("report rendering gives actionable Quarto error", {
  pca <- new_pca_result(data.frame(sample_id = "a", PC1 = 0, PC2 = 0), c(1, .5))
  if (!nzchar(Sys.which("quarto"))) {
    expect_error(write_population_genomics_report(list(pca), tempfile(), render = TRUE), "Quarto is required")
  } else {
    succeed()
  }
})

test_that("the Quarto render receives each path as one argument when the output directory contains a space", {
  # system2() quotes only the executable; unquoted, "my report dir" arrived
  # at quarto as three separate arguments. A stand-in `quarto` on PATH records
  # exactly what it is given and writes the expected output file.
  skip_on_os("windows")
  bin <- tempfile("fake-quarto-bin-"); dir.create(bin)
  log <- file.path(bin, "args.txt")
  writeLines(c(
    "#!/bin/sh",
    sprintf(": > '%s'", log),
    sprintf("for a in \"$@\"; do printf '%%s\\n' \"$a\" >> '%s'; done", log),
    "while [ $# -gt 0 ]; do [ \"$1\" = \"--output-dir\" ] && out=$2; shift; done",
    "echo '<html></html>' > \"$out/population_genomics_report.html\""
  ), file.path(bin, "quarto"))
  Sys.chmod(file.path(bin, "quarto"), "0755")
  withr::local_path(bin, action = "prefix")

  pca <- new_pca_result(data.frame(sample_id = c("a", "b"), PC1 = c(-1, 1), PC2 = c(0, 0)), c(2, 1))
  out <- file.path(tempfile("report parent-"), "my report dir")
  dir.create(out, recursive = TRUE)
  result <- write_population_genomics_report(list(pca = pca), out, render = TRUE)

  args <- readLines(log)
  expect_identical(args[[1L]], "render")
  expect_identical(args[[2L]], file.path(out, "population_genomics_report.qmd"))
  expect_identical(args[[which(args == "--output-dir") + 1L]], normalizePath(out))
  expect_length(args, 6L)
  expect_true(file.exists(file.path(out, "population_genomics_report.html")))
})
