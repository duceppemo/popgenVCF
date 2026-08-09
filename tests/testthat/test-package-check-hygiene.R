package_check_source_root <- function() {
  # .Rbuildignore is intentionally excluded from every built package (it is a
  # build-time control file, never installed), so it is not required here even
  # though one test below needs its literal content and checks for it itself.
  # 00_pkg_src/<pkg>, produced by R CMD check, otherwise carries everything
  # this helper needs (R/, man/, NAMESPACE, DESCRIPTION).
  required <- c("DESCRIPTION", "NAMESPACE", "R", "man")
  current <- normalizePath(testthat::test_path(), winslash = "/", mustWork = TRUE)
  candidates <- character()
  repeat {
    candidates <- c(candidates, current)
    parent <- dirname(current)
    if (identical(parent, current)) break
    current <- parent
  }
  workspace <- Sys.getenv("GITHUB_WORKSPACE", unset = "")
  candidates <- unique(c(
    workspace, candidates,
    file.path(candidates, "popgenVCF"),
    file.path(candidates, "00_pkg_src", "popgenVCF")
  ))
  matches <- candidates[vapply(candidates, function(path) {
    nzchar(path) && dir.exists(path) && all(file.exists(file.path(path, required)))
  }, logical(1))]
  if (!length(matches)) {
    # Not reachable from every installed-package test run (e.g. covr's own
    # temp-library install layout has no relationship to the checkout at
    # all), unlike R CMD check's 00_pkg_src/<pkg> sibling. Skip rather than
    # fail when the source tree genuinely cannot be found.
    testthat::skip(paste0(
      "package source tree not reachable for hygiene tests; checked: ",
      paste(candidates, collapse = ", ")
    ))
  }
  normalizePath(matches[[1L]], winslash = "/", mustWork = TRUE)
}

test_that("repository-only release files are excluded from source packages", {
  root <- package_check_source_root()
  ignore_file <- file.path(root, ".Rbuildignore")
  if (!file.exists(ignore_file)) {
    testthat::skip(".Rbuildignore is not shipped in built packages and is unavailable outside a source checkout")
  }
  patterns <- readLines(ignore_file, warn = FALSE)
  excluded <- c(".dockerignore", "Dockerfile", "Apptainer.def", "codemeta.json", "docker")
  expect_true(all(vapply(excluded, function(path) {
    any(vapply(patterns[nzchar(patterns)], grepl, logical(1), x = path, perl = TRUE))
  }, logical(1))))
})

test_that("package code does not call its own namespace with triple colon", {
  root <- package_check_source_root()
  files <- list.files(file.path(root, "R"), pattern = "\\.[Rr]$", full.names = TRUE)
  occurrences <- unlist(lapply(files, function(path) {
    lines <- readLines(path, warn = FALSE)
    hits <- grep("popgenVCF[[:space:]]*:::", lines, perl = TRUE)
    if (!length(hits)) return(character())
    paste0(basename(path), ":", hits)
  }), use.names = FALSE)
  if (length(occurrences)) {
    testthat::fail(paste(
      "Package-internal triple-colon access found:",
      paste(occurrences, collapse = "\n")
    ))
  }
  expect_length(occurrences, 0L)
})

test_that("static-analysis imports and data.table NSE declarations are retained", {
  root <- package_check_source_root()
  # Parsed rather than line-matched: roxygen2 can write importFrom() either as
  # one directive per line or, since 8.1.0, grouped into a single multi-line
  # block per package (confirmed against a real CI run) -- what matters is
  # that each function is actually imported, not the exact textual layout.
  namespace_text <- paste(readLines(file.path(root, "NAMESPACE"), warn = FALSE), collapse = "\n")
  import_blocks <- regmatches(namespace_text, gregexpr("importFrom\\([^)]*\\)", namespace_text))[[1]]
  imported <- unlist(lapply(import_blocks, function(block) {
    inner <- sub("^importFrom\\(", "", sub("\\)$", "", block))
    parts <- trimws(strsplit(inner, ",")[[1]])
    parts <- parts[nzchar(parts)]
    if (length(parts) < 2L) return(character())
    paste0(parts[1], "::", parts[-1])
  }))
  expected_imports <- c(
    "stats::aggregate", "stats::setNames",
    "utils::capture.output", "utils::head", "utils::modifyList",
    "utils::object.size", "utils::str", "utils::tail"
  )
  expect_true(all(expected_imports %in% imported))

  declarations <- readLines(file.path(root, "R", "package-check.R"), warn = FALSE)
  expect_true(any(grepl("utils::globalVariables", declarations, fixed = TRUE)))
})

test_that("documented usage lines fit the R CMD check width", {
  root <- package_check_source_root()
  topics <- c("golden_outputs.Rd", "journal-submission-profiles.Rd")
  for (topic in topics) {
    lines <- readLines(file.path(root, "man", topic), warn = FALSE)
    start <- match("\\usage{", lines, nomatch = 0L)
    expect_gt(start, 0L)
    depth <- 0L
    usage <- character()
    for (line in lines[start:length(lines)]) {
      depth <- depth + lengths(regmatches(line, gregexpr("{", line, fixed = TRUE))) -
        lengths(regmatches(line, gregexpr("}", line, fixed = TRUE)))
      usage <- c(usage, line)
      if (depth == 0L) break
    }
    width <- max(nchar(usage, type = "width"))
    if (width > 90L) {
      testthat::fail(sprintf("%s has a %d-character usage line", topic, width))
    }
    expect_lte(width, 90L)
  }
})
