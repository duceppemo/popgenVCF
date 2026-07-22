package_check_source_root <- function() {
  required <- c("DESCRIPTION", "NAMESPACE", ".Rbuildignore", "R", "man")
  current <- normalizePath(testthat::test_path(), winslash = "/", mustWork = TRUE)
  candidates <- character()
  repeat {
    candidates <- c(candidates, current)
    parent <- dirname(current)
    if (identical(parent, current)) break
    current <- parent
  }
  workspace <- Sys.getenv("GITHUB_WORKSPACE", unset = "")
  candidates <- unique(c(workspace, candidates, file.path(candidates, "popgenVCF")))
  matches <- candidates[vapply(candidates, function(path) {
    nzchar(path) && dir.exists(path) && all(file.exists(file.path(path, required)))
  }, logical(1))]
  if (!length(matches)) stop("Unable to locate package source root", call. = FALSE)
  normalizePath(matches[[1L]], winslash = "/", mustWork = TRUE)
}

test_that("repository-only release files are excluded from source packages", {
  root <- package_check_source_root()
  patterns <- readLines(file.path(root, ".Rbuildignore"), warn = FALSE)
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
  namespace <- readLines(file.path(root, "NAMESPACE"), warn = FALSE)
  expected_imports <- c(
    "importFrom(stats,aggregate)",
    "importFrom(stats,setNames)",
    "importFrom(utils,capture.output)",
    "importFrom(utils,head)",
    "importFrom(utils,modifyList)",
    "importFrom(utils,object.size)",
    "importFrom(utils,str)",
    "importFrom(utils,tail)"
  )
  expect_true(all(expected_imports %in% namespace))

  declarations <- readLines(file.path(root, "R", "zzz-package-check.R"), warn = FALSE)
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
