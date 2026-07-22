user_documentation_source_root <- function() {
  required <- c("DESCRIPTION", "README.md", "_pkgdown.yml", "vignettes", "docs/user")

  is_root <- function(path) {
    nzchar(path) && dir.exists(path) && all(file.exists(file.path(path, required)))
  }

  ancestors <- function(path) {
    out <- character()
    current <- path
    repeat {
      out <- c(out, current)
      parent <- dirname(current)
      if (identical(parent, current)) break
      current <- parent
    }
    out
  }

  workspace <- Sys.getenv("GITHUB_WORKSPACE", unset = "")
  test_dir <- normalizePath(testthat::test_path(), winslash = "/", mustWork = TRUE)
  working_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  bases <- unique(c(workspace, ancestors(test_dir), ancestors(working_dir)))
  bases <- bases[nzchar(bases)]
  candidates <- unique(c(
    bases,
    file.path(bases, "popgenVCF"),
    file.path(bases, "00_pkg_src", "popgenVCF")
  ))
  candidates <- normalizePath(candidates, winslash = "/", mustWork = FALSE)
  matches <- candidates[vapply(candidates, is_root, logical(1L))]
  if (!length(matches)) stop("Unable to locate package source root", call. = FALSE)
  matches[[1L]]
}

read_documentation_files <- function(root, paths) {
  stats::setNames(
    lapply(paths, function(path) readLines(file.path(root, path), warn = FALSE)),
    paths
  )
}

test_that("Phase 0.9.30 user guides are complete and site-linked", {
  root <- user_documentation_source_root()
  guides <- c(
    "vignettes/getting-started.Rmd",
    "vignettes/interpreting-results.Rmd",
    "vignettes/troubleshooting.Rmd",
    "vignettes/reproducibility.Rmd",
    "vignettes/containers-and-hpc.Rmd"
  )
  expect_true(all(file.exists(file.path(root, guides))))

  pkgdown <- readLines(file.path(root, "_pkgdown.yml"), warn = FALSE)
  slugs <- sub("^vignettes/|\\.Rmd$", "", guides)
  for (slug in slugs) {
    expect_true(any(grepl(paste0("articles/", slug, "\\.html"), pkgdown)))
    expect_true(any(trimws(pkgdown) == paste0("- ", slug)))
  }

  contents <- read_documentation_files(root, guides)
  for (path in names(contents)) {
    expect_true(
      any(grepl("%\\\\VignetteIndexEntry\\{", contents[[path]])),
      label = path
    )
  }
})

test_that("public user documentation uses current or release-neutral image examples", {
  root <- user_documentation_source_root()
  public_files <- c(
    "README.md",
    list.files(file.path(root, "vignettes"), pattern = "\\.Rmd$", full.names = FALSE),
    file.path("docs/user", list.files(file.path(root, "docs/user"), pattern = "\\.md$"))
  )
  public_files <- unique(c(
    "README.md",
    file.path("vignettes", basename(public_files[grepl("\\.Rmd$", public_files)])),
    public_files[grepl("^docs/user/", public_files)]
  ))
  contents <- read_documentation_files(root, public_files)
  stale <- unlist(lapply(names(contents), function(path) {
    hits <- grep(
      "ghcr\\.io/duceppemo/popgenvcf:0\\.(8|9)(\\.[0-9]+)?",
      contents[[path]],
      perl = TRUE
    )
    if (!length(hits)) return(character())
    paste0(path, ":", hits, ": ", trimws(contents[[path]][hits]))
  }), use.names = FALSE)
  if (length(stale)) {
    testthat::fail(paste("Stale public container examples found:", paste(stale, collapse = "\n")))
  }
  expect_length(stale, 0L)
})

test_that("README and release identity expose the user guide entry points", {
  root <- user_documentation_source_root()
  description <- read.dcf(file.path(root, "DESCRIPTION"))
  expect_identical(unname(description[1L, "Version"]), "0.10.0")

  readme <- readLines(file.path(root, "README.md"), warn = FALSE)
  required_links <- c(
    "vignettes/getting-started.Rmd",
    "vignettes/interpreting-results.Rmd",
    "vignettes/troubleshooting.Rmd",
    "vignettes/reproducibility.Rmd",
    "vignettes/containers-and-hpc.Rmd"
  )
  expect_true(all(vapply(required_links, function(path) {
    any(grepl(path, readme, fixed = TRUE))
  }, logical(1L))))
  expect_false(any(grepl("before Phase 0.9.30", readme, fixed = TRUE)))
})
