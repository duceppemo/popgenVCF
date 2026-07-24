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
  if (!length(matches)) return(NA_character_)
  matches[[1L]]
}

require_user_documentation_root <- function() {
  root <- user_documentation_source_root()
  if (is.na(root)) {
    testthat::skip("Repository-only documentation is unavailable in the built source package")
  }
  root
}

read_documentation_files <- function(root, paths) {
  stats::setNames(
    lapply(paths, function(path) readLines(file.path(root, path), warn = FALSE)),
    paths
  )
}

test_that("Phase 0.9.30 user guides are complete and site-linked", {
  root <- require_user_documentation_root()
  guides <- c(
    "vignettes/getting-started.Rmd",
    "vignettes/interpreting-results.Rmd",
    "vignettes/publication-gallery.Rmd",
    "vignettes/troubleshooting.Rmd",
    "vignettes/reproducibility.Rmd",
    "vignettes/containers-and-hpc.Rmd"
  )
  expect_true(all(file.exists(file.path(root, guides))))

  pkgdown <- readLines(file.path(root, "_pkgdown.yml"), warn = FALSE)
  slugs <- sub("\\.Rmd$", "", basename(guides))
  for (slug in slugs) {
    expect_true(any(grepl(paste0("articles/", slug, "\\.html"), pkgdown)))
    expect_true(any(trimws(pkgdown) == paste0("- ", slug)))
  }

  contents <- read_documentation_files(root, guides)
  for (path in names(contents)) {
    if (!any(grepl("%\\\\VignetteIndexEntry\\{", contents[[path]]))) {
      testthat::fail(paste("Missing VignetteIndexEntry in", path))
    }
  }
})

test_that("public user documentation uses current or release-neutral image examples", {
  root <- require_user_documentation_root()
  public_files <- c(
    "README.md",
    file.path(
      "vignettes",
      list.files(file.path(root, "vignettes"), pattern = "\\.Rmd$", full.names = FALSE)
    ),
    file.path(
      "docs/user",
      list.files(file.path(root, "docs/user"), pattern = "\\.md$", full.names = FALSE)
    )
  )
  contents <- read_documentation_files(root, unique(public_files))
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

test_that("README is a minimal landing page for the Wiki and pkgdown", {
  root <- require_user_documentation_root()
  description <- read.dcf(file.path(root, "DESCRIPTION"))
  expect_identical(unname(description[1L, "Version"]), "0.10.0")

  readme <- readLines(file.path(root, "README.md"), warn = FALSE)
  required_text <- c(
    "man/figures/popgenVCF-logo.svg",
    "actions/workflows/R-CMD-check.yaml/badge.svg",
    "actions/workflows/scientific-validation.yaml/badge.svg",
    "github.com/duceppemo/popgenVCF/wiki/Getting-Started",
    "github.com/duceppemo/popgenVCF/wiki/Validation-and-Scientific-Review",
    "github.com/duceppemo/popgenVCF/wiki/Developer-Guide",
    "duceppemo.github.io/popgenVCF/"
  )
  expect_true(all(vapply(required_text, function(text) {
    any(grepl(text, readme, fixed = TRUE))
  }, logical(1L))))
  expect_lte(length(readme), 120L)
  expect_false(any(grepl("## Workflow modes", readme, fixed = TRUE)))
  expect_false(any(grepl("## Metadata identity contract", readme, fixed = TRUE)))
  expect_false(any(grepl("before Phase 0.9.30", readme, fixed = TRUE)))
})
