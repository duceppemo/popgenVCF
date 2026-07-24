wiki_documentation_source_root <- function() {
  required <- c("README.md", "wiki", "scripts/publish-wiki.sh")
  ancestors <- function(path) {
    out <- character()
    repeat {
      out <- c(out, path)
      parent <- dirname(path)
      if (identical(parent, path)) break
      path <- parent
    }
    out
  }
  bases <- unique(c(
    Sys.getenv("GITHUB_WORKSPACE", unset = ""),
    ancestors(normalizePath(testthat::test_path(), mustWork = TRUE)),
    ancestors(normalizePath(getwd(), mustWork = TRUE))
  ))
  bases <- bases[nzchar(bases)]
  candidates <- unique(c(
    bases, file.path(bases, "popgenVCF"),
    file.path(bases, "00_pkg_src", "popgenVCF")
  ))
  candidates <- normalizePath(candidates, winslash = "/", mustWork = FALSE)
  matches <- candidates[vapply(candidates, function(path) {
    dir.exists(path) && all(file.exists(file.path(path, required)))
  }, logical(1L))]
  if (length(matches)) matches[[1L]] else NA_character_
}

test_that("Wiki provides role-oriented documentation sections", {
  root <- wiki_documentation_source_root()
  if (is.na(root)) testthat::skip("Repository Wiki source is unavailable")
  required_pages <- c(
    "Home.md", "Getting-Started.md", "User-Guide.md",
    "Configuration-Reference.md", "Results-and-Interpretation.md",
    "Deployment-and-Troubleshooting.md",
    "Validation-and-Scientific-Review.md", "Developer-Guide.md",
    "Release-and-Governance.md", "Documentation-Map.md",
    "_Sidebar.md", "_Footer.md"
  )
  expect_true(all(file.exists(file.path(root, "wiki", required_pages))))

  sidebar <- readLines(file.path(root, "wiki", "_Sidebar.md"), warn = FALSE)
  for (heading in c("Users", "Validators", "Developers", "Maintainers")) {
    expect_true(any(grepl(heading, sidebar, fixed = TRUE)))
  }
})

test_that("internal Wiki page links resolve to maintained source pages", {
  root <- wiki_documentation_source_root()
  if (is.na(root)) testthat::skip("Repository Wiki source is unavailable")
  pages <- list.files(file.path(root, "wiki"), pattern = "\\.md$", full.names = TRUE)
  pages <- pages[basename(pages) != "README.md"]
  targets <- unlist(lapply(pages, function(path) {
    lines <- readLines(path, warn = FALSE)
    matches <- gregexpr("\\[[^]]+\\]\\(([^)]+)\\)", lines, perl = TRUE)
    links <- regmatches(lines, matches)
    links <- unlist(links, use.names = FALSE)
    sub("^.*\\(([^)]+)\\)$", "\\1", links)
  }), use.names = FALSE)
  targets <- targets[
    nzchar(targets) & !grepl("^(https?://|#|mailto:)", targets)
  ]
  targets <- sub("#.*$", "", targets)
  expected <- file.path(root, "wiki", paste0(targets, ".md"))
  missing <- targets[!file.exists(expected)]
  expect_length(unique(missing), 0L)
})

test_that("scientific review handoff is explicit and non-automatic", {
  root <- wiki_documentation_source_root()
  if (is.na(root)) testthat::skip("Repository Wiki source is unavailable")
  review <- paste(readLines(
    file.path(root, "wiki", "Validation-and-Scientific-Review.md"),
    warn = FALSE
  ), collapse = "\n")
  for (term in c(
    "build_scientific_review_packet.R", "manual-review-checklist.tsv",
    "scientific-review-decision-template.json", "not automatically sent",
    "reviewed pull request", "READY", "BLOCKED"
  )) {
    expect_true(grepl(term, review, fixed = TRUE))
  }
})

test_that("Wiki publication helper is syntactically valid and dry-run by default", {
  root <- wiki_documentation_source_root()
  if (is.na(root)) testthat::skip("Repository Wiki source is unavailable")
  script <- file.path(root, "scripts", "publish-wiki.sh")
  status <- system2("bash", c("-n", shQuote(script)))
  expect_identical(status, 0L)
  contents <- paste(readLines(script, warn = FALSE), collapse = "\n")
  expect_match(contents, 'mode="dry-run"', fixed = TRUE)
  expect_match(contents, '[[ "$mode" == "dry-run" ]]', fixed = TRUE)
})
