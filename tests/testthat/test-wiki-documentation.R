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

test_that("Wiki User Guide contains the complete metadata contract", {
  root <- wiki_documentation_source_root()
  if (is.na(root)) testthat::skip("Repository Wiki source is unavailable")
  text <- paste(readLines(
    file.path(root, "wiki", "User-Guide.md"), warn = FALSE
  ), collapse = "\n")
  required <- c(
    "optional `alias`", "immutable VCF/GDS key", "display_order",
    "signed decimal degrees", "-90", "-180", "WGS84",
    "At least four", "Missing `population`", "literal `NA`",
    "qc.max_sample_missing", "qc.max_variant_missing",
    "02_sample_metadata_match.tsv", "analysis_capabilities.tsv"
  )
  for (term in required) {
    expect_true(
      grepl(term, text, fixed = TRUE),
      info = paste("Wiki metadata contract is missing:", term)
    )
  }
})

test_that("internal Wiki page links resolve to maintained source pages", {
  root <- wiki_documentation_source_root()
  if (is.na(root)) testthat::skip("Repository Wiki source is unavailable")
  pages <- list.files(file.path(root, "wiki"), pattern = "\\.md$", full.names = TRUE)
  pages <- pages[basename(pages) != "README.md"]
  hits <- unlist(lapply(pages, function(path) {
    lines <- readLines(path, warn = FALSE)
    # Optional leading "!" distinguishes an image embed (![alt](file.png),
    # validated against a real file) from a page link ([text](PageName),
    # validated against wiki/<PageName>.md) -- both share the same
    # [..](..) shape, so the two must not be checked the same way.
    matches <- gregexpr("!?\\[[^]]+\\]\\(([^)]+)\\)", lines, perl = TRUE)
    links <- regmatches(lines, matches)
    unlist(links, use.names = FALSE)
  }), use.names = FALSE)
  is_image <- startsWith(hits, "!")
  targets <- sub("^!?\\[[^]]+\\]\\(([^)]+)\\)$", "\\1", hits)
  targets <- sub("#.*$", "", targets)
  keep <- nzchar(targets) & !grepl("^(https?://|#|mailto:)", targets)
  targets <- targets[keep]
  is_image <- is_image[keep]

  link_targets <- targets[!is_image]
  expected_pages <- file.path(root, "wiki", paste0(link_targets, ".md"))
  missing_pages <- link_targets[!file.exists(expected_pages)]
  expect_length(unique(missing_pages), 0L)

  image_targets <- targets[is_image]
  expected_images <- file.path(root, "wiki", image_targets)
  missing_images <- image_targets[!file.exists(expected_images)]
  expect_length(unique(missing_images), 0L)
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
