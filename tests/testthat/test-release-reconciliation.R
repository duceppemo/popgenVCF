release_reconciliation_test_root <- function() {
  required <- c("DESCRIPTION", "NAMESPACE", "NEWS.md", "README.md")

  is_source_root <- function(path) {
    # docs/ is excluded from built packages by .Rbuildignore (see
    # release_reconciliation_root()), so a tree produced by R CMD check (e.g.
    # 00_pkg_src/<pkg>) only ever carries the packaged inst/doc/ROADMAP.md
    # mirror. Accept either roadmap copy.
    nzchar(path) && dir.exists(path) && all(file.exists(file.path(path, required))) &&
      any(file.exists(file.path(path, c("docs/ROADMAP.md", "inst/doc/ROADMAP.md"))))
  }

  workspace <- Sys.getenv("GITHUB_WORKSPACE", unset = "")
  test_dir <- normalizePath(testthat::test_path(), winslash = "/", mustWork = TRUE)
  working_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

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

  bases <- unique(c(workspace, ancestors(test_dir), ancestors(working_dir)))
  bases <- bases[nzchar(bases)]
  candidates <- unique(c(
    bases,
    file.path(bases, "popgenVCF"),
    file.path(bases, "00_pkg_src", "popgenVCF")
  ))
  candidates <- normalizePath(candidates, winslash = "/", mustWork = FALSE)

  matches <- candidates[vapply(candidates, is_source_root, logical(1))]
  if (length(matches) == 0L) {
    # These tests inspect the checked-out source tree (DESCRIPTION, NAMESPACE,
    # roxygen blocks, docs/ROADMAP.md), which is not reachable from an
    # installed-package test run (e.g. plain `R CMD check` on a built tarball,
    # or `devtools::check()`) unless it happens to share a directory ancestor
    # with the checkout. They run for real in CI (GITHUB_WORKSPACE) and via
    # scripts/check-package.sh; skip rather than fail everywhere else.
    testthat::skip(paste0(
      "package source tree not reachable for release reconciliation tests; checked: ",
      paste(candidates, collapse = ", ")
    ))
  }
  matches[[1L]]
}

test_that("release-facing metadata and public API remain reconciled", {
  root <- release_reconciliation_test_root()
  audit <- release_api_reconciliation(root)

  expect_identical(audit$version, "0.10.0")
  expect_true(all(audit$version_signals$present), info = paste(
    audit$version_signals$file[!audit$version_signals$present],
    collapse = ", "
  ))
  expect_length(setdiff(audit$exports, audit$aliases$alias), 0L)
  expect_equal(nrow(audit$dynamic_exports), 0L)
  expect_true(nrow(audit$roxygen_exports) > 0L)
  expect_false(any(audit$findings$severity == "blocking"), info = paste(
    paste(audit$findings$category, audit$findings$item, sep = ": "),
    collapse = "\n"
  ))
  expect_true(audit$passed)
})

test_that("every roxygen export has an explicit namespace declaration", {
  root <- release_reconciliation_test_root()
  audit <- release_api_reconciliation(root)
  s3_symbols <- if (nrow(audit$s3_methods)) {
    paste0(audit$s3_methods$generic, ".", audit$s3_methods$class)
  } else {
    character()
  }
  declared <- union(audit$exports, s3_symbols)
  missing <- setdiff(sort(unique(audit$roxygen_exports$symbol)), declared)

  expect_length(missing, 0L)
  expect_false(any(audit$findings$category == "roxygen-namespace"))
})

test_that("roxygen export ownership is unique", {
  root <- release_reconciliation_test_root()
  audit <- release_api_reconciliation(root)
  duplicated_symbols <- sort(unique(
    audit$roxygen_exports$symbol[duplicated(audit$roxygen_exports$symbol)]
  ))
  expect_length(duplicated_symbols, 0L)
})

test_that("release reconciliation evidence is deterministic and machine readable", {
  root <- release_reconciliation_test_root()
  output_one <- withr::local_tempdir()
  output_two <- withr::local_tempdir()

  audit_one <- write_release_api_reconciliation(root, output_one)
  audit_two <- write_release_api_reconciliation(root, output_two)

  files_one <- sort(list.files(output_one))
  files_two <- sort(list.files(output_two))
  expect_identical(files_one, files_two)
  expect_identical(files_one, c(
    "dynamic-exports.tsv",
    "exports.tsv",
    "findings.tsv",
    "roxygen-exports.tsv",
    "s3-methods.tsv",
    "summary.tsv",
    "version-signals.tsv"
  ))

  for (file in files_one) {
    expect_identical(
      readLines(file.path(output_one, file), warn = FALSE),
      readLines(file.path(output_two, file), warn = FALSE),
      info = file
    )
  }
  expect_true(audit_one$passed)
  expect_identical(audit_one$findings, audit_two$findings)
})
