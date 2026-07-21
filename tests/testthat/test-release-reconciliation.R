release_reconciliation_test_root <- function() {
  required <- c("DESCRIPTION", "NAMESPACE", "NEWS.md", "README.md", "docs/ROADMAP.md")

  is_source_root <- function(path) {
    nzchar(path) && dir.exists(path) && all(file.exists(file.path(path, required)))
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
      if (identical(parent, current)) {
        break
      }
      current <- parent
    }
    out
  }

  bases <- unique(c(
    workspace,
    ancestors(test_dir),
    ancestors(working_dir)
  ))
  bases <- bases[nzchar(bases)]

  candidates <- unique(c(
    bases,
    file.path(bases, "popgenVCF"),
    file.path(bases, "00_pkg_src", "popgenVCF")
  ))
  candidates <- normalizePath(candidates, winslash = "/", mustWork = FALSE)

  matches <- candidates[vapply(candidates, is_source_root, logical(1))]
  if (length(matches) == 0L) {
    stop(
      "Unable to locate the package source tree for release reconciliation tests. Checked: ",
      paste(candidates, collapse = ", "),
      call. = FALSE
    )
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
  expect_false(any(audit$findings$severity == "blocking"), info = paste(
    paste(audit$findings$category, audit$findings$item, sep = ": "),
    collapse = "\n"
  ))
  expect_true(audit$passed)
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
    "exports.tsv",
    "findings.tsv",
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
