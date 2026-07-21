release_reconciliation_test_root <- function() {
  test_dir <- normalizePath(testthat::test_path(), winslash = "/", mustWork = TRUE)
  candidates <- unique(c(
    normalizePath(file.path(test_dir, "..", ".."), winslash = "/", mustWork = TRUE),
    normalizePath(file.path(test_dir, "..", "..", "00_pkg_src", "popgenVCF"), winslash = "/", mustWork = FALSE),
    normalizePath(file.path(test_dir, "..", "..", ".."), winslash = "/", mustWork = FALSE)
  ))
  required <- c("DESCRIPTION", "NAMESPACE", "NEWS.md", "README.md", "docs/ROADMAP.md")
  matches <- candidates[vapply(candidates, function(path) {
    dir.exists(path) && all(file.exists(file.path(path, required)))
  }, logical(1))]
  if (length(matches) == 0L) {
    stop("Unable to locate the package source tree for release reconciliation tests.", call. = FALSE)
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
