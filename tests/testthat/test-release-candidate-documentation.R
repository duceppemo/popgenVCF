rc_source_root <- function() {
  required <- c("DESCRIPTION", "docs/developer/release-candidate-closure.md",
                "docs/user/ancestry-backends.md",
                "inst/metadata/release-candidate-policy.json")
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
  bases <- unique(c(Sys.getenv("GITHUB_WORKSPACE", unset = ""),
                    ancestors(normalizePath(testthat::test_path(), mustWork = TRUE)),
                    ancestors(normalizePath(getwd(), mustWork = TRUE))))
  bases <- bases[nzchar(bases)]
  candidates <- unique(c(bases, file.path(bases, "popgenVCF"),
                         file.path(bases, "00_pkg_src", "popgenVCF")))
  candidates <- normalizePath(candidates, winslash = "/", mustWork = FALSE)
  matches <- candidates[vapply(candidates, function(x) {
    dir.exists(x) && all(file.exists(file.path(x, required)))
  }, logical(1L))]
  if (length(matches)) matches[[1L]] else NA_character_
}

test_that("closure and ancestry operator documentation is retained", {
  root <- rc_source_root()
  if (is.na(root)) testthat::skip("Repository-only closure documentation is unavailable")
  closure <- readLines(file.path(root, "docs/developer/release-candidate-closure.md"), warn = FALSE)
  ancestry <- readLines(file.path(root, "docs/user/ancestry-backends.md"), warn = FALSE)
  expect_true(all(c("rehearsal", "production", "release-candidate-SHA256SUMS.txt") %in%
                    unlist(lapply(c("rehearsal", "production", "release-candidate-SHA256SUMS.txt"),
                                  function(x) x[any(grepl(x, closure, fixed = TRUE))]))))
  for (term in c("ADMIXTURE", "fastStructure", "LEA/sNMF", "q_sample_file")) {
    expect_true(any(grepl(term, ancestry, fixed = TRUE)))
  }
})
