test_that("submission companions preserve explicit author inputs", {
  project <- new_popgenvcf_project("companions")
  manuscript <- new_manuscript(
    project,
    title = "Companion test",
    authors = data.table::data.table(
      name = "A. Author", affiliation = "Institute", email = "a@example.org",
      orcid = "0000-0001-0000-0000", corresponding = TRUE
    )
  )
  first <- new_submission_companions(
    manuscript,
    journal = "Population Genetics Journal",
    significance = "This work provides a reproducible analysis framework.",
    novelty = "It links immutable scientific artifacts to submission records.",
    highlights = c("Deterministic manuscript companion generation", "Checksum-verified submission metadata"),
    confirmations = list(original_work = TRUE, all_authors_approved = TRUE)
  )
  second <- new_submission_companions(
    manuscript,
    journal = "Population Genetics Journal",
    significance = "This work provides a reproducible analysis framework.",
    novelty = "It links immutable scientific artifacts to submission records.",
    highlights = c("Deterministic manuscript companion generation", "Checksum-verified submission metadata"),
    confirmations = list(original_work = TRUE, all_authors_approved = TRUE)
  )
  expect_identical(first$digest, second$digest)
  expect_true(validate_submission_companions(first))
})

test_that("permissive companions expose placeholders and strict mode rejects them", {
  manuscript <- new_manuscript(new_popgenvcf_project("incomplete"), title = "Incomplete")
  companions <- new_submission_companions(manuscript)
  expect_true(validate_submission_companions(companions, strict = FALSE))
  expect_error(validate_submission_companions(companions, strict = TRUE), "incomplete")
  expect_true(all(c("journal", "significance", "novelty", "highlights", "corresponding_author") %in% submission_companion_missing(companions)))
})

test_that("companion limits are enforced", {
  manuscript <- new_manuscript(new_popgenvcf_project("limits"), title = "Limits")
  expect_error(
    new_submission_companions(manuscript, highlights = c("one", "two"), max_highlights = 1L),
    "too many highlights"
  )
  expect_error(
    new_submission_companions(manuscript, highlights = "too long", max_highlight_characters = 3L),
    "character limit"
  )
})

test_that("written companion directories are checksum verified", {
  manuscript <- new_manuscript(new_popgenvcf_project("write"), title = "Written")
  companions <- new_submission_companions(manuscript)
  directory <- tempfile()
  write_submission_companions(companions, directory, strict = FALSE)
  expect_true(validate_submission_companions(directory))
  expect_true(all(file.exists(file.path(directory, c("cover-letter.md", "highlights.md", "author-declarations.md", "companions-record.json", "companions-manifest.tsv")))))
  writeLines("modified", file.path(directory, "highlights.md"))
  expect_error(validate_submission_companions(directory), "checksum mismatch")
})

test_that("overwrite = TRUE clears a stale leftover file from a prior write, unlike write_scientific_release_bundle()'s own overwrite = TRUE convention which already does this", {
  # write_submission_companions(overwrite = TRUE) previously skipped straight
  # to writing this render's own files over whatever already existed in the
  # directory -- a stale file left over from a previous write (here, an old
  # companion set's highlights.md content, and an unrelated extra file) was
  # never removed, and got silently swept into the new manifest as if it
  # were part of the current render.
  manuscript <- new_manuscript(new_popgenvcf_project("overwrite"), title = "Overwrite")
  directory <- tempfile()

  first <- new_submission_companions(
    manuscript, highlights = "First highlight from the original companion set"
  )
  write_submission_companions(first, directory, strict = FALSE)
  writeLines("a leftover file this render never wrote", file.path(directory, "stale-leftover.txt"))

  second <- new_submission_companions(
    manuscript, highlights = "Second, completely different highlight"
  )
  write_submission_companions(second, directory, overwrite = TRUE, strict = FALSE)

  expect_false(file.exists(file.path(directory, "stale-leftover.txt")))
  manifest <- data.table::fread(file.path(directory, "companions-manifest.tsv"))
  expect_false("stale-leftover.txt" %in% manifest$path)
  highlights <- readLines(file.path(directory, "highlights.md"), warn = FALSE)
  expect_true(any(grepl("Second, completely different highlight", highlights, fixed = TRUE)))
  expect_false(any(grepl("First highlight from the original", highlights, fixed = TRUE)))
  expect_true(validate_submission_companions(directory))
})
