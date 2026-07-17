test_that("manuscript revisions are deterministic and validated", {
  sections <- data.frame(
    section_id = c("results", "methods"),
    title = c("Results", "Methods"),
    content = c("Observed result.", "Analysis method."),
    stringsAsFactors = FALSE
  )
  a <- new_manuscript_revision("ms-1", "r1", sections)
  b <- new_manuscript_revision("ms-1", "r1", sections[2:1, ])
  expect_identical(a$digest, b$digest)
  expect_true(validate_manuscript_revision(a))
  expect_error(new_manuscript_revision("ms-1", "r1", rbind(sections, sections[1, ])), "unique")
  expect_error(new_manuscript_revision("ms-1", "r1", sections, parent_revision_id = "r1"), "differ")
})

test_that("revision comparison classifies section changes", {
  before <- new_manuscript_revision(
    "ms-1", "r1",
    data.frame(
      section_id = c("methods", "results", "discussion"),
      title = c("Methods", "Results", "Discussion"),
      content = c("Old method.", "Stable result.", "Old discussion."),
      stringsAsFactors = FALSE
    )
  )
  after <- new_manuscript_revision(
    "ms-1", "r2",
    data.frame(
      section_id = c("methods", "results", "conclusion"),
      title = c("Methods", "Results", "Conclusion"),
      content = c("Expanded analytical method.", "Stable result.", "New conclusion."),
      stringsAsFactors = FALSE
    ),
    parent_revision_id = "r1"
  )
  diff <- compare_manuscript_revisions(before, after)
  expect_s3_class(diff, "PopgenVCFManuscriptRevisionDiff")
  expect_identical(diff$change_type, c("added", "removed", "modified", "unchanged"))
  expect_identical(diff$section_id, c("conclusion", "discussion", "methods", "results"))
  expect_true(diff$word_delta[diff$section_id == "methods"] > 0L)
  expect_error(compare_manuscript_revisions(before, after, strict = TRUE), "lack explicit explanations")
})

test_that("explicit annotations document changes", {
  before <- new_manuscript_revision("ms", "r1", data.frame(section_id = "methods", title = "Methods", content = "Old."))
  after <- new_manuscript_revision("ms", "r2", data.frame(section_id = "methods", title = "Methods", content = "New text."))
  annotations <- data.frame(
    section_id = "methods",
    explanation = "Added the author-supplied filtering details.",
    reviewer_comments = "reviewer-1.comment-2"
  )
  diff <- compare_manuscript_revisions(before, after, annotations, strict = TRUE)
  expect_identical(diff$status, "documented")
  expect_identical(diff$reviewer_comments, "reviewer-1.comment-2")
  expect_error(compare_manuscript_revisions(before, after, rbind(annotations, annotations)), "at most one")
  expect_error(compare_manuscript_revisions(before, after, data.frame(section_id = "unknown")), "unknown")
})

test_that("revision diff bundles are deterministic and tamper evident", {
  before <- new_manuscript_revision("ms", "r1", data.frame(section_id = "methods", title = "Methods", content = "Old."))
  after <- new_manuscript_revision("ms", "r2", data.frame(section_id = "methods", title = "Methods", content = "New."))
  annotations <- data.frame(section_id = "methods", explanation = "Author supplied revision.")
  root <- tempfile("revision-diff-")
  dir.create(root)
  out <- write_manuscript_revision_diff(before, after, root, annotations)
  expect_true(validate_manuscript_revision_diff_bundle(out))
  expect_true(all(file.exists(file.path(out, c(
    "revision-diff.json", "revision-diff.tsv", "revision-diff.md", "revision-diff-manifest.tsv"
  )))))
  expect_error(write_manuscript_revision_diff(before, after, root, annotations), "already exists")
  write("tampered", file.path(out, "revision-diff.md"), append = TRUE)
  expect_error(validate_manuscript_revision_diff_bundle(out), "checksum mismatch")
})
