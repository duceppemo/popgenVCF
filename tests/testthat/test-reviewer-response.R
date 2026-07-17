reviewer_comments_fixture <- function() {
  data.frame(
    reviewer = c("Reviewer 2", "Reviewer 1"),
    comment_id = c("2.1", "1.1"),
    section = c("Results", "Methods"),
    comment = c("Clarify the figure interpretation.", "Report the filtering thresholds."),
    status = c("partially_addressed", "addressed"),
    response = c("We clarified the interpretation.", "We added the thresholds."),
    action = c("Revised the result paragraph.", "Added a filtering paragraph."),
    evidence = c("Revised manuscript Results", "Revised manuscript Methods"),
    location = c("Results, paragraph 3", "Methods, paragraph 2"),
    stringsAsFactors = FALSE
  )
}

test_that("reviewer-response records are deterministic", {
  comments <- reviewer_comments_fixture()
  a <- new_reviewer_response(comments, "manuscript-1", "revision-1")
  b <- new_reviewer_response(comments[2:1, ], "manuscript-1", "revision-1")

  expect_s3_class(a, "PopgenVCFReviewerResponse")
  expect_identical(a$digest, b$digest)
  expect_identical(a$comments$reviewer, c("Reviewer 1", "Reviewer 2"))
  expect_true(validate_reviewer_response(a))
})

test_that("reviewer comments reject malformed and duplicate identities", {
  comments <- reviewer_comments_fixture()
  duplicate <- rbind(comments, comments[1, ])
  expect_error(new_reviewer_response(duplicate, "m", "r"), "must be unique")

  comments$status[1] <- "complete"
  expect_error(new_reviewer_response(comments, "m", "r"), "status must be")
  expect_error(new_reviewer_response(reviewer_comments_fixture(), "", "r"), "manuscript_id")
})

test_that("completion reports preserve explicit incomplete responses", {
  comments <- reviewer_comments_fixture()
  comments$status[1] <- "unanswered"
  comments$response[1] <- ""
  comments$action[1] <- ""
  comments$evidence[1] <- ""
  comments$location[1] <- ""
  response <- new_reviewer_response(comments, "m", "r")
  report <- reviewer_response_report(response)

  expect_s3_class(report, "PopgenVCFReviewerResponseReport")
  expect_equal(report[reviewer == "Reviewer 2", completion], "incomplete")
  expect_error(reviewer_response_report(response, strict = TRUE), "Reviewer responses are incomplete")
})

test_that("declined comments require an explicit rationale", {
  comments <- reviewer_comments_fixture()[1, ]
  comments$status <- "declined"
  comments$response <- ""
  comments$action <- ""
  comments$evidence <- ""
  comments$location <- ""
  response <- new_reviewer_response(comments, "m", "r")
  expect_equal(reviewer_response_report(response)$completion, "incomplete")

  comments$response <- "We respectfully decline because the requested analysis is outside the study design."
  response <- new_reviewer_response(comments, "m", "r")
  expect_equal(reviewer_response_report(response)$completion, "complete")
})

test_that("Markdown and written bundles are stable and checksummed", {
  response <- new_reviewer_response(reviewer_comments_fixture(), "m", "r")
  markdown <- render_reviewer_response(response)
  expect_true(any(grepl("Reviewer 1 - 1.1", markdown, fixed = TRUE)))
  expect_identical(markdown, render_reviewer_response(response))

  root <- tempfile("reviewer-response-")
  dir.create(root)
  out <- write_reviewer_response(response, root)
  expect_true(validate_reviewer_response(out))
  expect_true(all(file.exists(file.path(out, c(
    "reviewer-response.json", "reviewer-response.md", "reviewer-response.tsv",
    "reviewer-response-report.tsv", "reviewer-response-manifest.tsv"
  )))))
  expect_error(write_reviewer_response(response, root), "already exists")

  write("tampered", file.path(out, "reviewer-response.md"), append = TRUE)
  expect_error(validate_reviewer_response(out), "checksum mismatch")
})
