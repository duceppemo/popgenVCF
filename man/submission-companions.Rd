\name{submission-companions}
\alias{new_submission_companions}
\alias{validate_submission_companions}
\alias{write_submission_companions}
\title{Deterministic submission companion documents}
\usage{
new_submission_companions(manuscript, journal = NULL, editor = NULL,
  significance = NULL, novelty = NULL, highlights = character(),
  suggested_reviewers = NULL, opposed_reviewers = NULL,
  confirmations = list(), max_highlights = 5L,
  max_highlight_characters = 125L)

validate_submission_companions(x, strict = TRUE)

write_submission_companions(companions, directory, overwrite = FALSE,
  strict = FALSE)
}
\description{
Creates, validates, renders, and checksum-verifies cover letters, highlights,
and author declaration summaries without changing canonical manuscript content.
}
\value{
\code{new_submission_companions()} returns a
\code{PopgenVCFSubmissionCompanions} object. Validation returns \code{TRUE}
invisibly. Writing returns the normalized output directory invisibly.
}
