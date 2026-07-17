\name{reviewer_response}
\alias{new_reviewer_response}
\alias{validate_reviewer_response}
\alias{reviewer_response_table}
\alias{reviewer_response_report}
\alias{render_reviewer_response}
\alias{write_reviewer_response}
\title{Deterministic reviewer-response records and bundles}
\usage{
new_reviewer_response(comments, manuscript_id, revision_id,
  title = "Response to reviewers", version = "1.0")

validate_reviewer_response(x)

reviewer_response_table(x)

reviewer_response_report(x, strict = FALSE)

render_reviewer_response(x)

write_reviewer_response(x, directory, overwrite = FALSE)
}
\arguments{
  \item{comments}{A data frame containing reviewer, comment_id, section, comment, status, response, action, evidence, and location columns.}
  \item{manuscript_id}{Stable manuscript identifier.}
  \item{revision_id}{Stable revision identifier.}
  \item{title}{Human-readable response-letter title.}
  \item{version}{Record content version.}
  \item{x}{A reviewer-response record or, for validation, a written bundle directory.}
  \item{strict}{Whether incomplete responses raise an error.}
  \item{directory}{Parent output directory.}
  \item{overwrite}{Whether an existing bundle may be replaced.}
}
\value{
\code{new_reviewer_response()} returns a \code{PopgenVCFReviewerResponse}.
\code{reviewer_response_table()} and \code{reviewer_response_report()} return deterministic data tables.
\code{render_reviewer_response()} returns Markdown lines.
\code{write_reviewer_response()} invisibly returns the normalized bundle directory.
Validation functions return \code{TRUE} invisibly on success.
}
\description{
Creates immutable reviewer-response records from explicit author-supplied reviewer comments, responses, manuscript actions, evidence, and locations. The API does not invent comments, rebuttals, or claims of completion.
}
