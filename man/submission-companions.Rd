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
\arguments{
\item{manuscript}{A canonical \code{PopgenVCFManuscript} object supplying the title, authors, corresponding-author metadata, and declarations.}
\item{journal}{Optional target journal name supplied by the author. Missing values are retained as visible placeholders.}
\item{editor}{Optional handling-editor name or salutation supplied by the author.}
\item{significance}{Optional author-written statement describing the manuscript's significance. The function never generates this claim automatically.}
\item{novelty}{Optional author-written statement describing the manuscript's novelty. The function never generates this claim automatically.}
\item{highlights}{Character vector of author-supplied manuscript highlights in submission order.}
\item{suggested_reviewers}{Optional reviewer records supplied by the author, including names and any available affiliation, email, or rationale fields.}
\item{opposed_reviewers}{Optional records identifying reviewers the authors request be excluded, with author-supplied reasons.}
\item{confirmations}{Named list of explicit author confirmations, such as originality, author approval, or absence of simultaneous submission.}
\item{max_highlights}{Maximum permitted number of highlight entries.}
\item{max_highlight_characters}{Maximum permitted character count for each highlight.}
\item{x}{A \code{PopgenVCFSubmissionCompanions} object to validate.}
\item{strict}{Logical flag. In strict mode, missing required author inputs and placeholders cause validation to fail; permissive mode reports them without failing.}
\item{companions}{A validated \code{PopgenVCFSubmissionCompanions} object to write.}
\item{directory}{Destination directory in which the isolated \code{companions/} output is created.}
\item{overwrite}{Logical flag controlling replacement of an existing companion-document output directory.}
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
