\name{manuscript_revision_diffs}
\alias{new_manuscript_revision}
\alias{validate_manuscript_revision}
\alias{compare_manuscript_revisions}
\alias{render_manuscript_revision_diff}
\alias{write_manuscript_revision_diff}
\title{Deterministic manuscript revision records and diff reports}
\description{
Creates immutable manuscript revision records, compares canonical manuscript
sections, and writes deterministic revision-diff bundles. Explanations and
reviewer links are recorded only when explicitly supplied by authors.
}
\usage{
new_manuscript_revision(
  manuscript_id,
  revision_id,
  sections,
  parent_revision_id = "",
  summary = "",
  created_by = ""
)

validate_manuscript_revision(x)

compare_manuscript_revisions(before, after, annotations = NULL, strict = FALSE)

render_manuscript_revision_diff(diff)

write_manuscript_revision_diff(
  before,
  after,
  directory,
  annotations = NULL,
  overwrite = FALSE
)
}
\arguments{
  \item{manuscript_id}{Stable manuscript identifier.}
  \item{revision_id}{Stable revision identifier.}
  \item{sections}{Data frame containing \code{section_id}, \code{title}, and \code{content}.}
  \item{parent_revision_id}{Optional parent revision identifier.}
  \item{summary}{Optional explicit author-supplied revision summary.}
  \item{created_by}{Optional explicit creator identifier.}
  \item{x}{A manuscript revision record.}
  \item{before}{Earlier manuscript revision record.}
  \item{after}{Later manuscript revision record.}
  \item{annotations}{Optional data frame containing explicit section explanations and reviewer-comment links.}
  \item{strict}{Whether changed sections without explicit explanations raise an error.}
  \item{diff}{A manuscript revision diff returned by \code{compare_manuscript_revisions()}.}
  \item{directory}{Parent output directory.}
  \item{overwrite}{Whether an existing output bundle may be replaced.}
}
\value{
\code{new_manuscript_revision()} returns a validated
\code{PopgenVCFManuscriptRevision}. \code{compare_manuscript_revisions()}
returns a deterministic \code{PopgenVCFManuscriptRevisionDiff} table.
Rendering returns Markdown lines, writing returns the normalized output path
invisibly, and validation returns \code{TRUE} invisibly.
}
\details{
Revision identities and section ordering are deterministic. The comparison
classifies sections as added, removed, modified, or unchanged and records
canonical content hashes and count deltas. The implementation does not infer
why scientific text changed, evaluate scientific correctness, or generate
replacement prose.
}
\seealso{
\code{new_manuscript_regeneration_plan()}
}
