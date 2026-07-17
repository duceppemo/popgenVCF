\name{manuscript-generation}
\alias{new_manuscript}
\alias{validate_manuscript}
\alias{render_manuscript_markdown}
\alias{write_manuscript}
\title{Automatic manuscript generation}
\usage{
new_manuscript(
  project,
  publication = NULL,
  title = project$name,
  authors = NULL,
  abstract = NULL,
  keywords = character(),
  introduction = NULL,
  results = NULL,
  discussion = NULL,
  declarations = list()
)

validate_manuscript(x)

render_manuscript_markdown(manuscript)

write_manuscript(manuscript, directory, overwrite = FALSE)
}
\arguments{
  \item{project}{A validated \code{PopgenVCFProject}.}
  \item{publication}{A \code{PopgenVCFPublicationBundle}; generated from \code{project} when omitted.}
  \item{title}{Manuscript title.}
  \item{authors}{Author metadata containing at least a \code{name} column and optionally affiliation, email, ORCID, and corresponding-author status.}
  \item{abstract}{Author-supplied abstract.}
  \item{keywords}{Character vector of manuscript keywords.}
  \item{introduction}{Author-supplied introduction.}
  \item{results}{Author-supplied scientific interpretation for Results.}
  \item{discussion}{Author-supplied discussion.}
  \item{declarations}{Named overrides for generated declaration text.}
  \item{x}{A \code{PopgenVCFManuscript} or generated manuscript directory.}
  \item{manuscript}{A validated \code{PopgenVCFManuscript}.}
  \item{directory}{Output directory.}
  \item{overwrite}{Whether a non-empty output directory may be replaced.}
}
\value{
\code{new_manuscript()} returns a validated \code{PopgenVCFManuscript}. \code{render_manuscript_markdown()} returns deterministic Markdown source. \code{write_manuscript()} invisibly returns the normalized output directory. Validation returns \code{TRUE} invisibly.
}
\description{
Construct, validate, render, and write deterministic manuscript sources from canonical popgenVCF project and publication records. Generated factual content is kept separate from author-supplied scientific interpretation.
}
