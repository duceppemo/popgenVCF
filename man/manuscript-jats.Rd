\name{manuscript-jats}
\alias{render_manuscript_jats}
\alias{write_manuscript_jats}
\alias{validate_manuscript_jats}
\title{Generate deterministic JATS manuscript XML}
\usage{
render_manuscript_jats(manuscript)

write_manuscript_jats(manuscript, directory = NULL, overwrite = FALSE)

validate_manuscript_jats(x)
}
\arguments{
  \item{manuscript}{A validated \code{PopgenVCFManuscript} or a written manuscript directory.}
  \item{directory}{Output directory when \code{manuscript} is an object.}
  \item{overwrite}{Permit replacing existing JATS output.}
  \item{x}{JATS XML text, a JATS file, or a \code{PopgenVCFJATSRecord}.}
}
\value{
\code{render_manuscript_jats()} returns JATS XML text.
\code{write_manuscript_jats()} returns a validated \code{PopgenVCFJATSRecord}.
\code{validate_manuscript_jats()} returns \code{TRUE} invisibly.
}
\description{
Creates a deterministic, offline JATS XML representation of canonical manuscript metadata, sections, contributors, artifacts, declarations, and citation identities.
}
\details{
The generated XML uses stable identifiers derived from canonical project, section, artifact, and citation identities. Journal-specific DTD validation and submission packaging are intentionally deferred.
}
