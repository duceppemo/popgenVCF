\name{manuscript_cross_reference_table}
\alias{manuscript_cross_reference_table}
\title{Build the manuscript artifact cross-reference table}
\usage{
manuscript_cross_reference_table(manuscript)
}
\arguments{
  \item{manuscript}{A validated \code{PopgenVCFManuscript}.}
}
\value{
A deterministic data table linking manuscript labels, stable anchors,
relative paths, captions, and immutable artifact identifiers.
}
\description{
Builds the canonical artifact-reference table used by manuscript Markdown
rendering and portable manuscript source directories.
}
\details{
Reference anchors depend only on artifact category and immutable artifact
identifier. Supported image formats are marked as directly embeddable;
other artifacts are represented as deterministic links.
}
