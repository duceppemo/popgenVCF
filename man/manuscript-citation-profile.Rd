\name{manuscript-citation-profile}
\alias{new_citation_profile}
\alias{validate_citation_profile}
\alias{set_manuscript_citation_profile}
\alias{manuscript_citation_keys}
\title{Configure reproducible manuscript citation metadata}
\usage{
new_citation_profile(style_id = "generic", csl = NULL)

validate_citation_profile(x)

set_manuscript_citation_profile(manuscript, profile)

manuscript_citation_keys(manuscript)
}
\arguments{
  \item{style_id}{Stable citation-style identity.}
  \item{csl}{Optional path to a Citation Style Language file.}
  \item{x}{A \code{PopgenVCFCitationProfile}.}
  \item{manuscript}{A validated \code{PopgenVCFManuscript}.}
  \item{profile}{A validated citation profile.}
}
\value{
\code{new_citation_profile()} returns a validated citation profile.
\code{validate_citation_profile()} returns \code{TRUE} invisibly.
\code{set_manuscript_citation_profile()} returns an updated manuscript.
\code{manuscript_citation_keys()} returns sorted unique BibTeX keys.
}
\description{
Creates and validates deterministic citation-style metadata, attaches it to a
canonical manuscript, and extracts stable bibliography identities for
Pandoc-ready manuscript source generation.
}
\details{
Custom CSL files are identified by SHA256 and copied into written manuscript
directories. Citation formatting never changes canonical bibliography keys or
scientific manuscript content.
}
