\name{write_ancestry_publication_artifacts}
\alias{write_ancestry_publication_artifacts}
\title{Write publication-ready ancestry artifacts}
\usage{
write_ancestry_publication_artifacts(
  ancestry,
  output_dir,
  metadata = NULL,
  k_selection = NULL,
  backend = NULL,
  k = NULL,
  replicate = NULL,
  palette = NULL,
  module = "ancestry"
)
}
\arguments{
\item{ancestry}{A \code{PopgenVCFAncestryReplicate}, \code{PopgenVCFAncestryConsensus}, or \code{PopgenVCFAncestryResult}.}
\item{output_dir}{Output directory.}
\item{metadata}{Optional data frame with \code{sample_id} and optionally \code{population}.}
\item{k_selection}{Optional \code{PopgenVCFKSelection} object.}
\item{backend}{Optional backend filter for ancestry result collections.}
\item{k}{Optional K filter for ancestry result collections.}
\item{replicate}{Optional replicate filter for ancestry result collections.}
\item{palette}{Optional cluster colour vector.}
\item{module}{Artifact module namespace.}
}
\value{A validated \code{PopgenVCFArtifactManifest}.}
\description{
Creates backend-neutral ancestry barplots, uncertainty and stability diagnostics,
K-selection figures, figure source tables, manuscript-ready methods and results
text, captions, and validation records. Population metadata is optional.
}
