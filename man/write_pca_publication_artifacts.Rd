\name{write_pca_publication_artifacts}
\alias{write_pca_publication_artifacts}
\title{Write Publication-Ready PCA Artifacts}
\usage{
write_pca_publication_artifacts(
  coordinates,
  eigenvalues,
  metadata = NULL,
  output_dir,
  palette = NULL,
  module = "pca",
  variance_percent = NULL
)
}
\arguments{
\item{coordinates}{Data frame containing \code{sample_id}, \code{PC1}, and \code{PC2}.}
\item{eigenvalues}{Numeric PCA eigenvalues, for reference/diagnostic display only.}
\item{metadata}{Optional data frame containing \code{sample_id} and \code{population}.}
\item{output_dir}{Analysis output directory.}
\item{palette}{Optional named population colour vector.}
\item{module}{Artifact module name.}
\item{variance_percent}{Optional pre-computed percent-of-total-variance for
each component (same length/order as \code{eigenvalues}), on a 0-100 scale.
Required whenever \code{eigenvalues} is a truncated subset of a larger
spectrum (e.g. \code{SNPRelate::snpgdsPCA(eigen.cnt = N)}'s retained-only
eigenvalues): \code{eigenvalues / sum(eigenvalues)} is only the true percent
of TOTAL variance when \code{eigenvalues} is the complete spectrum, and
silently inflates every percentage otherwise. Defaults to \code{NULL}, which
falls back to treating \code{eigenvalues} as the complete spectrum --
correct only when it genuinely is.}
}
\value{A \code{PopgenVCFArtifactManifest}.}
\description{
Converts validated PCA coordinates and eigenvalues into canonical tables,
vector and raster figures, manuscript methods text, a figure caption, source
data, and machine-readable validation checks.
}
