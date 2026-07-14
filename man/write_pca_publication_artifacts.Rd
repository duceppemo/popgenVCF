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
  module = "pca"
)
}
\arguments{
\item{coordinates}{Data frame containing \code{sample_id}, \code{PC1}, and \code{PC2}.}
\item{eigenvalues}{Numeric PCA eigenvalues.}
\item{metadata}{Optional data frame containing \code{sample_id} and \code{population}.}
\item{output_dir}{Analysis output directory.}
\item{palette}{Optional named population colour vector.}
\item{module}{Artifact module name.}
}
\value{A \code{PopgenVCFArtifactManifest}.}
\description{
Converts validated PCA coordinates and eigenvalues into canonical tables,
vector and raster figures, manuscript methods text, a figure caption, source
data, and machine-readable validation checks.
}
