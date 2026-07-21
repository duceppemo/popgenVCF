\name{publication_ordination_outputs}
\alias{new_publication_ordination_spec}
\alias{validate_publication_ordination_spec}
\alias{new_publication_ordination_output}
\alias{validate_publication_ordination_output}
\alias{publication_ordination_caption}
\alias{publication_ordination_report}
\title{Deterministic publication PCA and ordination outputs}
\usage{
new_publication_ordination_spec(
  method = "pca",
  axis_columns = c("PC1", "PC2"),
  sample_id_column = "sample_id",
  group_column = NULL,
  region = c("none", "centroid", "ellipse", "hull"),
  labels = FALSE,
  source_data_format = "tsv",
  version = "1.0.0"
)

validate_publication_ordination_spec(spec)

new_publication_ordination_output(
  spec,
  coordinates,
  metadata = NULL,
  variance_explained = NULL,
  loadings = NULL,
  result_fingerprint,
  figure_binding = NULL
)

validate_publication_ordination_output(output, spec)

publication_ordination_caption(output, spec)

publication_ordination_report(output, spec)
}
\arguments{
\item{method}{Stable PCA or ordination method name.}
\item{axis_columns}{Ordered coordinate columns to publish.}
\item{sample_id_column}{Coordinate and metadata sample identity column.}
\item{group_column}{Optional aligned metadata grouping column.}
\item{region}{Deterministic group-region policy.}
\item{labels}{Whether sample labels are requested.}
\item{source_data_format}{Stable machine-readable source-data format.}
\item{version}{Specification version.}
\item{spec}{A publication ordination specification.}
\item{coordinates}{Authoritative PCA or ordination coordinates.}
\item{metadata}{Optional sample metadata.}
\item{variance_explained}{Optional variance proportions or percentages.}
\item{loadings}{Optional loading matrix or data frame.}
\item{result_fingerprint}{Fingerprint of the authoritative scientific result.}
\item{figure_binding}{Optional publication figure-style binding.}
\item{output}{A publication ordination output.}
}
\value{
Constructors return fingerprinted publication ordination records. Validators return \code{TRUE} invisibly. Caption and report functions return deterministic character output.
}
\description{
Creates and validates deterministic publication contracts around authoritative PCA and ordination results, including stable sample ordering, metadata alignment, variance tables, loadings, group centroids, source-data exports, figure-style bindings, captions, provenance, and mutation detection.
}
