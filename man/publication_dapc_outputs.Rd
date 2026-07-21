\name{publication_dapc_outputs}
\alias{new_publication_dapc_spec}
\alias{validate_publication_dapc_spec}
\alias{new_publication_dapc_output}
\alias{validate_publication_dapc_output}
\alias{publication_dapc_caption}
\alias{publication_dapc_report}
\title{Deterministic publication DAPC outputs}
\usage{
new_publication_dapc_spec(
  sample_id_column = "sample",
  population_column = "population",
  cluster_column = "cluster",
  axis_columns = c("LD1", "LD2"),
  selected_k = NULL,
  source_data_format = "tsv",
  version = "1.0.0"
)

validate_publication_dapc_spec(spec)

new_publication_dapc_output(
  spec,
  coordinates,
  membership,
  diagnostics = NULL,
  cross_validation = NULL,
  confusion = NULL,
  result_fingerprint,
  figure_binding = NULL
)

validate_publication_dapc_output(output, spec)

publication_dapc_caption(output, spec)

publication_dapc_report(output, spec)
}
\arguments{
\item{sample_id_column}{Sample identity column in the discriminant-coordinate table.}
\item{population_column}{Optional known-population column.}
\item{cluster_column}{Optional inferred-cluster column.}
\item{axis_columns}{Ordered discriminant-coordinate column names.}
\item{selected_k}{Optional selected number of clusters.}
\item{source_data_format}{Machine-readable source-data format, either \code{"tsv"} or \code{"csv"}.}
\item{version}{Specification version.}
\item{spec}{A validated publication DAPC specification.}
\item{coordinates}{Authoritative discriminant coordinates.}
\item{membership}{Authoritative membership probability matrix or data frame with sample row names.}
\item{diagnostics}{Optional authoritative per-K diagnostics.}
\item{cross_validation}{Optional authoritative cross-validation summary.}
\item{confusion}{Optional authoritative confusion matrix or long-form table.}
\item{result_fingerprint}{Fingerprint of the authoritative DAPC result.}
\item{figure_binding}{Optional publication figure-style binding.}
\item{output}{A publication DAPC output.}
}
\value{
\code{new_publication_dapc_spec()} returns a fingerprinted
\code{PopgenVCFPublicationDAPCSpec}. \code{new_publication_dapc_output()}
returns a fingerprinted \code{PopgenVCFPublicationDAPCOutput}. Validation
functions return \code{TRUE} invisibly. \code{publication_dapc_caption()}
returns one manuscript-ready caption string, and
\code{publication_dapc_report()} returns Markdown report lines.
}
\description{
Creates and validates deterministic publication-layer contracts around
existing authoritative DAPC results. The publication layer preserves,
orders, fingerprints, reports, and exports discriminant coordinates,
membership probabilities, diagnostics, cross-validation summaries,
confusion data, and selected-K metadata without recomputing DAPC.
}
\details{
Coordinates are ordered deterministically by sample identity. Membership
probabilities must align exactly with coordinate sample identities, contain
finite values between zero and one, and sum to one for each sample. Optional
figure-style bindings are validated for sufficient membership-group capacity.
Specification and output fingerprints provide fail-closed mutation and drift
detection.
}
\examples{
spec <- new_publication_dapc_spec(selected_k = 2L)
coordinates <- data.frame(
  sample = c("sample_b", "sample_a"),
  LD1 = c(1.2, -0.8),
  LD2 = c(0.4, -0.2)
)
membership <- data.frame(
  cluster_1 = c(0.9, 0.1),
  cluster_2 = c(0.1, 0.9),
  row.names = c("sample_a", "sample_b")
)
output <- new_publication_dapc_output(
  spec,
  coordinates,
  membership,
  result_fingerprint = "example-dapc-result"
)
publication_dapc_caption(output, spec)
publication_dapc_report(output, spec)
}
