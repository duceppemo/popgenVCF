\name{canonical_real_data}
\alias{new_canonical_dataset}
\alias{validate_canonical_dataset}
\alias{verify_canonical_dataset}
\alias{materialize_canonical_dataset}
\alias{compare_external_results}
\alias{write_canonical_validation_evidence}
\title{Canonical real-data validation contracts}
\usage{
new_canonical_dataset(id, version, title, license, citation, files,
  organism = "unspecified", analyses = character(), metadata = list())

validate_canonical_dataset(x)

verify_canonical_dataset(descriptor, directory, strict_size = TRUE)

materialize_canonical_dataset(descriptor, destination, source_dir = NULL,
  allow_download = FALSE, quiet = TRUE)

compare_external_results(observed, reference, id_cols, value_cols,
  tolerance = 1e-8, tool, tool_version)

write_canonical_validation_evidence(descriptor, directory, output_dir,
  comparisons = NULL)
}
\arguments{
\item{id}{Stable dataset identifier.}
\item{version}{Dataset release version.}
\item{title}{Human-readable dataset title.}
\item{license}{SPDX identifier or explicit license label.}
\item{citation}{Dataset citation.}
\item{files}{Data frame containing \code{filename}, \code{sha256}, and optional \code{size_bytes} and \code{source}.}
\item{organism}{Organism label.}
\item{analyses}{Supported analysis identifiers.}
\item{metadata}{Additional named metadata.}
\item{x,descriptor}{A canonical dataset descriptor.}
\item{directory}{Directory containing materialized files.}
\item{strict_size}{Whether declared file sizes are enforced.}
\item{destination}{Destination directory.}
\item{source_dir}{Optional local mirror directory.}
\item{allow_download}{Whether explicit network downloads are permitted.}
\item{quiet}{Suppress download progress.}
\item{observed,reference}{Observed and external-reference result tables.}
\item{id_cols}{Columns used for deterministic alignment.}
\item{value_cols}{Numeric columns to compare.}
\item{tolerance}{Absolute numerical tolerance.}
\item{tool,tool_version}{External-tool provenance.}
\item{output_dir}{Evidence output directory.}
\item{comparisons}{Optional external-comparison table.}
}
\value{A validated descriptor, verification/comparison table, materialized directory, or named evidence paths, depending on the function.}
\description{Defines licensed, checksum-pinned canonical datasets; materializes them only from explicit sources; verifies their bytes deterministically; compares numerical results against versioned external tools; and writes machine-readable validation evidence.}
