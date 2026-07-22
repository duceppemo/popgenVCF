\name{canonical_1000g_chrY_source}
\alias{canonical_1000g_chrY_source}
\alias{validate_canonical_source}
\alias{verify_canonical_source}
\alias{canonical_dataset_from_source}
\alias{approved_1000g_chrY_registry}
\alias{write_approved_canonical_source_evidence}
\title{Approved 1000 Genomes chromosome Y canonical dataset}
\usage{
canonical_1000g_chrY_source()

validate_canonical_source(source)

verify_canonical_source(source, directory)

canonical_dataset_from_source(source, directory)

approved_1000g_chrY_registry(
  directory,
  source = canonical_1000g_chrY_source()
)

write_approved_canonical_source_evidence(source, directory, output_dir)
}
\arguments{
  \item{source}{Canonical source specification containing dataset identity,
  review provenance, source URLs, and the upstream MD5 inventory.}

  \item{directory}{Directory containing the complete locally staged source
  file inventory.}

  \item{output_dir}{Directory where deterministic verification, registry, and
  canonical dataset evidence is written.}
}
\value{
\code{canonical_1000g_chrY_source()} returns a named source specification.
\code{validate_canonical_source()} returns \code{source} invisibly.
\code{verify_canonical_source()} returns a deterministic verification data
frame. \code{canonical_dataset_from_source()} returns a
\code{PopgenVCFCanonicalDataset}. \code{approved_1000g_chrY_registry()} returns
a registry containing one approved descriptor.
\code{write_approved_canonical_source_evidence()} returns named evidence paths.
}
\description{
Defines and validates the first approved real-data source used by popgenVCF.
Staged files are verified against the archive-published MD5 inventory before
being promoted into the package's SHA-256 canonical dataset contract.
}
