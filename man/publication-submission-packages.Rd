\name{publication-submission-packages}
\alias{new_publication_submission_package_spec}
\alias{validate_publication_submission_package_spec}
\alias{new_publication_supplementary_index}
\alias{validate_publication_supplementary_index}
\alias{new_publication_submission_package_manifest}
\alias{validate_publication_submission_package_manifest}
\alias{publication_submission_package_report}
\title{Deterministic publication submission packages}
\usage{
new_publication_submission_package_spec(
  id,
  journal_profile,
  layout_profile,
  figure_style,
  report_spec,
  archive_format = c("zip", "tar.gz"),
  root_directory = "submission",
  required_roles = c("manuscript", "metadata", "provenance"),
  version = "1.0.0"
)

validate_publication_submission_package_spec(
  spec,
  journal_profile,
  layout_profile,
  figure_style,
  report_spec
)

new_publication_supplementary_index(supplements = list())

validate_publication_supplementary_index(index)

new_publication_submission_package_manifest(
  spec,
  files,
  supplementary_index = new_publication_supplementary_index(),
  output_manifest_fingerprint,
  execution_fingerprint
)

validate_publication_submission_package_manifest(
  manifest,
  spec,
  supplementary_index
)

publication_submission_package_report(manifest)
}
\arguments{
  \item{id}{Stable package identifier.}
  \item{journal_profile}{Validated journal profile.}
  \item{layout_profile}{Validated publication layout profile.}
  \item{figure_style}{Validated publication figure-style profile.}
  \item{report_spec}{Validated publication report specification.}
  \item{archive_format}{Archive format, either \code{zip} or \code{tar.gz}.}
  \item{root_directory}{Stable archive root directory.}
  \item{required_roles}{Required logical file roles.}
  \item{version}{Specification version.}
  \item{spec}{A submission-package specification.}
  \item{supplements}{Supplementary entries with package and manuscript metadata.}
  \item{index}{A supplementary-material index.}
  \item{files}{Package file entries.}
  \item{supplementary_index}{Validated supplementary-material index.}
  \item{output_manifest_fingerprint}{Fingerprint of the originating report output manifest.}
  \item{execution_fingerprint}{Fingerprint of the originating report execution.}
  \item{manifest}{A package manifest.}
}
\value{
Constructors return deterministic fingerprinted records. Validators return
\code{TRUE} invisibly. \code{publication_submission_package_report()} returns
Markdown report lines.
}
\description{
Creates and validates journal-ready package specifications, supplementary
indexes, and package manifests with deterministic ordering, checksums,
publication-contract bindings, and fail-closed mutation detection.
}
