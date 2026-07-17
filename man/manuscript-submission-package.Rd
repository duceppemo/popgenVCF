\name{manuscript-submission-package}
\alias{submission_package_plan}
\alias{write_submission_package}
\alias{verify_submission_package}
\title{Deterministic manuscript submission packages}
\usage{
submission_package_plan(manuscript_directory)

write_submission_package(manuscript_directory, path, overwrite = FALSE)

verify_submission_package(path)
}
\arguments{
  \item{manuscript_directory}{A validated manuscript source directory.}
  \item{path}{Destination or existing submission \code{.tar.gz} archive.}
  \item{overwrite}{Permit replacement of an existing archive.}
}
\value{
\code{submission_package_plan()} returns a stable data table. \code{write_submission_package()} invisibly returns a \code{PopgenVCFSubmissionPackage} record. \code{verify_submission_package()} invisibly returns \code{TRUE} or raises an error.
}
\description{
Plans, assembles, and verifies immutable manuscript submission archives with stable paths, semantic roles, provenance metadata, and SHA256 integrity checks.
}
