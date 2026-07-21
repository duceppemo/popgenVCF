\name{phase10_release_conformance}
\alias{new_phase10_release_identity}
\alias{new_phase10_release_conformance}
\alias{validate_phase10_release_conformance}
\alias{assert_phase10_release_conformance}
\alias{phase10_release_conformance_report}
\title{Phase 10 public API release conformance}
\usage{
new_phase10_release_identity(channel, release_version, artifact_digest,
  api_fingerprint)

new_phase10_release_conformance(descriptor, compatibility, migration_plan,
  identities, policy = phase10_api_evolution_policy(), allow_breaking = FALSE)

validate_phase10_release_conformance(manifest, descriptor, compatibility,
  migration_plan, policy = phase10_api_evolution_policy())

assert_phase10_release_conformance(manifest, descriptor, compatibility,
  migration_plan, policy = phase10_api_evolution_policy())

phase10_release_conformance_report(manifest, descriptor, compatibility,
  migration_plan, policy = phase10_api_evolution_policy())
}
\arguments{
\item{channel}{Required release channel identifier.}
\item{release_version}{Package release semantic version.}
\item{artifact_digest}{Stable channel-specific artifact digest.}
\item{api_fingerprint}{Canonical public API descriptor fingerprint.}
\item{descriptor}{Candidate public API descriptor.}
\item{compatibility}{Phase 10.2.1 compatibility record.}
\item{migration_plan}{Phase 10.2.2 migration plan.}
\item{identities}{Release-channel identity rows.}
\item{policy}{Phase 10.2.2 API evolution policy.}
\item{allow_breaking}{Whether a separately reviewed breaking release is approved.}
\item{manifest}{Release-conformance manifest.}
}
\value{
Identity construction returns a one-row data frame. Manifest construction returns
a \code{PopgenVCFPublicAPIReleaseConformance} object. Validation and assertion
return \code{TRUE} invisibly. Reporting returns Markdown lines.
}
\description{
Creates deterministic, fingerprinted release-conformance evidence and enforces
exact agreement across package, container, Apptainer, documentation, and
scientific-validation identities.
}
