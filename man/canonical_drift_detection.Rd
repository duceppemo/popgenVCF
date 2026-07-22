\name{canonical_drift_detection}
\alias{new_canonical_drift_profile}
\alias{new_canonical_baseline_snapshot}
\alias{assess_canonical_baseline_drift}
\alias{canonical_drift_table}
\alias{canonical_drift_history}
\alias{canonical_drift_summary}
\alias{write_canonical_drift_evidence}
\title{Canonical baseline drift detection}
\description{Construct versioned baseline snapshots, assess pairwise and historical scientific drift, summarize classifications, and write deterministic evidence.}
\usage{
new_canonical_drift_profile(minor = 1, moderate = 2, major = 5)

new_canonical_baseline_snapshot(id, registry, recorded_at,
  provenance = list())

assess_canonical_baseline_drift(previous, current,
  profile = new_canonical_drift_profile())

canonical_drift_table(assessment)

canonical_drift_history(snapshots,
  profile = new_canonical_drift_profile())

canonical_drift_summary(assessment)

write_canonical_drift_evidence(assessment, output_dir)
}
\arguments{
  \item{minor}{Maximum normalized drift classified as minor.}
  \item{moderate}{Maximum normalized drift classified as moderate.}
  \item{major}{Maximum normalized drift classified as major.}
  \item{id}{Stable snapshot identifier.}
  \item{registry}{A canonical baseline registry.}
  \item{recorded_at}{ISO-8601 date or datetime.}
  \item{provenance}{Optional named provenance metadata.}
  \item{previous}{Earlier canonical baseline snapshot.}
  \item{current}{Later canonical baseline snapshot.}
  \item{profile}{Canonical drift threshold profile.}
  \item{assessment}{A canonical drift assessment or history.}
  \item{snapshots}{Explicitly ordered list of at least two baseline snapshots.}
  \item{output_dir}{Evidence output directory.}
}
\value{
Constructors return validated profile and snapshot objects. Pairwise assessment returns a \code{PopgenVCFCanonicalDriftAssessment}; historical assessment returns a \code{PopgenVCFCanonicalDriftHistory}. Table and summary functions return deterministic data frames. The evidence writer returns normalized output paths.
}
\details{
Numeric drift is normalized against the larger approved tolerance from the previous and current baseline versions. Default classifications are stable, minor, moderate, major, and breaking. Incompatible contracts and missing historical metrics are classified as breaking.
}
