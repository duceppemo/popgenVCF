\name{canonical-real-data-baseline}
\alias{new_canonical_real_data_baseline_snapshot}
\alias{validate_canonical_real_data_baseline_snapshot}
\alias{read_canonical_real_data_baseline_snapshot}
\alias{approve_canonical_real_data_baseline_snapshot}
\alias{write_canonical_real_data_baseline_snapshot}
\title{Canonical real-data baseline snapshots}
\usage{
new_canonical_real_data_baseline_snapshot(
  dataset,
  registry,
  sample_metadata,
  dataset_version,
  generated_by,
  generated_at,
  source_commit,
  approval = c("proposed", "approved"),
  approved_by = NULL,
  approved_at = NULL,
  notes = NULL
)

validate_canonical_real_data_baseline_snapshot(
  snapshot,
  require_approved = FALSE
)

read_canonical_real_data_baseline_snapshot(path, require_approved = FALSE)

approve_canonical_real_data_baseline_snapshot(
  snapshot,
  approved_by,
  approved_at,
  notes = snapshot$notes
)

write_canonical_real_data_baseline_snapshot(
  snapshot,
  path,
  require_approved = FALSE
)
}
\arguments{
\item{dataset}{A validated \code{PopgenVCFCanonicalDataset}.}
\item{registry}{A validated \code{PopgenVCFCanonicalBaselineRegistry}.}
\item{sample_metadata}{Complete sample metadata containing sample, population, superpopulation, and sex fields.}
\item{dataset_version}{Version of the acquired canonical dataset.}
\item{generated_by}{Non-empty description of the generating workflow.}
\item{generated_at}{ISO-8601 UTC timestamp.}
\item{source_commit}{Full 40-character lowercase Git commit SHA.}
\item{approval}{Either \code{proposed} or \code{approved}.}
\item{approved_by}{Reviewer identity, required for approved snapshots.}
\item{approved_at}{ISO-8601 review date, required for approved snapshots.}
\item{notes}{Optional review notes.}
\item{snapshot}{A canonical real-data baseline snapshot.}
\item{require_approved}{Fail unless the snapshot has approved status.}
\item{path}{Destination JSON path.}
}
\value{
The constructor and reader return a validated \code{PopgenVCFCanonicalRealDataBaselineSnapshot}. The approval helper returns a validated approved snapshot, the validator returns the snapshot invisibly, and the writer returns the normalized output path.
}
\description{
Creates, validates, reads, explicitly approves, and serializes deterministic production baseline snapshots that bind an approved canonical dataset, filename-bound source checksums, complete sample metadata, quantitative metrics, workflow provenance, and scientific approval metadata.
}
