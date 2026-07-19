\name{runtime_replay_verification}
\alias{new_runtime_replay_bundle}
\alias{verify_runtime_replay}
\title{Create and verify deterministic runtime replay bundles}
\usage{
new_runtime_replay_bundle(
  execution_ledger,
  attempt_ledger = NULL,
  process_results = list(),
  process_workspaces = list()
)

verify_runtime_replay(bundle)
}
\arguments{
\item{execution_ledger}{A validated \code{PopgenVCFExecutionLedger}.}
\item{attempt_ledger}{An optional validated \code{PopgenVCFAttemptLedger}.}
\item{process_results}{A list of validated external-process results.}
\item{process_workspaces}{A list of validated workspace provenance records.}
\item{bundle}{A \code{PopgenVCFRuntimeReplayBundle}.}
}
\value{
A verified \code{PopgenVCFRuntimeReplayBundle} containing a deterministic
\code{PopgenVCFRuntimeReplayVerification} report and replay fingerprint.
}
\description{
Builds a fail-closed cross-artifact replay graph. The verifier confirms that
execution and attempt ledgers describe the same modules and final states, retry
counters match contiguous retry chains, and process results map one-to-one to
workspace provenance through command fingerprints and process status.
}
\details{
Individual runtime artifacts remain responsible for their own checksum, schema,
and structural validation. This verifier adds the relationship boundary required
before replay, audit, diagnostics, or scientific reconstruction. Orphaned,
duplicated, cross-linked, or mutually inconsistent artifacts are rejected.
}
