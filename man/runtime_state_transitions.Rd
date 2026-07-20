\name{runtime_state_transitions}
\alias{runtime_state_transition_matrix}
\alias{validate_runtime_state_transition}
\alias{validate_runtime_state_history}
\alias{assemble_runtime_replay_from_envelopes}
\title{Validate runtime state transitions and assemble migrated replay bundles}
\usage{
runtime_state_transition_matrix()

validate_runtime_state_transition(from, to)

validate_runtime_state_history(statuses, module = NULL)

assemble_runtime_replay_from_envelopes(envelopes, migration_registry)
}
\arguments{
  \item{from}{Source runtime status.}
  \item{to}{Target runtime status.}
  \item{statuses}{Ordered runtime statuses for one module.}
  \item{module}{Optional module identity used in diagnostics.}
  \item{envelopes}{List of runtime integrity envelopes.}
  \item{migration_registry}{Runtime migration registry.}
}
\value{
The matrix helper returns a logical transition matrix. Validation helpers return their input invisibly. Replay assembly returns a \code{PopgenVCFRuntimeReplayAssembly} containing a verified replay bundle, canonical migration records, and an assembly fingerprint.
}
\description{
Defines the fail-closed runtime state machine and the explicit boundary that migrates every legacy runtime envelope before current-schema validation and cross-artifact replay verification.
}
