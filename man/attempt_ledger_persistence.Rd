\name{attempt_ledger_persistence}
\alias{new_attempt_ledger}
\alias{validate_attempt_ledger}
\alias{write_attempt_ledger}
\alias{read_attempt_ledger}
\title{Attempt ledger integrity persistence}
\usage{
new_attempt_ledger(ledger)

validate_attempt_ledger(ledger)

write_attempt_ledger(ledger, path, overwrite = FALSE)

read_attempt_ledger(path)
}
\arguments{
  \item{ledger}{A data frame containing one row per module attempt, or a canonical \code{PopgenVCFAttemptLedger}.}
  \item{path}{Destination or source \code{.rds} path.}
  \item{overwrite}{Whether existing ledger and sidecar files may be replaced.}
}
\value{
\code{new_attempt_ledger()} and \code{read_attempt_ledger()} return a validated
\code{PopgenVCFAttemptLedger} data table. \code{validate_attempt_ledger()} returns
its input invisibly. \code{write_attempt_ledger()} returns the normalized path
invisibly.
}
\description{
Creates, validates, writes, and reads deterministic retry-attempt histories.
Persisted ledgers are protected by versioned runtime integrity envelopes and
whole-file SHA-256 sidecars. Validation requires non-empty module identities,
supported statuses, positive attempt numbers, unique module-attempt pairs,
contiguous retry chains, and no attempts after terminal states.
}
