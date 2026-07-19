\name{execution-ledger-persistence}
\alias{new_persisted_execution_ledger}
\alias{validate_execution_ledger}
\alias{write_execution_ledger}
\alias{read_execution_ledger}
\title{Persist and validate execution ledgers}
\usage{
new_persisted_execution_ledger(ledger)

validate_execution_ledger(ledger)

write_execution_ledger(ledger, path, overwrite = FALSE)

read_execution_ledger(path)
}
\arguments{
  \item{ledger}{A data frame containing execution records, or a validated \code{PopgenVCFExecutionLedger}.}
  \item{path}{Destination or source \code{.rds} path.}
  \item{overwrite}{Whether an existing ledger and checksum sidecar may be replaced.}
}
\value{
\code{new_persisted_execution_ledger()} and \code{read_execution_ledger()} return a validated
\code{PopgenVCFExecutionLedger} data table. \code{validate_execution_ledger()} returns its input
invisibly. \code{write_execution_ledger()} returns the normalized output path invisibly.
}
\description{
Creates, validates, writes, and reads execution ledgers protected by versioned runtime integrity
envelopes and whole-file SHA-256 sidecars. Loading fails closed when file integrity, schema
compatibility, envelope integrity, or ledger invariants do not validate.
}
