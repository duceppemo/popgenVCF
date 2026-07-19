\name{external_process_result_persistence}
\alias{validate_external_process_result}
\alias{write_external_process_result}
\alias{read_external_process_result}
\title{External process result integrity persistence}
\usage{
validate_external_process_result(result)

write_external_process_result(result, path, overwrite = FALSE)

read_external_process_result(path)
}
\arguments{
  \item{result}{A \code{PopgenVCFExternalProcessResult} object.}
  \item{path}{Destination or source \code{.rds} path.}
  \item{overwrite}{Whether existing result and sidecar files may be replaced.}
}
\value{
\code{validate_external_process_result()} returns its input invisibly.
\code{write_external_process_result()} returns the normalized path invisibly.
\code{read_external_process_result()} returns a validated
\code{PopgenVCFExternalProcessResult} object.
}
\description{
Validates, writes, and reads deterministic external-process execution records.
Persisted records are protected by versioned \code{process_result} runtime
integrity envelopes and whole-file SHA-256 sidecars. Validation covers the
embedded command and fingerprint, normalized status and exit-status semantics,
independent output streams, timestamps, elapsed time, executable resolution,
and diagnostic fields.
}
