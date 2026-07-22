\name{golden_outputs}
\alias{new_golden_spec}
\alias{new_golden_entry}
\alias{new_golden_store}
\alias{register_golden_entry}
\alias{validate_golden_entry}
\alias{compare_golden_outputs}
\alias{golden_output_table}
\alias{write_golden_store}
\alias{read_golden_store}
\alias{verify_golden_store}
\title{Golden-output scientific regression framework}
\usage{
new_golden_spec(id, mode = "exact", absolute_tolerance = 0,
  relative_tolerance = 0, role = "gating", metadata = list())

new_golden_entry(
  spec, value, approved_by = NA_character_,
  approval_reason = NA_character_,
  created_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
)

new_golden_store(entries = list(), metadata = list())

register_golden_entry(store, entry, replace = FALSE)

validate_golden_entry(x)

compare_golden_outputs(observed, store, ids = names(store$entries))

golden_output_table(x)

write_golden_store(store, path, overwrite = FALSE)

read_golden_store(path, verify = TRUE)

verify_golden_store(path)
}
\arguments{
  \item{id}{Stable output identifier.}
  \item{mode}{Comparison mode: exact, numeric, matrix, eigenspace, q_matrix, or manifest.}
  \item{absolute_tolerance, relative_tolerance}{Nonnegative comparison tolerances.}
  \item{role}{Either gating or diagnostic.}
  \item{metadata}{Additional named metadata.}
  \item{spec}{A golden specification.}
  \item{value}{Canonical output object.}
  \item{approved_by, approval_reason}{Approval metadata required for intentional replacement.}
  \item{created_at}{UTC timestamp.}
  \item{entries}{Optional list of golden entries.}
  \item{store}{A golden store.}
  \item{entry}{A golden entry.}
  \item{replace}{Permit an explicitly approved replacement.}
  \item{x}{A golden entry, store, or result.}
  \item{observed}{Named list of observed outputs.}
  \item{ids}{Optional identifiers to compare.}
  \item{path}{Golden-store directory.}
  \item{overwrite}{Permit replacement of an existing directory.}
  \item{verify}{Verify SHA256 checksums while reading.}
}
\value{Constructors return validated S3 objects. Comparison returns a \code{PopgenVCFGoldenResult}. Table conversion returns a data table. Store writing returns the normalized path invisibly; reading returns the validated store; verification returns \code{TRUE} invisibly.}
\description{Stores canonical scientific outputs with explicit comparison semantics, tolerances, approval metadata, deterministic serialization, and SHA256 integrity verification.}
