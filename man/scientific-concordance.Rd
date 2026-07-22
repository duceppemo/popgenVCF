\name{scientific-concordance}
\alias{new_scientific_concordance_record}
\alias{validate_scientific_concordance_record}
\alias{new_scientific_concordance_suite}
\alias{scientific_concordance_table}
\alias{write_scientific_concordance_evidence}
\title{Canonical external-tool scientific concordance}
\usage{
new_scientific_concordance_record(
  dataset_id, analysis, reference_tool, reference_version, command, result,
  tolerance_profile, environment = list(),
  approval = c("proposed", "approved"), approved_by = NULL, approved_at = NULL
)

validate_scientific_concordance_record(record, require_approved = FALSE)

new_scientific_concordance_suite(
  records, require_complete = TRUE,
  required_tools = character(), required_analyses = character()
)

scientific_concordance_table(suite)

write_scientific_concordance_evidence(
  suite, output_dir, require_release_ready = FALSE
)
}
\arguments{
\item{dataset_id}{Canonical dataset identifier.}
\item{analysis}{Analysis identifier.}
\item{reference_tool}{External implementation name.}
\item{reference_version}{External implementation version.}
\item{command}{Exact command or reproducible invocation.}
\item{result}{A \code{PopgenVCFExternalReferenceResult}.}
\item{tolerance_profile}{Named tolerance metadata.}
\item{environment}{Named environment and provenance metadata.}
\item{approval}{Proposed or approved scientific state.}
\item{approved_by, approved_at}{Approval metadata.}
\item{record}{A concordance record.}
\item{require_approved}{Require explicit scientific approval.}
\item{records}{A non-empty list of concordance records.}
\item{require_complete}{Require the declared tool and analysis inventory.}
\item{required_tools, required_analyses}{Required inventory entries.}
\item{suite}{A scientific concordance suite.}
\item{output_dir}{Evidence output directory.}
\item{require_release_ready}{Refuse finalization unless release ready.}
}
\description{
Creates deterministic, reviewable scientific concordance records and suites that
bind canonical analyses to external-tool versions, commands, tolerances,
comparison results, provenance, citations, and explicit approval.
}
\value{
A validated concordance record or suite, a deterministic summary table, or named
evidence paths depending on the function called.
}
