\name{canonical_change_approval}
\alias{new_canonical_change_request}
\alias{new_canonical_change_registry}
\alias{register_canonical_change_request}
\alias{set_canonical_change_status}
\alias{reconcile_canonical_changes}
\alias{canonical_change_summary}
\alias{write_canonical_change_evidence}
\title{Canonical Scientific Change Approval}
\description{Create, govern, and reconcile scientifically approved changes against canonical baseline drift assessments.}
\usage{
new_canonical_change_request(id, metric_ids, expected_classifications,
  justification, status = "pending", dataset_ids = character(), requested_by,
  decided_by = NULL, decided_at = NULL, supersedes = NULL, provenance = list())

new_canonical_change_registry(requests = list())

register_canonical_change_request(registry, request, replace = FALSE)

set_canonical_change_status(registry, id, status, decided_by, decided_at)

reconcile_canonical_changes(assessment, registry)

canonical_change_summary(reconciliation)

write_canonical_change_evidence(reconciliation, output_dir)
}
\arguments{
\item{id}{Stable request identifier.}
\item{metric_ids}{Affected canonical metric identifiers.}
\item{expected_classifications}{Named expected maximum drift classifications.}
\item{justification}{Scientific justification.}
\item{status}{Lifecycle state.}
\item{dataset_ids}{Optional dataset scope.}
\item{requested_by}{Requestor identifier.}
\item{decided_by}{Decision maker identifier.}
\item{decided_at}{ISO-8601 decision timestamp.}
\item{supersedes}{Optional superseded request identifier.}
\item{provenance}{Named provenance metadata.}
\item{requests}{Optional request list.}
\item{registry}{Canonical change registry.}
\item{request}{Canonical change request.}
\item{replace}{Permit replacement of a duplicate request id.}
\item{assessment}{Canonical drift assessment.}
\item{reconciliation}{Canonical change reconciliation result.}
\item{output_dir}{Evidence output directory.}
}
\value{Validated governance objects, deterministic reconciliation tables, release summaries, or evidence paths.}
