\name{phase10_1_closure}
\alias{phase10_1_public_surface_manifest}
\alias{phase10_1_audit_public_surface}
\alias{phase10_1_closure_evidence}
\alias{phase10_1_closure_report}
\title{Audit and close the Phase 10.1 public API milestone}
\usage{
phase10_1_public_surface_manifest()

phase10_1_audit_public_surface(
  descriptor = phase10_api_descriptor(),
  exported_symbols = getNamespaceExports("popgenVCF"),
  available_symbols = ls(asNamespace("popgenVCF"), all.names = TRUE)
)

phase10_1_closure_evidence(
  ci_evidence_id,
  unresolved_blockers = character(),
  audit = phase10_1_audit_public_surface()
)

phase10_1_closure_report(closure)
}
\arguments{
  \item{descriptor}{A validated Phase 10 public API descriptor.}
  \item{exported_symbols}{Character vector of exported package symbols.}
  \item{available_symbols}{Character vector of namespace symbols.}
  \item{ci_evidence_id}{Immutable identity for the authoritative green CI run.}
  \item{unresolved_blockers}{Character vector of unresolved closure blockers.}
  \item{audit}{A successful Phase 10.1 public-surface audit.}
  \item{closure}{A Phase 10.1 closure evidence record.}
}
\value{
The manifest function returns a data frame. The audit and closure functions
return deterministic fingerprinted records. The report function returns
Markdown lines.
}
\description{
Verifies that all five stable Phase 10.1 operations, schemas, adapters, exports,
and lifecycle states agree exactly, then assembles deterministic closure
evidence and a roadmap handoff to Phase 10.2.
}
