\name{canonical_release_gate}
\alias{new_canonical_release_gate_policy}
\alias{evaluate_canonical_release_gate}
\alias{canonical_release_gate_table}
\alias{canonical_release_certificate}
\alias{write_canonical_release_gate_evidence}
\title{Canonical scientific release gate}
\description{Combines canonical validation, baseline conformance, drift assessment, and scientific change reconciliation into one deterministic fail-closed release decision.}
\usage{
new_canonical_release_gate_policy(
  require_validation = TRUE,
  require_baselines = TRUE,
  require_drift = TRUE,
  require_reconciliation = TRUE
)

evaluate_canonical_release_gate(
  release_id,
  validation = NULL,
  baselines = NULL,
  drift = NULL,
  reconciliation = NULL,
  policy = new_canonical_release_gate_policy(),
  evaluated_at,
  provenance = list()
)

canonical_release_gate_table(result)

canonical_release_certificate(result)

write_canonical_release_gate_evidence(result, output_dir)
}
\arguments{
  \item{require_validation, require_baselines, require_drift, require_reconciliation}{Logical requirements for each evidence component.}
  \item{release_id}{Stable release candidate identifier.}
  \item{validation}{Optional canonical validation-suite result.}
  \item{baselines}{Optional canonical baseline result.}
  \item{drift}{Optional canonical drift assessment.}
  \item{reconciliation}{Optional canonical scientific change reconciliation.}
  \item{policy}{Canonical release gate policy.}
  \item{evaluated_at}{ISO-8601 evaluation timestamp.}
  \item{provenance}{Optional named provenance metadata.}
  \item{result}{Canonical release gate result.}
  \item{output_dir}{Evidence output directory.}
}
\value{A release gate policy, release gate result, deterministic component table, release certificate, or named evidence paths.}
