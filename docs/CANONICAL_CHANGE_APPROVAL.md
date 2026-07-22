# Canonical Scientific Change Approval

Phase 0.9.24 adds governance to the longitudinal drift framework introduced in Phase 0.9.23. Drift detection establishes what changed; change approval records whether that change was intentional, scientifically justified, and formally accepted.

## Change requests

A change request records stable metric identifiers, optional dataset scope, expected maximum drift classifications, scientific justification, requestor, decision metadata, provenance, and lifecycle status.

Supported states are:

- `pending`: proposed but not authorized;
- `approved`: eligible to reconcile observed drift;
- `rejected`: explicitly denied and unable to authorize drift;
- `superseded`: retained for audit history but no longer active.

Decided requests require both a decision maker and timestamp. Registries retain deterministic ordering and reject duplicate identifiers unless replacement is explicit.

## Reconciliation outcomes

Observed drift is matched only against approved requests:

- `approved_change`: observed severity is at or below the approved maximum;
- `exceeds_approval`: observed severity is greater than authorized;
- `unexpected_change`: non-stable drift has no active approval;
- `no_change`: stable output with no applicable approval;
- `missing_expected_change`: an approved change was expected but the metric remained stable or was absent from changed metrics.

Release readiness is fail-closed. A release is ready only when no unexpected or excessive drift exists and every approved expected change is observed.

## Evidence

`write_canonical_change_evidence()` writes deterministic metric reconciliation TSV, missing-expected-change TSV, release summary TSV, JSON, and Markdown methods text. Evidence contains request identifiers, expected classifications, scientific justifications, observed classifications, and release readiness.

## Scientific integrity

Approvals never modify measured drift. They annotate whether an independently calculated change is authorized. Pending, rejected, and superseded requests cannot legitimize a result. Historical records remain available for audit and reviewer-ready reporting.
