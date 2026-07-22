# Canonical release gate

Phase 0.9.25 combines the canonical reproducibility controls into one deterministic, fail-closed release decision.

## Inputs

The gate accepts four independently produced evidence objects:

1. canonical validation-suite results;
2. canonical baseline-conformance results;
3. canonical drift assessments;
4. scientific change reconciliation results.

The default policy requires all four components. A custom policy may mark components optional for narrowly scoped development workflows, but production release workflows should retain the default.

## Decision model

A release is ready only when every required component passes.

- Validation passes when at least one approved dataset was executed and every dataset validation passed.
- Baseline conformance passes when all required quantitative comparisons passed.
- Stable drift passes directly.
- Non-stable drift passes only when the corresponding scientific change reconciliation is release ready.
- Reconciliation passes only when no unexpected, excessive, or missing expected scientific changes remain.
- Missing or malformed required evidence blocks the release.

Approval never changes measured validation, baseline, or drift results. It only determines whether independently measured drift is scientifically authorized.

## Evidence

`write_canonical_release_gate_evidence()` writes:

- `canonical_release_gate_components.tsv`;
- `canonical_release_gate_blocking.tsv`;
- `canonical_release_certificate.json`;
- `canonical_release_gate.md`.

The certificate records the release identifier, evaluation timestamp, required and passed components, blocking reasons, provenance, and final release-ready decision.

## CI policy

Regression tests use deterministic synthetic evidence and do not require network access or real canonical datasets. Production release certificates should be generated from approved full-validation evidence and retained with the scientific release bundle.
