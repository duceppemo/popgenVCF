# Phase 9 closure evidence and roadmap handoff

Phase 9 closure is a review decision, not an inference from merged pull requests.
The package therefore assembles a deterministic evidence bundle and remains
fail closed until all release-readiness gates pass.

## Milestone lineage

`phase9_milestone_manifest()` binds milestones 9.1 through 9.14 to their merged
implementation commits. These bindings are immutable inputs to the closure
fingerprint and prevent a roadmap entry from being treated as evidence without
an identifiable implementation.

## Required evidence

`phase9_closure_evidence_manifest()` enumerates the required domains: schemas,
runtime, cache, replay, checkpointing, recovery, provenance, validation,
publication, performance, documentation, and migration.

Evidence identities refer to canonical Phase 9 records. Production closure must
also provide identities for the Phase 9.13 release-readiness record, production
migration registry, deprecation portfolio, and the CI evidence set used for the
closure decision.

## Assembly

Use `phase9_assemble_closure()` with immutable evidence identities. Setting
`release_ready = TRUE` does not override blockers. Closure is approved only when
release readiness is true and `unresolved_blockers` is empty.

```r
bundle <- phase9_assemble_closure(
  release_readiness_id = "release-readiness:<fingerprint>",
  migration_registry_id = "migration-registry:<fingerprint>",
  deprecation_portfolio_id = "deprecation-portfolio:<fingerprint>",
  ci_evidence_id = "ci-evidence:<fingerprint>",
  release_ready = TRUE,
  unresolved_blockers = character()
)

writeLines(phase9_closure_report(bundle))
```

The resulting bundle contains the closure review and a deterministic handoff to
Phase 10.1. Deferred work and acknowledged risks remain visible and cannot be
mistaken for completed Phase 9 scope.

## Phase 10.1 entry gate

Phase 10.1 begins only after:

- the Phase 9 closure review is approved;
- CI and scientific-validation evidence is immutable;
- migration blockers are empty; and
- rollback and compatibility obligations are documented.

Phase 10.1 defines the canonical public analysis and artifact API. It exposes
the unified runtime through stable module, result, artifact, report, and
provenance interfaces. It does not introduce new statistical methods or a
parallel execution runtime.
