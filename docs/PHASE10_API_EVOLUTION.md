# Phase 10.2.2 - API evolution policy and migration planning

Phase 10.2.2 turns Phase 10.2.1 compatibility evidence into deterministic migration guidance.

## Policy

- Additive changes remain release compatible and must be documented.
- Deprecated operations require a migration action, schema guidance, a deprecation version, and an earliest removal version.
- The default policy preserves a deprecated operation for at least two minor releases.
- Breaking changes require explicit compatibility approval and a documented migration or replacement path.
- Replacement guidance names the stable successor operation.
- Removal without prior deprecation is rejected by policy and remains a separately reviewed breaking release decision.

## Deterministic evidence

`phase10_api_evolution_policy()` creates the canonical fingerprinted policy. `new_phase10_migration_guidance()` creates normalized operation-level guidance. `new_phase10_api_migration_plan()` binds that guidance to the exact compatibility and policy fingerprints. `validate_phase10_api_migration_plan()` fails closed on missing paths, invalid schedules, missing successors, evidence mismatch, or tampering.

`phase10_api_migration_report()` renders the validated plan as deterministic Markdown.

## Architectural boundary

Migration planning consumes the canonical Phase 10 descriptor and Phase 10.2.1 compatibility record. It does not create another API registry, schema registry, executor, artifact system, provenance system, or report engine.
