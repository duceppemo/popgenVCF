# Phase 10.2.1 — Public API compatibility contracts

Phase 10.2.1 compares two canonical Phase 10 public API descriptors and produces deterministic, tamper-evident compatibility evidence.

## Classification policy

Each operation is classified as one of:

- **compatible** — operation, schemas, and lifecycle are unchanged;
- **additive** — a new operation is introduced or an existing schema advances within the same major version without reversing semantic-version order;
- **deprecated** — a stable operation enters the explicit deprecated lifecycle while retaining compatible schemas;
- **breaking** — an operation is removed, a schema name or major version changes, a schema version regresses, or lifecycle drift invalidates the established contract.

The overall descriptor classification is the most severe operation classification in the order compatible, additive, deprecated, breaking.

## Release gate

Compatibility records include baseline and candidate descriptor fingerprints, a canonical change table, an overall classification, and their own SHA-256 fingerprint. Validation fails closed when evidence is malformed, tampered with, or classified as breaking. A breaking record can only be validated with explicit `allow_breaking = TRUE`, which represents a separate reviewed release decision rather than silent acceptance.

## Architectural boundary

The comparator consumes the existing `phase10_api_descriptor()` registry. It does not create a second API registry, schema registry, executor, artifact system, provenance system, or report engine.
