# Phase 10.1 public analysis and artifact API

Phase 10.1 defines a stable public boundary above the deterministic Phase 8 runtime and canonical Phase 9 contracts.

## Stable operations

- `analysis.execute`
- `result.inspect`
- `artifact.list`
- `provenance.inspect`
- `report.render`

Use `phase10_api_descriptor()` and `phase10_api_operations()` for deterministic, version-aware discovery. Use `new_public_analysis_request()` and `new_public_analysis_response()` for canonical request and response envelopes.

Every public record has a deterministic SHA-256 fingerprint. Unsupported major versions, unknown operations, incompatible schemas, mutated records, internal runtime fields, and cross-request response substitution fail closed.

Public responses expose scientific values, artifact identities, provenance identities, warnings, and stable errors. They do not expose mutable executor, scheduler, worker, retry, process, checkpoint, migration, or deprecation internals.

`write_public_api_record()` and `read_public_api_record()` provide exact type-preserving round trips through a validated JSON envelope.

Existing low-level execution functions remain compatible. New integrations should treat the Phase 10 public contracts as their supported boundary and use adapters into the authoritative unified runtime rather than duplicating execution logic.