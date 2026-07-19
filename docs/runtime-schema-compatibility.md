# Execution-runtime schema compatibility

Phase 8 runtime objects are governed by explicit integer schema versions. The canonical registry is exposed by `runtime_schema_versions()` and covers execution plans, execution and attempt ledgers, checkpoints, scheduler metadata, resource policies, process results, process workspaces, and lifecycle events.

## Compatibility rules

- A **current** schema is accepted directly.
- A **legacy** schema is rejected by default and may be admitted only through an explicit migration path.
- An **unsupported future** schema is always rejected fail-closed because the installed package cannot safely infer newer semantics.
- Unknown runtime artifact kinds and malformed versions are always rejected.

Schema compatibility is independent from package serialization compatibility. A readable R object is not accepted merely because it can be deserialized; its runtime kind, schema version, required fields, integrity metadata, and semantic invariants must also validate.

## Migration boundary

This first Phase 8.10 increment establishes the registry and compatibility classifications. Follow-up increments will attach schema metadata to runtime objects, add ordered migrations for supported legacy versions, validate checksums and required references, and distinguish corruption from unsupported compatibility states.

Migrations must never reinterpret incomplete or scientifically invalid outputs as successful work. Unsupported, truncated, stale, or ambiguous records remain diagnostic only and cannot contribute accepted scientific results.
