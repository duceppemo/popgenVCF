# Execution ledger integrity

Execution ledgers are persisted as versioned `execution_ledger` runtime integrity envelopes. A ledger is accepted only after every layer below succeeds.

1. The ledger file and SHA-256 sidecar both exist.
2. The sidecar contains one syntactically valid lowercase SHA-256 digest.
3. The serialized RDS file matches the sidecar digest.
4. The RDS payload can be deserialized completely.
5. The object is a runtime integrity envelope of kind `execution_ledger`.
6. The runtime schema is current and supported.
7. The envelope payload digest matches the ledger payload.
8. The payload is a `PopgenVCFExecutionLedger` data table.
9. Required columns, module identities, statuses, and optional attempt numbers satisfy ledger invariants.

Deserialization alone never implies acceptance. Corrupted, truncated, mutated, legacy unwrapped, malformed, or unsupported future ledger records fail closed.

## Stable API

- `new_execution_ledger()` creates a canonical ledger object.
- `validate_execution_ledger()` validates in-memory ledger invariants.
- `write_execution_ledger()` writes an atomic deterministic RDS envelope and SHA-256 sidecar.
- `read_execution_ledger()` verifies every integrity layer and returns the validated data table.

The serialized representation is deterministic for identical ledger inputs, allowing byte-for-byte replay comparisons and reproducible provenance checks.
