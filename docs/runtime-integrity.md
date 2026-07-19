# Runtime integrity envelopes

Phase 8.10 protects persisted execution-runtime payloads with deterministic integrity envelopes.

Each envelope records:

- the registered artifact kind;
- canonical runtime schema metadata;
- the SHA-256 digest algorithm;
- a serialized payload digest;
- the original payload.

Validation is deliberately fail closed. A payload is not returned unless the envelope is complete, the artifact kind matches its schema metadata, the schema is compatible, the digest algorithm is supported, and the digest exactly matches the payload.

Changing a payload after envelope construction invalidates the envelope. Unsupported future schemas are rejected before extraction. Legacy schemas require explicit opt-in and still require a separately implemented migration before they may contribute accepted results.

This generic contract does not yet change checkpoint or ledger serialization. Subsequent Phase 8.10 increments will integrate envelopes into persisted checkpoints, execution records, scheduler metadata, process results, and workspaces.