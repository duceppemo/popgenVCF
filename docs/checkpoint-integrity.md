# Execution checkpoint integrity

Execution checkpoints are accepted only after a layered, fail-closed validation sequence. Each layer protects a different boundary and no single successful check is treated as sufficient evidence that checkpoint state is safe to reuse.

## Persistence format

`write_execution_checkpoint()` validates the checkpoint object, wraps it in a runtime integrity envelope of kind `checkpoint`, serializes the envelope as an RDS file, and writes a SHA-256 sidecar for the complete serialized file.

The persisted envelope contains:

- the artifact kind;
- canonical runtime schema metadata;
- the digest algorithm;
- a SHA-256 digest of the checkpoint payload;
- the checkpoint payload itself.

The checkpoint payload retains its existing internal `checkpoint_digest`. This deliberately creates three independent integrity layers rather than replacing earlier protections.

## Read validation order

`read_execution_checkpoint()` validates from the outermost persistence boundary inward:

1. both the checkpoint file and SHA-256 sidecar must exist;
2. the sidecar must contain a checksum;
3. the checksum of the serialized checkpoint file must match the sidecar;
4. the RDS file must deserialize successfully;
5. the deserialized object must be a runtime integrity envelope;
6. the envelope schema must be supported;
7. the envelope kind must be `checkpoint`;
8. the envelope payload digest must match;
9. the checkpoint internal digest and structural invariants must pass;
10. the analysis and artifact manifest must validate;
11. when supplied, the current registry must match the checkpoint plan and module contracts.

Only after all checks pass is the checkpoint payload returned.

## Failure classification

The checkpoint reader distinguishes the following classes of invalid persistence:

- missing file or sidecar;
- malformed sidecar;
- whole-file checksum mismatch;
- unreadable or truncated RDS content;
- legacy unwrapped checkpoint requiring migration;
- unsupported future runtime schema;
- malformed integrity envelope;
- envelope payload digest mismatch;
- checkpoint internal digest mismatch;
- checkpoint invariant or registry compatibility failure.

All failures stop before any module output is reused.

## Compatibility boundary

Plain checkpoint objects written by earlier versions are not silently accepted. They are classified as legacy unwrapped checkpoints and require an explicit migration path. Future migration code must validate the legacy object before transformation, record the source and target schema versions, construct a current envelope, and revalidate the migrated checkpoint under the current runtime contract.

## Determinism

For the same validated checkpoint payload and package runtime, checkpoint serialization is deterministic. Repeated writes produce identical serialized bytes and identical SHA-256 checksums. This property is covered by regression tests and supports reproducible archives and deterministic checkpoint comparison.
