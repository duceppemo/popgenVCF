# Phase 10.1 public analysis and artifact API

Phase 10.1 introduces a stable public boundary above the deterministic Phase 8 runtime and the canonical Phase 9 contracts. The boundary is intentionally narrow: users and downstream software receive scientific values, immutable artifact identities, provenance identities, warnings, and stable public errors without receiving mutable scheduler, worker, retry, process, checkpoint, migration, or deprecation internals.

## Stable operation families

| Operation | Purpose |
|---|---|
| `analysis.execute` | Submit a canonical analysis execution request. |
| `result.inspect` | Inspect canonical scientific results. |
| `artifact.list` | Discover publication and supplementary artifacts. |
| `provenance.inspect` | Inspect scientific provenance identities. |
| `report.render` | Request deterministic report rendering. |

Operation discovery is deterministic through `phase10_api_operations()`. Each operation has explicit request and response schemas and a lifecycle state.

## Version and compatibility policy

The initial public API version is `1.0.0`.

- requests for unsupported major versions fail closed;
- unknown operations fail closed;
- request and response schemas are bound to the selected operation;
- future major versions are rejected until explicitly supported;
- stable public errors contain only a code and a message;
- every descriptor, request, and response has a deterministic SHA-256 fingerprint.

## Request example

```r
request <- new_public_analysis_request(
  operation_id = "analysis.execute",
  analysis_id = "cohort-pca-001",
  parameters = list(module = "pca", components = 10L),
  input_ids = c(vcf = "sha256:...", metadata = "sha256:...")
)
```

Named parameters and input identities are canonicalized. Duplicate, unnamed, empty, unsupported, or internal fields are rejected.

## Response example

```r
response <- new_public_analysis_response(
  request = request,
  status = "completed",
  scientific_values = list(result_id = "result:pca-001"),
  artifact_ids = c(scores = "artifact:pca-scores"),
  provenance_ids = c(run = "provenance:pca-run-001")
)

inspect_public_analysis_response(response)
```

Successful and cached responses cannot contain errors. Failed and rejected responses require a stable public error record. Response validation can be bound to the originating request to prevent cross-request substitution.

## Serialization

`write_public_api_record()` writes a JSON envelope containing an exact, type-preserving, version-3 serialized record. `read_public_api_record()` validates both envelope identity and the canonical record fingerprint. This preserves named vectors, integer types, data-frame schemas, classes, and immutable identities across round trips.

## Migration guidance

Existing low-level execution functions remain available during Phase 10.1. New integrations should use the public request and response contracts as their compatibility boundary rather than depending directly on executor, scheduler, retry, process-supervision, checkpoint, migration, or deprecation record layouts.

The public API currently defines the stable contracts and discovery surface. Concrete dispatch adapters should translate public `analysis.execute` requests into the authoritative unified runtime and translate accepted runtime results back into public responses without duplicating execution logic.