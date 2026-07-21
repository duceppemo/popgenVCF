# Phase 10 public result inspection

`inspect_public_result()` implements `result.inspect` over existing canonical core and ancestry result contracts.

The response contains stable scientific tables, validation summaries, artifact identities, and a deterministic result identity. Parameters, raw provenance, metadata, paths, software details, seeds, runtimes, and execution internals are excluded.

The adapter delegates validation and table extraction to existing package functions and fails closed for unsupported or malformed objects.
