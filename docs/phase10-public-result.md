# Phase 10 public result inspection

`inspect_public_result()` implements the stable `result.inspect` operation over existing canonical result contracts.

The adapter validates core and ancestry results with the authoritative package validators and obtains primary scientific tables through `core_result_table()` or `ancestry_result_table()`. It returns the stable analysis name, canonical result class, validation summary, primary scientific table, artifact identities, and a deterministic result identity.

Parameters, raw provenance payloads, sample metadata, filesystem paths, software details, random seeds, runtime measurements, and mutable execution fields remain internal. Ancestry summary tables therefore omit the seed and runtime columns supplied by the lower-level operational record.

Malformed or unsupported result objects and requests for other operations fail closed with stable public error codes. The adapter does not define a second result schema, validator, artifact registry, or scientific table extractor.
