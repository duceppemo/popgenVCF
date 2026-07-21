# Publication spatial-genetics outputs

Phase 0.9.12 adds deterministic publication contracts around authoritative spatial-genetics results.

The publication layer preserves validated sample coordinates, spatial-statistic summaries, neighborhood or distance-class evidence, permutation results, provenance fingerprints, source data, captions, and Markdown reports. It does not infer coordinates, recompute spatial statistics, fit neighborhood models, or rerun permutations.

## Deterministic guarantees

- sample coordinates are ordered by stable sample identity;
- spatial summaries, neighborhood tables, and permutation evidence are deterministically ordered;
- coordinates must be finite and sample identities must be unique;
- specifications and outputs are fingerprinted;
- source-data drift and output mutation fail closed;
- optional figure-style bindings are preserved by fingerprint.
