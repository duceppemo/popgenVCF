# Publication isolation-by-distance outputs

Phase 0.9.11 adds deterministic publication contracts over authoritative isolation-by-distance results.

The publication layer preserves canonical pairwise genetic and geographic distances, regression summaries, permutation or Mantel evidence, result provenance, figure-style bindings, captions, and machine-readable source data. It does not calculate distances, fit regressions, or rerun permutations.

All records are fingerprinted. Validation fails closed when pair identities are malformed, distances are non-finite or negative, canonical pairs are duplicated, source data drifts, specification bindings differ, or fingerprints are mutated.
