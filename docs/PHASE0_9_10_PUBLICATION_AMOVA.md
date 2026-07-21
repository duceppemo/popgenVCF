# Phase 0.9.10: publication AMOVA outputs

Phase 0.9.10 adds deterministic publication contracts around authoritative AMOVA results.

The publication layer preserves variance-component tables, Phi statistics, permutation evidence, result fingerprints, optional figure-style bindings, machine-readable source data, captions, and reports. It validates ordering and identity constraints and fails closed when source data or fingerprints drift.

The layer does not recompute AMOVA statistics, rerun permutations, modify hierarchy definitions, or replace the authoritative AMOVA implementation.

## Initial public surface

- `new_publication_amova_spec()`
- `validate_publication_amova_spec()`
- `new_publication_amova_output()`
- `validate_publication_amova_output()`
- `publication_amova_caption()`
- `publication_amova_report()`

## Remaining integration work

The draft phase still requires generated exports and reference documentation, packaged policy documentation, and integration fixtures using canonical `PopgenVCF` AMOVA result helpers.