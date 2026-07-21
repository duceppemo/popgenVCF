# Publication PCA and ordination outputs

Phase 0.9.5 adds deterministic publication contracts around authoritative PCA and ordination results. The contract does not recompute ordinations. It validates, normalizes, fingerprints, and publishes coordinates produced by the existing analysis runtime.

## Contract

`new_publication_ordination_spec()` records the method, ordered axes, sample identity field, optional grouping field, confidence-region policy, labeling policy, source-data format, version, and fingerprint.

`new_publication_ordination_output()` consumes authoritative coordinates and optional metadata, variance explained, loadings, and a publication figure-style binding. It produces stable sample ordering, aligned metadata, normalized variance percentages, optional loadings, deterministic group centroids, machine-readable source-data tables, result provenance, and a fingerprint.

## Fail-closed validation

Validation rejects duplicate or missing sample identities, metadata mismatches, non-finite coordinates or loadings, missing axes, invalid variance totals, insufficient figure-style capacity, source-data drift, specification drift, and mutation after construction.

## Publication integration

The output retains the authoritative scientific-result fingerprint and, when supplied, the figure-style binding fingerprint. The resulting source-data tables, caption, and report can be registered through the existing publication-artifact, report, supplementary-index, submission-package, lineage, and provenance contracts.

## Stable source data

Every rendered panel must derive from `output$source_data`. The scores table is identical to the validated publication scores, including deterministic sample ordering and aligned metadata. This identity is checked during validation so a figure cannot silently diverge from its exported source data.
