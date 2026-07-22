# Canonical dataset registry and approval gate

Phase 0.9.19 adds a deterministic registry around the Phase 0.9.18 canonical real-data descriptor and materialization contracts.

## Why approval is explicit

The repository previously contained an empty `public_reference` placeholder. This phase deliberately does not fill that placeholder with guessed URLs, licences, citations, or checksums. A public real-data dataset becomes executable only after an immutable release, analysis/redistribution licence, citation, complete SHA-256 inventory, and scientific review provenance are available.

Registry states are:

- `candidate`: metadata collection or scientific review is incomplete;
- `approved`: review provenance is complete and registry-mediated materialization is permitted;
- `deprecated`: retained for reproducibility but not selected for new validation runs;
- `rejected`: reviewed and explicitly unsuitable.

## Registry workflow

```r
registry <- new_canonical_dataset_registry()
registry <- register_canonical_dataset(
  registry,
  descriptor,
  approval = "candidate",
  notes = "Awaiting licence and checksum review"
)

list_canonical_datasets(registry)
```

Approval requires reviewer identity and an ISO-8601 review date:

```r
registry <- register_canonical_dataset(
  registry,
  descriptor,
  approval = "approved",
  reviewed_by = "scientific-review-board",
  reviewed_at = "2026-07-22",
  replace = TRUE
)
```

Only approved descriptors can pass through `materialize_registered_canonical_dataset()`. This prevents a candidate entry from being accidentally downloaded or included in release evidence.

## First public candidate

`validation/datasets.yml` now contains `first_public_canonical_candidate`. It is intentionally disabled and has null source, version, citation, licence, and checksum fields. Activation requires all declared approval conditions. This is a fail-closed scientific control, not an unfinished network dependency.

## Evidence

`write_canonical_dataset_registry()` writes a deterministic TSV containing dataset identity, version, organism, licence, approval state, review provenance, file count, and supported analyses. This table can be attached to later scientific release bundles.
