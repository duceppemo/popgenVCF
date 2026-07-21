# Canonical validation datasets

Phase 0.9.13 defines deterministic contracts for licensed real-data validation datasets without redistributing restricted source data.

Each contract binds a stable dataset identifier and version to its authoritative source, license identifier, SHA-256 checksum, deterministic sample/population/locus inventories, expected scientific results, numerical tolerances, and optional external-tool comparison provenance.

The contract layer validates metadata and drift only. It does not download datasets, infer license terms, recompute expected results, or replace the scientific validation workflows that consume these records.

Tiny synthetic fixtures remain part of every CI run. Canonical real-data integration fixtures are opt-in and must fail closed when the source artifact, checksum, expected results, or comparison provenance changes.