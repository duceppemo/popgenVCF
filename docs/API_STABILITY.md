# API stability policy

popgenVCF follows semantic versioning.

## Public API

Only functions exported in `NAMESPACE`, documented configuration keys, the CLI flags shown by `--help`, and the top-level fields of `PopgenVCFAnalysis` are public interfaces.

## Compatibility promises

- Patch releases (`x.y.z`) fix defects without intentionally breaking public interfaces.
- Minor releases (`x.y.0`) may add functionality. Deprecated interfaces remain available for at least one subsequent minor release.
- Major releases may remove deprecated interfaces or alter documented behavior.
- Internal functions accessed with `:::` are unsupported and may change at any time.
- Canonical TSV schemas are versioned in the run manifest and data dictionary.
- Serialized analysis objects include `schema_version`; migrations will be provided when the schema changes.

## Statistical behavior

Changes that alter estimator definitions, filtering semantics, default thresholds, or sample inclusion rules are treated as breaking scientific changes and documented prominently in `NEWS.md`.
