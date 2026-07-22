# Canonical validation suites

Phase 0.9.21 adds deterministic orchestration across multiple approved canonical datasets.

A suite records a stable identifier, title, execution policy, materialized dataset directories, and optional dataset-specific scientific validation functions. Dataset identifiers are sorted before execution and must resolve to approved entries in a canonical dataset registry.

## Execution policy

`fail_fast = TRUE` stops after the first failed or errored dataset. `fail_fast = FALSE` records the failure and continues with the remaining approved datasets. Missing, unapproved, or corrupt datasets fail closed.

```r
suite <- new_canonical_validation_suite(
  id = "release_validation",
  title = "Release canonical validation",
  fail_fast = FALSE
)

suite <- register_canonical_validation(
  suite,
  id = "1000g_phase3_chry_v2a",
  directory = "canonical-source"
)

result <- run_canonical_validation_suite(suite, registry)
canonical_validation_suite_table(result)
```

## Dataset-specific scientific checks

A registered validation function receives the approved descriptor and materialized directory. It must return a data frame containing a logical `passed` column. The suite combines these checks with mandatory inventory, size, and SHA-256 verification.

## Aggregate evidence

`canonical_validation_suite_table()` reports status, elapsed time, file count, scientific check count, and errors for every executed dataset. `canonical_validation_coverage()` produces a deterministic dataset-by-analysis coverage table. `write_canonical_validation_suite()` writes a machine-readable TSV and concise methods statement suitable for scientific release evidence.

Routine CI remains synthetic and offline. Real-data suites continue to run only through dedicated full-validation workflows.
