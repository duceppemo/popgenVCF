# Golden-output regression testing

Golden outputs protect curated scientific results from silent numerical or structural drift. They are not regenerated automatically during ordinary CI or release runs.

## Comparison modes

- `exact`: serialized R objects must be identical.
- `numeric`: vectors are compared with absolute and relative tolerances.
- `matrix`: dimensions and dimnames must match before tolerant numeric comparison.
- `eigenspace`: PCA/MDS axes are compared through canonical correlations, allowing sign changes and valid rotations.
- `q_matrix`: ancestry matrices are aligned for label switching before comparison.
- `manifest`: artifact or report manifests must match exactly.

Each specification is either `gating` or `diagnostic`. Only failed or errored gating comparisons fail release certification.

## Creating a store

```r
store <- new_golden_store(metadata = list(dataset = "synthetic-v1"))
store <- register_golden_entry(store, new_golden_entry(
  new_golden_spec("pca", "eigenspace", absolute_tolerance = 1e-8),
  pca_coordinates
))
store <- register_golden_entry(store, new_golden_entry(
  new_golden_spec("ibs", "matrix", absolute_tolerance = 1e-10),
  ibs_matrix
))
write_golden_store(store, "tests/golden/synthetic-v1")
```

The directory contains `store.rds`, one RDS file per entry, stable TSV/JSON metadata, and a SHA256 manifest.

## Intentional updates

Replacing an existing entry requires both an approver and a reason:

```r
replacement <- new_golden_entry(
  old_entry$spec,
  revised_output,
  approved_by = "maintainer-login",
  approval_reason = "Documented estimator correction in issue #123"
)
store <- register_golden_entry(store, replacement, replace = TRUE)
```

Reviewers should inspect the scientific rationale and the generated comparison table before accepting the changed baseline. A changed method should normally be accompanied by NEWS, validation, and manuscript-method updates.

## Release certification

Set `POPGENVCF_GOLDEN_STORE` to a verified store directory when running `scripts/build_release_benchmark_archive.R`. The runner compares the scientific-validation and population-structure outputs, writes `golden_output_comparison.tsv`, archives the complete result, and fails when a gating comparison fails.

Without this variable, the release summary reports `golden_output_status = not-configured`; the runner never creates or approves a baseline implicitly.
