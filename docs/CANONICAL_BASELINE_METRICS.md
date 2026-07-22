# Canonical baseline metrics

Phase 0.9.22 adds versioned quantitative scientific baselines to the canonical validation framework.

A baseline metric binds a stable metric identifier to a canonical dataset, analysis, expected value, comparator, tolerance, version, scientific rationale, and provenance. Baselines are stored in deterministic registries and evaluated from named observed values.

## Comparators

- `exact` requires identical value and R type.
- `absolute` compares the maximum absolute element-wise deviation with an inclusive tolerance.
- `relative` compares the maximum element-wise relative deviation with an inclusive tolerance.
- `set` compares unordered membership and reports symmetric-difference size.
- `distribution` compares deciles and reports the maximum deviation normalized by the expected range.

Every tolerance must be non-negative and every metric must include a rationale. This makes numerical acceptance criteria reviewable scientific records rather than hidden implementation constants.

## Example

```r
metric <- new_canonical_baseline_metric(
  id = "pca_pc1_variance",
  dataset_id = "1000g_phase3_chry_v2a",
  analysis = "pca",
  expected = 0.1842,
  comparator = "absolute",
  tolerance = 1e-4,
  version = "2026.1",
  rationale = "Allows platform-level floating-point variation",
  provenance = list(reference_release = "popgenVCF-0.10.0")
)

registry <- new_canonical_baseline_registry(list(metric))
result <- evaluate_canonical_baselines(
  registry,
  list(pca_pc1_variance = 0.18419)
)
canonical_baseline_table(result)
```

## Suite integration

`canonical_baseline_validation()` converts a baseline registry and an observation function into a callback accepted by `register_canonical_validation()`. The callback contributes one suite check per baseline metric, so inventory integrity and scientific numerical stability are evaluated in the same dataset result.

## Evidence

`write_canonical_baseline_evidence()` writes:

- `canonical_baseline_metrics.tsv` for tabular inspection;
- `canonical_baseline_metrics.json` for machine-readable release evidence;
- `canonical_baseline_methods.md` for publication and audit records.

The comparison table records expected and observed values, comparator, tolerance, deviation, pass/fail status, baseline version, detail, and scientific rationale.

Routine tests remain fully offline and use deterministic fixtures. Real-data expected values should be approved and versioned only after the corresponding full canonical workflow has been executed and scientifically reviewed.
