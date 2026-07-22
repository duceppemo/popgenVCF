# Canonical drift detection

Phase 0.9.23 adds longitudinal scientific drift assessment across versioned canonical baseline snapshots.

A snapshot binds a stable release identifier and recording date to a validated canonical baseline registry. Drift is assessed only between metrics sharing the same identifier, dataset, analysis, and comparator. Incompatible metric contracts, removed metrics, and metrics introduced without historical baselines fail closed as `breaking`.

## Classification model

Numeric drift is expressed in approved tolerance units:

```
normalized drift = baseline change magnitude / max(previous tolerance, current tolerance)
```

The default profile classifies normalized drift as:

- `stable`: zero change;
- `minor`: greater than zero and at most 1 tolerance unit;
- `moderate`: at most 2 tolerance units;
- `major`: at most 5 tolerance units;
- `breaking`: greater than 5 tolerance units, non-finite drift, or an incompatible contract.

Threshold boundaries use the same scale-aware floating-point guard as canonical baseline comparisons. Profiles are explicit objects and may be replaced with scientifically justified project-specific thresholds.

## Pairwise assessment

```r
previous <- new_canonical_baseline_snapshot(
  "release-1", previous_registry, "2026-07-01"
)
current <- new_canonical_baseline_snapshot(
  "release-2", current_registry, "2026-07-22"
)

assessment <- assess_canonical_baseline_drift(previous, current)
canonical_drift_table(assessment)
canonical_drift_summary(assessment)
```

Absolute, relative, and distribution metrics reuse their baseline comparison scales. Exact and set metrics are stable when unchanged and breaking when changed without a non-zero quantitative tolerance model.

## Historical trajectories

`canonical_drift_history()` compares every adjacent pair in an explicitly ordered snapshot list. It records transition identifiers and sums finite normalized drift per metric to provide cumulative trajectories without hiding breaking or missing-history events.

## Evidence

`write_canonical_drift_evidence()` writes:

- `canonical_drift_metrics.tsv`;
- `canonical_drift_summary.tsv`;
- `canonical_drift.json`;
- `canonical_drift_methods.md`.

Rows are deterministically ordered by dataset, analysis, and metric. Evidence records baseline versions, raw magnitude, normalized drift, classification, and the reason for fail-closed decisions. Routine tests remain synthetic and offline; approved real-data histories remain governed by full-validation workflows.
