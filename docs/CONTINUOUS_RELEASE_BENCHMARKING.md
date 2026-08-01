# Continuous release benchmarking

Phase 0.9.29 promotes performance measurements to deterministic, reviewable release evidence.

## Observation contract

A continuous benchmark observation records:

- a stable benchmark, module, and dataset-tier identifier;
- release identity and full Git commit SHA;
- wall-clock runtime and peak memory;
- throughput and thread-scaling efficiency;
- thread count and repetition count;
- a deterministically ordered environment fingerprint.

Dataset tiers are `synthetic`, `canonical`, `medium`, and `large`. Pull-request CI should use fast synthetic observations. Canonical and larger tiers belong in scheduled, opt-in, or release workflows.

## Performance budgets

`new_release_performance_budget()` defines explicit release limits for runtime, memory, throughput, scaling efficiency, and the minimum evidence count. Budgets are versionable scientific-release policy rather than hard-coded CI percentages.

`compare_continuous_release_benchmark()` classifies evidence as:

- `passed` when every budget check passes;
- `failed` when an adequately repeated benchmark exceeds a budget;
- `insufficient-evidence` when too few repetitions were collected.

Insufficient evidence never produces a release-ready certificate. This prevents noisy or incomplete measurements from becoming authoritative.

## Evidence artifacts

`write_continuous_benchmark_evidence()` writes deterministic:

- `continuous_benchmarks.tsv`;
- `continuous_benchmarks.json`;
- `continuous_benchmark_summary.md`.

Production release assembly should use `require_release_ready = TRUE`. The writer then fails closed when any supplied comparison is failed or insufficient.

## CI policy

Every pull request may run a lightweight synthetic benchmark for early feedback. Scheduled and release workflows should run canonical benchmarks, retain historical observations, compare against an approved baseline, and publish the resulting artifacts in the scientific release bundle.

A performance regression should block a release only when the benchmark identity matches, the evidence meets the configured repetition requirement, and an approved performance budget is exceeded.

## First execution and review (2026-08-01)

`scripts/build_release_benchmark_archive.R` and `inst/scripts/scientific_review_packet.R` previously ran as two disconnected systems: CI was green on every PR, but never produced the `continuous_benchmarks.json` evidence the release-review packet actually looks for. That gap is fixed -- the script now also builds a real `PopgenVCFContinuousBenchmarkObservation` from an actual pipeline-realistic benchmark (PCA/IBS/diversity/FST on a deterministic synthetic dataset) and writes `continuous_benchmarks.{tsv,json,md}`. Verified against a manually dispatched real CI run (`release-benchmark-archive.yml`, run 30677987758): the artifact was produced correctly and `scientific_review_find_one()` finds and parses it.

**2026-08-01 -- Marc-Olivier Duceppe:** reviewed the manual checklist against that real run's evidence.

- **Approved** `benchmark_identity`: `benchmark_id`/`module`/`dataset_tier`/`threads`/`release`/`git_sha` are all well-formed and internally consistent with the derived observation key.
- **Approved** `repetition_count`: `repetitions=5`, exactly matching the budget's `minimum_repetitions=5` -- at the threshold, not with margin.
- **Approved as well-formed, not as comparable** `environment_comparability`: the one real fingerprint (GitHub-hosted Ubuntu/Azure runner, 4 cores, R 4.6.1, OpenBLAS) is complete, but nothing exists yet to compare it against.
- **Recorded as `insufficient-evidence`, not reviewed** `budget_checks` and `trend_interpretation`: this repository has **zero published GitHub Releases** (only a bare `v0.9.0-alpha1` git tag with no Release object). `release-benchmark-archive.yml`'s baseline-discovery step (`gh release download`) only ever looks at published Releases, never at prior workflow runs, so every run to date -- before and after this reconciliation -- has seen `comparisons: []`. This is not a defect in the reconciliation work; `compare_continuous_release_benchmark()` and the budget-check/trend-interpretation machinery have simply never been exercised against real data in this repository. Producing reviewable evidence for these two items requires this project's first actual published GitHub Release; that is an explicit, separate decision, not something implied by this review.
