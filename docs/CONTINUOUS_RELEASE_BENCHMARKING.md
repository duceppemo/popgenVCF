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
