# Scientific release benchmarking

The `Scientific release archive` workflow turns every eligible execution into a reproducible scientific QA artifact.

## Triggers

- Every pull request targeting `main` runs a non-publishing comparison.
- Tags matching `v*` build and attach release assets.
- `workflow_dispatch` supports manual certification and an optional release identifier.

## Baseline selection

The workflow queries the most recent GitHub Release and attempts to download `scientific-benchmark-archive.tar.gz`. If no prior archive exists, a new append-only archive is initialized and the report records `no-baseline`.

`latest_release_benchmark()` selects the highest semantic version when all record identifiers are version-like. Otherwise, it selects the newest UTC record timestamp. The current identifier can be excluded to prevent self-comparison.

## Generated assets

- `scientific-benchmark-archive.tar.gz`: complete checksummed archive, including canonical RDS records, per-release source tables, metadata, and manifests.
- `scientific-regression-report.tar.gz`: Quarto HTML when available, deterministic QMD source, release history, comparison tables, and summary.
- `scientific-release-SHA256SUMS.txt`: transport checksums for both compressed assets.

Workflow artifacts are retained for 90 days. Tagged runs also upload the three files to the matching GitHub Release using `gh release upload --clobber`.

## Local execution

Install the current package and run:

```bash
POPGENVCF_RELEASE_ID=v0.10.0-test \
GITHUB_SHA="$(git rev-parse HEAD)" \
Rscript scripts/build_release_benchmark_archive.R benchmark-release
```

To extend an existing archive:

```bash
Rscript scripts/build_release_benchmark_archive.R \
  benchmark-release previous/archive
```

The runner refuses to append a duplicate release identifier.

## Scientific policy

Numerical validation and population-structure validation are gating. The quick performance profile is recorded as informational because GitHub-hosted runners are noisy and machine fingerprints change. Stable self-hosted release runners can later supply gating performance baselines.

A changed component digest is reported as a change, not automatically as a scientific regression. Explicit comparison objects determine gating failures. This avoids claiming equivalence between distinct estimators or software families.

## Permissions

The workflow requires `contents: write` only so tagged runs can attach assets to an existing GitHub Release. Pull-request runs do not mutate releases or historical archives.
