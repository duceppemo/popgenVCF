# Release-state and public-API reconciliation

## Decision

`0.10.0` is the authoritative development release identity. The completed Phase 10 public-interface work justifies the 0.10 series; the 1.0 release remains gated by canonical real-data validation, complete publication adapters, benchmark evidence, metadata, documentation, and reproducible release artifacts.

## Sources of truth

- `DESCRIPTION` owns the package version.
- roxygen source owns generated `NAMESPACE` and `man/*.Rd` files.
- `NEWS.md` owns the release change log.
- `docs/ROADMAP.md` owns milestone status; `inst/doc/ROADMAP.md` is its packaged mirror.
- GitHub issues own actionable work and acceptance criteria.

Generated files must never be repaired independently of their source annotations.

## Reconciliation audit

Run from the repository root:

```bash
Rscript tools/reconcile-release-api.R
```

The audit writes deterministic evidence under `artifacts/release-reconciliation/`:

- `summary.tsv` — release identity and inventory counts;
- `exports.tsv` — every exported symbol and its documentation status;
- `s3-methods.tsv` — every registered S3 method;
- `version-signals.tsv` — release identity in all release-facing files;
- `findings.tsv` — blocking and advisory findings.

## Blocking findings

The audit fails closed when:

- an exported symbol lacks a matching Rd alias;
- `NAMESPACE` contains a duplicate export;
- a registered S3 method is not documented by either its method or generic;
- `DESCRIPTION`, `NEWS.md`, `README.md`, or either roadmap copy does not identify the authoritative development version.

Duplicate Rd aliases are reported as advisory because intentional shared documentation topics can legitimately expose the same alias. They must nevertheless be reviewed.

## Generated-file verification

Before merging release-facing API changes:

```r
roxygen2::roxygenise()
```

The resulting `NAMESPACE` and `man/` changes must be intentional and committed. CI is expected to reject unexplained generated-file drift through package tests and repository-diff checks.

## Release sequence

After this reconciliation milestone, the preferred sequence is:

1. canonical contract hardening;
2. IBS/MDS publication outputs;
3. licensed canonical real-data integration;
4. release evidence, metadata, benchmark publication, and final container/HPC documentation;
5. 0.10.0 release-readiness review.

The 1.0 release is not selected merely by feature count. It requires complete scientific validation and reproducible release evidence.
