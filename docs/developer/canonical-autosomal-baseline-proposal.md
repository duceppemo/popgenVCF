# Autosomal quantitative baseline proposal

Phase 0.9.32 adds a bounded quantitative execution after successful structural validation of the approved 1000 Genomes Phase 3 chromosome 22 source. The execution creates a scientific-review proposal; it does not approve a production baseline or authorize a release.

## Fixed analysis contract

The proposal runner derives biallelic SNPs from `22:20000000-21000000` into a temporary work directory. It then runs the package pipeline with the approved panel, sample and variant missingness thresholds of 0.20, MAF 0.05, LD pruning at r2 0.20, seed 42, ten requested principal components, and four compute threads. Only PCA is selected for this bounded execution.

Six observations are retained:

- derived interval variant count;
- retained sample count;
- QC-passing variant count;
- LD-pruned variant count;
- PC1 variance proportion;
- PC2 variance proportion.

The four counts use exact proposal comparators. The variance proportions use a relative tolerance of `1e-6` to allow supported numerical-library variation. These settings and values remain proposals until scientific review explicitly accepts or revises them.

## Execution and evidence

The manual **Canonical real-data production validation** workflow runs this step only when `dataset: chr22` and `confirm_production: true`. Structural validation must pass first. The source VCF, derived VCF, tabix indexes, and transient GDS remain in runner temporary storage.

The uploaded sibling directory `autosomal-baseline-proposal/` contains:

```text
autosomal-baseline-proposal/
|-- autosomal-baseline-proposal.json
|-- autosomal-baseline-proposal-record.json
|-- autosomal-baseline-observations.tsv
|-- autosomal-baseline-derived-input.json
|-- autosomal-baseline-environment.tsv
|-- autosomal-baseline-artifacts.tsv
|-- autosomal-baseline-SHA256SUMS.txt
`-- analysis/
    |-- analysis_execution_ledger.tsv
    |-- analysis_execution_plan.tsv
    |-- analysis_summary.tsv
    |-- analysis_validation.tsv
    |-- pipeline.log
    `-- tables/
```

Every retained file is covered by the terminal SHA-256 inventory. Verification rejects missing, injected, altered, symlinked, or raw-genotype files.

## Review boundary

The snapshot and proposal record both state `approval: proposed`; the record separately states `production_baseline_gate: not_passed`. Code paths that require an approved real-data snapshot continue to fail closed.

Scientific review must independently verify the exact candidate commit, source and derived-input checksums, sample inventory, QC and LD-pruning evidence, PCA output, metric definitions, expected values, and tolerances. Approval requires named reviewer metadata and a separate reviewed change. A successful workflow run alone cannot promote this proposal.
