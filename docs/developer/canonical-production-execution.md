# Canonical production execution

The **Canonical real-data production validation** workflow is the first production-evidence execution path for issue #22. It acquires an approved 1000 Genomes Phase 3 dataset from its checksum-pinned Zenodo record, validates the source inventory from a clean external data directory, and emits evidence for the `canonical_validation` release-candidate gate.

Phase 0.9.32 adds chromosome 22 as the bounded autosomal production input. It is the smallest archived autosome (about 215 MB compressed), contains all 2,504 Phase 3 samples, and supports mixed-sex diploid analyses that chromosome Y cannot validate. The manual workflow defaults to `chr22`; `chrY` remains selectable for the existing haploid structural-validation path.

A successful chromosome 22 run now produces a bounded quantitative baseline proposal after structural validation. It does **not** approve that proposal, external-tool concordance, ancestry evidence, or the 0.10.0 release. The proposal record keeps the production-baseline gate explicitly `not_passed` until retained evidence receives named scientific approval.

## Safety boundary

Ordinary pull requests and pushes run only the synthetic execution-contract tests. They never download canonical data.

The real-data job runs only through `workflow_dispatch` when all of the following are supplied:

- an exact candidate ref or full commit SHA;
- a stable production-run identifier;
- `confirm_production: true`.

The workflow checks out the requested revision, records the resulting full commit SHA, installs the package and `bcftools` in a clean GitHub-hosted environment, and executes the approved source specification selected for chromosome 22 or chromosome Y.

## Production checks

The execution fails closed unless it establishes all of the following:

1. every approved source file is acquired explicitly from the declared URL or a supplied local mirror;
2. every file matches the approved upstream MD5 inventory;
3. every verified source file is promoted to a retained SHA-256 identity;
4. the VCF is readable by `bcftools` and its tabix index reports a non-zero variant count;
5. the VCF sample inventory is non-empty and unique;
6. the panel contains complete sample, population, superpopulation, and sex metadata;
7. the VCF and panel sample sets match exactly;
8. the source-specific sample-sex policy passes: male-only for chromosome Y, or complete mixed male/female assignments for chromosome 22;
9. every retained evidence artifact is bound by byte size and SHA-256;
10. the terminal checksum inventory verifies after evidence construction.

A checksum mismatch, incomplete panel, missing executable, malformed source, stale output directory, unsafe path, or attempt to place raw canonical data inside the evidence directory aborts the run before a passing gate record can be finalized. Approved remote acquisitions use a bounded minimum 10-minute R download timeout and restore the caller's prior option afterward; incomplete transfers still fail closed before installation.

## Manual workflow execution

From the GitHub Actions page, select **Canonical real-data production validation**, choose **Run workflow**, and provide:

```text
candidate_ref: <full candidate commit SHA>
candidate_id: 0.10.0-production-1
confirm_production: true
```

A full commit SHA is preferred over a moving branch name for retained production evidence.

The equivalent repository command is:

```bash
Rscript scripts/run-approved-canonical-validation.R \
  canonical-production-evidence \
  /path/outside/the/repository/canonical-data \
  0.10.0-production-1 \
  "$(git rev-parse HEAD)" \
  "$(date -u +'%Y-%m-%dT%H:%M:%SZ')" \
  --dataset=chr22 \
  --allow-download \
  --verbose
```

To use an independently maintained local mirror instead of network acquisition, replace `--allow-download` with:

```text
--source-dir=/path/to/approved-source-mirror
```

The source mirror must contain the exact basenames declared by the approved source specification.

## Evidence artifact

The workflow uploads evidence only. The raw VCF, tabix index, and panel are kept in the runner temporary directory and are not included in the uploaded artifact.

```text
canonical-production-evidence/
|-- canonical-production-execution.json
|-- canonical-production-environment.tsv
|-- canonical-production-artifacts.tsv
|-- canonical-production-SHA256SUMS.txt
|-- canonical-validation-gate-record.json
|-- canonical_dataset_structure.tsv
|-- canonical_sample_metadata.tsv
`-- source/
    |-- canonical_source_acquisition.tsv
    |-- canonical_source_verification.tsv
    |-- canonical_dataset_registry.tsv
    `-- dataset/
        |-- canonical_dataset.tsv
        |-- canonical_dataset_verification.tsv
        `-- canonical_validation_methods.md
```

`canonical-validation-gate-record.json` is shaped as the `canonical_validation` record consumed by the Phase 0.9.31 evidence index. Its status can be `passed` only after the complete acquisition, identity, structure, sample, and checksum sequence succeeds.

`canonical-production-execution.json` explicitly records:

- candidate identifier, package version, exact commit, and execution timestamp;
- approved dataset identity, DOI, licence, source URLs, sizes, and SHA-256 values;
- exact `bcftools` inspection commands;
- sample and variant counts;
- the passed `canonical_validation` state;
- `not_run` states for `production_baseline` and `external_concordance`;
- confirmation that raw source data were excluded from the evidence artifact.

Verify an extracted artifact with:

```bash
cd canonical-production-evidence
sha256sum --check canonical-production-SHA256SUMS.txt
```

For chromosome 22, the workflow also uploads the checksum-independent sibling `autosomal-baseline-proposal/` bundle documented in [Autosomal quantitative baseline proposal](canonical-autosomal-baseline-proposal.md). Raw source, derived VCF, tabix, and GDS files are excluded from both evidence directories.

## Scientific review sequence

After a successful run:

1. verify the exact commit and package version in `canonical-production-execution.json`;
2. recompute the terminal checksum inventory;
3. inspect the acquisition, upstream MD5, promoted SHA-256, VCF structure, and sample metadata tables;
4. retain the successful workflow run and artifact as candidate-specific evidence;
5. incorporate the gate record into the production evidence index only for the same exact commit;
6. review the chromosome 22 quantitative proposal without treating it as an approved baseline;
7. execute the required external-tool comparisons separately.

The production baseline and concordance gates require named approval and cannot be inferred from this workflow's success. Baseline promotion is a separate reviewed change.
