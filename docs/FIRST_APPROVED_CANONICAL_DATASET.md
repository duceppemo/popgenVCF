# First approved canonical dataset

Phase 0.9.20 integrates the 1000 Genomes Project Phase 3 chromosome Y callset archived at Zenodo DOI `10.5281/zenodo.3359882`.

## Dataset

The selected files are:

- `ALL.chrY.phase3_integrated_v2a.20130502.genotypes.vcf.gz`
- `ALL.chrY.phase3_integrated_v2a.20130502.genotypes.vcf.gz.tbi`
- `integrated_call_male_samples_v3.20130502.ALL.panel`

The archive is not bundled in the R package. Routine CI remains offline.

## Two-stage integrity model

The source archive publishes MD5 digests. popgenVCF therefore applies two independent stages:

1. verify every staged source file against the immutable upstream filename and MD5 inventory;
2. compute SHA-256 locally and promote the verified files into the Phase 0.9.18 `PopgenVCFCanonicalDataset` contract.

No dataset can enter the approved registry unless every upstream digest passes and every derived SHA-256 digest is valid.

## Approval record

The catalogue records:

- stable dataset identifier and version;
- organism and genome assembly;
- DOI and scientific citation;
- archive rights statement;
- reviewer identity and ISO-8601 review date;
- explicit `full_validation_only` execution policy.

## Running full validation

Stage the three files in one directory, install popgenVCF, and run:

```bash
Rscript scripts/run-approved-canonical-validation.R \
  /path/to/1000g-phase3-chry \
  canonical-validation-evidence
```

The output includes source verification, the approved registry table, the promoted SHA-256 descriptor, canonical verification evidence, and methods text.

## CI policy

The dedicated workflow is manual only. It downloads and verifies the approved archive, runs the evidence writer, and uploads the evidence bundle. Pull-request and package-check workflows never download this dataset.
