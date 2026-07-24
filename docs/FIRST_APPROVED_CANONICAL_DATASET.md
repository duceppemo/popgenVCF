# First approved canonical dataset

The approved public production-validation source is the 1000 Genomes Project Phase 3 chromosome Y callset archived at Zenodo DOI `10.5281/zenodo.3359882`.

## Approved bounded autosomal source

Phase 0.9.32 also approves the chromosome 22 Phase 3 callset from the same Zenodo record as the first autosomal production-validation input:

- `ALL.chr22.phase3_shapeit2_mvncall_integrated_v5a.20130502.genotypes.vcf.gz` (MD5 `ad7d6e0c05edafd7faed7601f7f3eaba`);
- its Tabix index (MD5 `4202e9a481aa8103b471531a96665047`);
- `integrated_call_samples_v3.20130502.ALL.panel` (MD5 `7ee5675553088230530a7fe88c22f201`).

Chromosome 22 is intentionally bounded: it is the smallest archived autosome while retaining all 2,504 samples. It enables diploid PCA, IBS/MDS, DAPC, FST, diversity, AMOVA, and tree execution without downloading the 19.4 GB whole archive. Its panel must contain complete recognized male and female assignments; it is not subject to the chromosome-Y male-only rule.

## Dataset

The selected files are:

- `ALL.chrY.phase3_integrated_v2a.20130502.genotypes.vcf.gz`
- `ALL.chrY.phase3_integrated_v2a.20130502.genotypes.vcf.gz.tbi`
- `integrated_call_male_samples_v3.20130502.ALL.panel`

The archive is not bundled in the R package. Ordinary package checks and pull-request validation remain offline and synthetic.

## Two-stage integrity model

The source archive publishes MD5 digests. popgenVCF therefore applies two independent stages:

1. verify every staged source file against the immutable upstream filename and MD5 inventory;
2. compute SHA-256 locally and promote the verified files into the `PopgenVCFCanonicalDataset` contract.

No dataset can enter the approved registry unless every upstream digest passes and every derived SHA-256 digest is valid.

## Approval record

The catalogue records:

- stable dataset identifier and version;
- organism and genome assembly;
- DOI and scientific citation;
- archive rights statement;
- reviewer identity and ISO-8601 review date;
- explicit `full_validation_only` execution policy.

This approval authorizes the dataset source and registry entry. It does not approve a generated quantitative baseline, external-tool comparison, ancestry result, or release decision.

## Candidate-bound production execution

The manual **Canonical real-data production validation** workflow is the first execution path under issue #22. It checks out an explicitly selected candidate revision, acquires the three approved files into runner-temporary storage, and records the resulting full commit SHA before evidence is finalized.

The workflow then:

1. verifies the approved upstream MD5 inventory;
2. promotes each verified file to SHA-256 identity;
3. uses `bcftools` to verify that the indexed VCF is readable and contains variants;
4. verifies unique VCF sample identifiers;
5. verifies complete sample, population, superpopulation, and sex metadata;
6. requires the VCF and panel sample inventories to match exactly;
7. requires the chromosome Y panel to contain male samples only;
8. writes candidate, package, environment, command, dataset, structure, and sample evidence;
9. creates the `canonical_validation` release-candidate gate record;
10. binds every retained evidence file with a terminal SHA-256 inventory.

The raw VCF, tabix index, and panel remain outside the uploaded evidence directory.

## Running from GitHub Actions

From the Actions page, select **Canonical real-data production validation** and provide:

```text
candidate_ref: <full candidate commit SHA>
candidate_id: 0.10.0-production-1
confirm_production: true
```

A full commit SHA should be used for retained production evidence. Pull requests exercise only the synthetic execution-contract tests and never download the canonical dataset.

## Running from a clean local environment

Install popgenVCF and `bcftools`, then run against the approved remote source:

```bash
Rscript scripts/run-approved-canonical-validation.R \
  canonical-production-evidence \
  /path/outside/the/repository/canonical-data \
  0.10.0-production-1 \
  "$(git rev-parse HEAD)" \
  "$(date -u +'%Y-%m-%dT%H:%M:%SZ')" \
  --allow-download \
  --verbose
```

An independently maintained local mirror may be supplied with:

```text
--source-dir=/path/to/1000g-phase3-chry
```

The output is a checksum-linked evidence bundle containing source acquisition and verification records, the approved registry entry, promoted SHA-256 descriptor, structural and sample inventories, exact inspection commands, environment identity, execution record, artifact manifest, and `canonical_validation` gate record.

See [Canonical production execution](developer/canonical-production-execution.md) for the evidence schema, safety boundary, verification procedure, and scientific review sequence.

## Remaining scientific boundary

A successful execution can satisfy only the non-approval `canonical_validation` gate for the exact recorded candidate commit. The production baseline and external concordance states remain `not_run` until their respective measurements are generated, retained, reviewed, and explicitly approved. The 0.10.0 production dossier therefore remains blocked after this workflow succeeds.
