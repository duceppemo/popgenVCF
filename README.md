# popgenVCF

> **Scientifically correct. Reproducible by design. Publication ready by default.**

**popgenVCF** is a modular R toolkit and command-line application for population-genomic analysis of diploid, biallelic SNP VCF files. It supports VCF-only quality control and sample-level analyses, analyses with population metadata, and spatial analyses when geographic coordinates are available.

> Development series: **0.10.0**. The public API is under release-conformance review; interfaces and output schemas may still evolve before 1.0.

## Repository status

Implementation contracts are complete through Phase **0.9.30.4**. Phase **0.9.30** archival readiness now includes DOI-ready but unpublished Zenodo metadata, a standalone source-package SPDX SBOM, OCI SBOM and provenance extraction, checksum-linked source-release provenance, complete release manifests, and archival verification instructions. Phase **0.9.31** is the active 0.10.0 release-candidate closure phase.

The software contracts are not the same as approved production evidence. The first checksum-verified real-data baseline, full external-tool concordance suite, historical release benchmark archive, real-data three-backend ancestry evidence, final release certificate, release tag, and published DOI still require execution, scientific review, approval, and publication through the dedicated validation and release workflows. See the [roadmap](docs/ROADMAP.md) for the authoritative distinction between completed infrastructure and remaining release evidence.

## Highlights

- Accepts `.vcf` and `.vcf.gz` input.
- Automatically sorts, BGZF-compresses, and Tabix-indexes input when required.
- Runs PCA, IBS/MDS, QC, filtering, and LD pruning directly from VCF sample IDs without metadata.
- Validates metadata sample IDs against the VCF before attaching annotations.
- Detects which analyses are possible from the available metadata columns.
- Performs exact audited SNPRelate QC and LD pruning.
- Provides diversity, FST, DAPC, population-structure, AMOVA, Mantel, and isolation-by-distance workflows when their requirements are met.
- Produces publication artifacts, validation records, and reproducible container images.
- Preserves canonical dataset checksums, scientific approval state, external-tool provenance, and release benchmark budgets as machine-readable evidence.
- Synchronizes the installed package citation, `CITATION.cff`, CodeMeta, FAIR software records, Zenodo deposition metadata, and reproducibility statement against one development-safe software identity.
- Produces checksum-linked source and OCI SBOM/provenance evidence without claiming an unpublished release date or DOI.

## Start here

The public guide sequence is:

1. [Run your first analysis](vignettes/getting-started.Rmd)
2. [Interpret population-genomic results](vignettes/interpreting-results.Rmd)
3. [Explore the maintained publication gallery](vignettes/publication-gallery.Rmd)
4. [Troubleshoot analyses and deployments](vignettes/troubleshooting.Rmd)
5. [Computational reproducibility](vignettes/reproducibility.Rmd)
6. [Containers and HPC deployment](vignettes/containers-and-hpc.Rmd)

The rendered versions are available from the [pkgdown site](https://duceppemo.github.io/popgenVCF/).

## Recommended installation: container image

For current development and evaluation:

```bash
export POPGENVCF_IMAGE="ghcr.io/duceppemo/popgenvcf:latest"
docker pull "$POPGENVCF_IMAGE"
```

For a reproducible release analysis, replace `latest` with the immutable digest recorded by the corresponding GitHub Release:

```bash
export POPGENVCF_IMAGE="ghcr.io/duceppemo/popgenvcf@sha256:<digest>"
docker pull "$POPGENVCF_IMAGE"
```

Generate a default configuration from the analysis directory:

```bash
docker run --rm \
  --user "$(id -u):$(id -g)" \
  -e HOME=/tmp \
  -v "$PWD:/data" \
  "$POPGENVCF_IMAGE" \
  --write-config /data/analysis.yml
```

Run the analysis:

```bash
docker run --rm \
  --user "$(id -u):$(id -g)" \
  -e HOME=/tmp \
  -v "$PWD:/data" \
  "$POPGENVCF_IMAGE" \
  --config /data/analysis.yml
```

Paths in `analysis.yml` must use container paths such as `/data/cohort.vcf.gz`, not host paths.

## VCF input

The input may be:

- an uncompressed `.vcf`;
- a BGZF-compressed `.vcf.gz` with `.tbi` or `.csi`;
- a BGZF-compressed `.vcf.gz` without an index;
- an ordinary gzip-compressed `.vcf.gz`;
- an unsorted VCF.

popgenVCF reuses a valid existing index. When a writable BGZF file lacks an index, it creates the `.tbi` beside the source. Otherwise it creates a coordinate-sorted BGZF copy and Tabix index in the analysis cache using `bcftools sort -Oz` and `bcftools index --tbi`.

```yaml
input:
  vcf: /data/cohort.vcf.gz
```

## Workflow modes

### VCF-only mode

A metadata file is optional:

```yaml
input:
  vcf: /data/cohort.vcf.gz
  metadata: null
```

The VCF sample names are the canonical sample identifiers. Without metadata, popgenVCF can perform sample and variant QC, filtering, LD pruning, PCA, IBS/MDS, and configured ancestry analyses. Population and spatial modules are skipped transparently because their annotations are unavailable.

### Sample metadata mode

A metadata file may add descriptive columns such as location, collection date, sex, or species. PCA and IBS/MDS do not require this file; metadata annotates their samples and figures.

### Population metadata mode

A complete `population` column enables population diversity, FST, DAPC, AMOVA, and population-level summaries.

### Spatial metadata mode

Valid `latitude` and `longitude` values enable Mantel tests, isolation by distance, geographic figures, and other spatial modules. Samples without coordinates are excluded only from the spatial calculation; the module must still meet its minimum complete-pair requirement.

## Metadata identity contract

When metadata is supplied, its `sample` column must match the VCF sample names **exactly**.

popgenVCF enforces all of the following before downstream analysis:

- every metadata sample ID exists in the VCF;
- every VCF sample ID occurs exactly once in metadata;
- duplicate metadata sample IDs are rejected;
- metadata rows are reordered to VCF sample order;
- matching is case-sensitive;
- internal whitespace in identifiers is not silently changed.

A mismatch is a fatal input error because silent dropping or reordering could attach population, location, or other metadata to the wrong individual.

### Metadata example

```text
sample	population	latitude	longitude	location
Sample01	Ontario	45.4215	-75.6972	Ottawa
Sample02	Ontario	45.4200	-75.6900	Ottawa
Sample03	Quebec	45.5019	-73.5674	Montreal
Sample04	Quebec	NA	NA	Montreal
```

Only `sample` is mandatory when metadata is supplied. Recognized optional fields include `population`, `latitude`, `longitude`, `location`, `collection_date`, `sex`, `species`, and `group`. Additional columns are retained rather than discarded.

## Capability and execution evidence

Every run writes machine-readable evidence before results should be interpreted:

- `analysis_capabilities.tsv` records available and skipped modules with reasons;
- `analysis_execution_plan.tsv` records the dependency-resolved plan;
- `analysis_execution_ledger.tsv` records success, failure, blocking, retries, and timeouts;
- `analysis_module_contracts.tsv` records registered module contracts;
- `analysis_artifacts.tsv` records produced artifacts when present;
- `analysis_validation.tsv` records module validation when present;
- `analysis_summary.tsv` provides a stable summary;
- `analysis_results.rds` stores the complete analysis object;
- `sessionInfo.txt` records the R environment.

A skipped or unavailable module is not a negative biological result. A blocked, failed, or timed-out module must not be interpreted.

## Example configuration

```yaml
input:
  vcf: /data/cohort.vcf.gz
  metadata: /data/metadata.tsv
  metadata_header: auto

output:
  directory: /data/results

compute:
  threads: 8
  seed: 42

qc:
  maf: 0.05
  max_sample_missing: 0.20

report:
  enabled: true
```

For VCF-only operation, remove `metadata` or set it to `null`.

## Local Conda/Mamba installation

```bash
git clone https://github.com/duceppemo/popgenVCF.git
cd popgenVCF
conda config --set channel_priority strict
mamba env create --file inst/conda/environment.yml
conda activate popgenvcf
Rscript inst/scripts/install-bioconductor.R
R CMD INSTALL .
bash inst/scripts/verify-environment.sh
```

## Scientific validation

Routine offline validation uses deterministic fixtures:

```r
core <- popgenVCF::run_scientific_validation(integration = TRUE, threads = 4)
structure <- popgenVCF::run_population_structure_validation(integration = TRUE)
stopifnot(core$passed, structure$passed)
```

Canonical real-data acquisition and external-tool execution are deliberately excluded from ordinary package checks. They run only in opt-in or scheduled full-validation workflows, with checksum verification and explicit approval before evidence can gate a release.

## Project documentation

- [Project charter](docs/PROJECT_CHARTER.md)
- [Architecture](docs/ARCHITECTURE.md)
- [Reproducibility statement](docs/reproducibility.md)
- [Release archival readiness](docs/developer/release-archival-readiness.md)
- [Citation metadata](CITATION.cff)
- [Zenodo deposition metadata](.zenodo.json)
- [CodeMeta software record](codemeta.json)
- [Container images](docs/user/container-images.md)
