# popgenVCF

> **Scientifically correct. Reproducible by design. Publication ready by default.**

**popgenVCF** is a modular R toolkit and command-line application for population-genomic analysis of diploid, biallelic SNP VCF files. It supports VCF-only quality control and sample-level analyses, analyses with population metadata, and spatial analyses when geographic coordinates are available.

> Development series: **0.9.0**. Interfaces and output schemas may still evolve before 1.0.

## Highlights

- Accepts `.vcf` and `.vcf.gz` input.
- Automatically sorts, BGZF-compresses, and Tabix-indexes input when required.
- Runs PCA, IBS/MDS, QC, filtering, and LD pruning directly from VCF sample IDs without metadata.
- Validates metadata sample IDs against the VCF before attaching any annotations.
- Detects which analyses are possible from the available metadata columns.
- Performs exact audited SNPRelate QC and LD pruning.
- Provides diversity, FST, DAPC, population-structure, AMOVA, Mantel, and isolation-by-distance workflows when their requirements are met.
- Produces publication artifacts, validation records, and reproducible container images.

## Recommended installation: Docker

```bash
docker pull ghcr.io/duceppemo/popgenvcf:latest
```

Generate a default configuration from your analysis directory:

```bash
docker run --rm \
  -v "$PWD:/data" \
  ghcr.io/duceppemo/popgenvcf:latest \
  --write-config /data/analysis.yml
```

Run the analysis:

```bash
docker run --rm \
  --user "$(id -u):$(id -g)" \
  -e HOME=/tmp \
  -v "$PWD:/data" \
  ghcr.io/duceppemo/popgenvcf:latest \
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

### 1. VCF-only mode

A metadata file is optional:

```yaml
input:
  vcf: /data/cohort.vcf.gz
  metadata: null
```

The sample names stored in the VCF are the canonical sample identifiers. Without metadata, popgenVCF performs:

- sample and variant QC;
- allele-frequency, MAF, missingness, and heterozygosity summaries;
- filtering audits and exact LD pruning;
- PCA;
- IBS/MDS and related sample-level ordination or distance outputs;
- external ancestry/structure analyses when their own required inputs are configured.

Population and spatial modules are skipped because the necessary annotations are unavailable. The reasons are recorded in `analysis_capabilities.tsv`, and the workflow exits successfully.

### 2. Sample metadata mode

A metadata file may add descriptive columns such as location, collection date, sex, or species. PCA and IBS/MDS do not require this file; metadata only annotates their samples and plots.

### 3. Population metadata mode

Adding a complete `population` column enables population diversity, FST, DAPC, AMOVA, and population-level summaries.

### 4. Spatial metadata mode

Adding valid `latitude` and `longitude` columns enables Mantel tests, isolation by distance, geographic figures, and other spatial modules. Samples without coordinates are excluded only from the spatial calculation; the module must still meet its minimum number of complete coordinate pairs.

## Metadata file format

Metadata should normally be a tab-delimited file with a header. Comma-delimited and whitespace-delimited input are also detected. Column names are normalized to lowercase snake case.

### Sample identity contract

When a metadata file is supplied, its `sample` column must match the VCF sample names **exactly**.

popgenVCF enforces all of the following before any downstream analysis:

- every metadata sample ID must exist in the VCF;
- every VCF sample ID must occur exactly once in the metadata;
- duplicate metadata sample IDs are rejected;
- metadata rows are reordered to the VCF sample order before annotations are attached;
- matching is case-sensitive and whitespace is not silently altered inside identifiers.

A mismatch is a fatal input error because silently dropping or misaligning samples could attach population, location, or other metadata to the wrong individual.

Every run with metadata writes:

```text
tables/02_sample_metadata_match.tsv
```

This records the VCF sample IDs, metadata presence, and post-QC retention state.

### Required columns

Only one column is mandatory when a metadata file is supplied:

| Column | Required | Type | Description |
|---|:---:|---|---|
| `sample` | Yes | text | Identifier matching a VCF sample exactly. Aliases such as `sample_id`, `id`, `individual`, and `individual_id` are accepted. |

### Recognized optional columns

| Column | Required | Type | Used by |
|---|:---:|---|---|
| `population` | No | text | Population diversity, FST, DAPC, AMOVA, population summaries, and plot colours. Alias `pop` is accepted. |
| `latitude` | No | numeric decimal degrees | Mantel, isolation by distance, maps, and spatial analyses. |
| `longitude` | No | numeric decimal degrees | Mantel, isolation by distance, maps, and spatial analyses. |
| `location` | No | text | Reports, labels, and descriptive summaries. |
| `collection_date` | No | text/date | Reports and future temporal analyses. |
| `sex` | No | text | Descriptive summaries and future stratified analyses. |
| `species` | No | text | Reports and multi-taxon metadata. |
| `group` | No | text | User-defined grouping retained for custom or future modules. |

Additional columns are accepted and carried through the analysis object and outputs. Unknown columns are not discarded.

### Sample-only example

```text
sample	location	collection_date
Sample01	Ottawa	2025-06-01
Sample02	Ottawa	2025-06-03
Sample03	Montreal	2025-06-10
```

This adds labels and descriptive annotations to QC, PCA, IBS/MDS, and other sample-level analyses. Population-dependent modules are skipped.

### Population example without coordinates

```text
sample	population	location
Sample01	Ontario	Ottawa
Sample02	Ontario	Ottawa
Sample03	Quebec	Montreal
Sample04	Quebec	Montreal
```

This enables population modules, but spatial analyses are skipped because no complete coordinate pairs are available.

### Full or partial spatial example

```text
sample	population	latitude	longitude	location
Sample01	Ontario	45.4215	-75.6972	Ottawa
Sample02	Ontario	45.4200	-75.6900	Ottawa
Sample03	Quebec	45.5019	-73.5674	Montreal
Sample04	Quebec	NA	NA	Montreal
```

Missing coordinates are allowed. Spatial modules use samples with complete latitude/longitude pairs and must still satisfy their module-specific minimum sample requirements. The other analyses continue to use all retained samples.

## Analysis requirements

| Analysis | VCF only | `sample` metadata | complete `population` | usable coordinates |
|---|:---:|:---:|:---:|:---:|
| Sample and variant QC | Yes | Optional |  |  |
| Filtering and LD pruning | Yes | Optional |  |  |
| PCA | Yes | Optional |  |  |
| IBS/MDS | Yes | Optional |  |  |
| ADMIXTURE / fastStructure / sNMF | Yes* | Optional |  |  |
| Population diversity |  | Yes | Yes |  |
| FST |  | Yes | Yes |  |
| DAPC |  | Yes | Yes |  |
| AMOVA |  | Yes | Yes |  |
| Mantel / isolation by distance |  | Yes | Usually | Yes |
| Geographic plots and spatial modules |  | Yes | Optional | Yes |

`*` External engines also require their configured executable and method-specific input files.

## Capability reporting

Every run writes:

```text
analysis_capabilities.tsv
```

This table records each registered module, whether it was available, and why it was skipped. Examples include:

- `available from VCF sample IDs`;
- `metadata not supplied; population annotations unavailable`;
- `complete population annotations unavailable`;
- `no complete latitude/longitude pairs available`.

Explicitly requested unavailable modules are skipped with a warning rather than causing unrelated analyses to fail.

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

## Conda/Mamba installation

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

```r
core <- popgenVCF::run_scientific_validation(integration = TRUE, threads = 4)
structure <- popgenVCF::run_population_structure_validation(integration = TRUE)
stopifnot(core$passed, structure$passed)
```

## Project documentation

- [Project charter](docs/PROJECT_CHARTER.md)
- [Architecture](docs/ARCHITECTURE.md)
- [Scientific validation policy](docs/SCIENTIFIC_VALIDATION.md)
- [Development guide](docs/DEVELOPMENT_GUIDE.md)
- [Style guide](docs/STYLE_GUIDE.md)
- [Roadmap](docs/ROADMAP.md)
- [Contributing](CONTRIBUTING.md)

## License

popgenVCF is released under the MIT License. External programs retain their own licenses.
