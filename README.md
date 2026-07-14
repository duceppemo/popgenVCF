# popgenVCF

> **Scientifically correct. Reproducible by design. Publication ready by default.**

**popgenVCF** is a modular R toolkit and command-line application for population-genomic analysis of diploid, biallelic SNP VCF files. It supports VCF-only quality control, analyses with basic sample metadata, and spatial analyses when geographic coordinates are available.

> Development series: **0.9.0**. Interfaces and output schemas may still evolve before 1.0.

## Highlights

- Accepts `.vcf` and `.vcf.gz` input.
- Automatically sorts, BGZF-compresses, and Tabix-indexes input when required.
- Runs successfully without a metadata file.
- Detects which analyses are possible from the available metadata columns.
- Performs exact audited SNPRelate QC and LD pruning.
- Provides PCA, IBS/MDS, diversity, FST, DAPC, population-structure, AMOVA, Mantel, and isolation-by-distance workflows when their requirements are met.
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

The workflow performs sample QC, variant QC, allele-frequency and missingness summaries, heterozygosity, filtering audits, exact LD pruning, QC tables, and QC figures. Population and spatial modules are skipped, the reason is recorded in `analysis_capabilities.tsv`, and the workflow exits successfully.

### 2. Sample metadata mode

A metadata file containing sample IDs enables analyses that do not require predefined populations, such as PCA and IBS/MDS.

### 3. Population metadata mode

Adding a `population` column enables population diversity, FST, DAPC, AMOVA, and population-level summaries.

### 4. Spatial metadata mode

Adding valid `latitude` and `longitude` columns enables Mantel tests, isolation by distance, geographic figures, and other spatial modules.

## Metadata file format

Metadata should normally be a tab-delimited file with a header. Comma-delimited and whitespace-delimited input are also detected. Column names are normalized to lowercase snake case.

### Required columns

Only one column is mandatory when a metadata file is supplied:

| Column | Required | Type | Description |
|---|:---:|---|---|
| `sample` | Yes | text | Sample identifier matching a VCF sample exactly. Aliases such as `sample_id`, `id`, `individual`, and `individual_id` are accepted. |

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

This enables QC, PCA, IBS/MDS, and other sample-level analyses. Population-dependent modules are skipped.

### Population example without coordinates

```text
sample	population	location
Sample01	Ontario	Ottawa
Sample02	Ontario	Ottawa
Sample03	Quebec	Montreal
Sample04	Quebec	Montreal
```

This enables population modules, but spatial analyses are skipped because coordinates are unavailable.

### Full spatial example

```text
sample	population	latitude	longitude	location
Sample01	Ontario	45.4215	-75.6972	Ottawa
Sample02	Ontario	45.4200	-75.6900	Ottawa
Sample03	Quebec	45.5019	-73.5674	Montreal
Sample04	Quebec	45.5100	-73.5700	Montreal
```

Missing coordinates are allowed. Spatial modules use samples with complete latitude/longitude pairs and must still satisfy their module-specific minimum sample requirements.

## Analysis requirements

| Analysis | VCF only | `sample` | `population` | coordinates |
|---|:---:|:---:|:---:|:---:|
| Sample and variant QC | Yes |  |  |  |
| Filtering and LD pruning | Yes |  |  |  |
| PCA |  | Yes |  |  |
| IBS/MDS |  | Yes |  |  |
| ADMIXTURE / fastStructure / sNMF |  | Yes |  |  |
| Population diversity |  | Yes | Yes |  |
| FST |  | Yes | Yes |  |
| DAPC |  | Yes | Yes |  |
| AMOVA |  | Yes | Yes |  |
| Mantel / isolation by distance |  | Yes | Usually | Yes |
| Geographic plots and spatial modules |  | Yes | Optional | Yes |

The exact availability of external engines also depends on their configuration and input files.

## Capability reporting

Every run writes:

```text
analysis_capabilities.tsv
```

This table records each registered module, whether it was available, and why it was skipped. Examples include:

- `metadata not supplied; VCF-only QC workflow`;
- `population column unavailable`;
- `latitude/longitude unavailable`.

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
