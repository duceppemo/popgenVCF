# popgenVCF

> **Scientifically correct. Reproducible by design. Publication ready by default.**

**popgenVCF** is a modular R toolkit and command-line application for population-genomic analysis of diploid, biallelic SNP VCF files. It combines deterministic QC, validated statistical modules, external population-structure engines, publication-quality outputs, and reproducible execution through Conda or containers.

> Development series: **0.9.0**. Interfaces and output schemas may still evolve before 1.0.

## Highlights

- Canonical YAML configuration and command-line interface.
- Exact, audited SNPRelate QC and LD-pruning semantics.
- PCA, IBS/MDS, diversity, FST, DAPC, ADMIXTURE, fastStructure, and LEA/sNMF integration.
- Label-switching-aware ancestry-matrix comparison and replicate consensus.
- Scientific validation against hand calculations and independent implementations.
- Publication artifact contracts for tables, figures, methods, captions, validation, and provenance.
- Reproducible Conda environment and validated GHCR container.
- GitHub Actions package checks, numerical validation, coverage, container smoke tests, SBOM, and provenance.

## Recommended installation: Docker

The published image contains popgenVCF, R, matched Bioconductor packages, PLINK 1.9/2, ADMIXTURE, bcftools, tabix, vcftools, Pandoc, and the other dependencies used by the validated workflow.

### 1. Install Docker

Install Docker Engine or Docker Desktop and verify it is available:

```bash
docker --version
```

### 2. Pull the image

The development image built from `main` is:

```bash
docker pull ghcr.io/duceppemo/popgenvcf:latest
```

`latest` and `main` track the current validated development branch. For a publication or archived analysis, prefer a version tag such as `v0.9.0` when available:

```bash
docker pull ghcr.io/duceppemo/popgenvcf:v0.9.0
```

A commit-specific `sha-...` image is the most precise choice when reproducing an exact development snapshot.

If GHCR requires authentication, create a GitHub personal access token with `read:packages`, then run:

```bash
echo "$GITHUB_TOKEN" | docker login ghcr.io -u duceppemo --password-stdin
```

### 3. Prepare a working directory

Keep the configuration, VCF, metadata, and outputs under one host directory so they can all be mounted into the container:

```text
analysis/
├── analysis.yml
├── cohort.vcf.gz
├── cohort.vcf.gz.tbi
├── metadata.tsv
└── results/
```

Paths written in `analysis.yml` must use the **container path**, not the host path. When the host directory is mounted at `/data`, use entries such as:

```yaml
input:
  vcf: /data/cohort.vcf.gz
  metadata: /data/metadata.tsv

output:
  directory: /data/results
```

### 4. Generate a default configuration

From the host analysis directory:

```bash
docker run --rm \
  -v "$PWD:/data" \
  ghcr.io/duceppemo/popgenvcf:latest \
  --write-config /data/analysis.yml
```

Edit `analysis.yml` on the host before running the analysis.

### 5. Run popgenVCF

```bash
docker run --rm \
  -v "$PWD:/data" \
  ghcr.io/duceppemo/popgenvcf:latest \
  --config /data/analysis.yml
```

The image entrypoint already invokes popgenVCF, so do not add `Rscript` or the script name.

For a machine with a fixed CPU allocation, limit Docker and set the same thread count in the YAML:

```bash
docker run --rm \
  --cpus 8 \
  -v "$PWD:/data" \
  ghcr.io/duceppemo/popgenvcf:latest \
  --config /data/analysis.yml
```

### 6. Avoid root-owned output files on Linux

Docker normally runs as root inside the container. To create results owned by the current host user:

```bash
docker run --rm \
  --user "$(id -u):$(id -g)" \
  -e HOME=/tmp \
  -v "$PWD:/data" \
  ghcr.io/duceppemo/popgenvcf:latest \
  --config /data/analysis.yml
```

The mounted directory must be writable by that user.

### 7. Run the scientific validation suites

The image is validated while it is built, but the tests can also be rerun locally:

```bash
docker run --rm \
  ghcr.io/duceppemo/popgenvcf:latest \
  Rscript -e 'x <- popgenVCF::run_scientific_validation(integration = TRUE, threads = 4); print(x$checks); stopifnot(x$passed)'
```

```bash
docker run --rm \
  ghcr.io/duceppemo/popgenvcf:latest \
  Rscript -e 'x <- popgenVCF::run_population_structure_validation(integration = TRUE); print(x$checks); stopifnot(x$passed)'
```

### 8. Open a shell or R session in the image

The entrypoint permits direct passthrough to `bash`, `sh`, `R`, and `Rscript`:

```bash
docker run --rm -it \
  -v "$PWD:/data" \
  ghcr.io/duceppemo/popgenvcf:latest \
  bash
```

```bash
docker run --rm -it \
  ghcr.io/duceppemo/popgenvcf:latest \
  R
```

### 9. Record the exact image used

For reproducible publication records, save the immutable image digest:

```bash
docker image inspect \
  ghcr.io/duceppemo/popgenvcf:latest \
  --format '{{index .RepoDigests 0}}'
```

Record this digest with the analysis configuration and popgenVCF result manifest.

### Docker troubleshooting

**Permission denied writing results:** use `--user "$(id -u):$(id -g)" -e HOME=/tmp`, or correct host-directory permissions.

**Input file not found:** confirm the file is inside the mounted host directory and that the YAML uses `/data/...` paths.

**Image pull denied:** authenticate to GHCR or confirm the package visibility is public.

**Out of memory:** increase Docker Desktop memory or reduce configured threads and concurrent external jobs.

**SELinux systems:** add `:Z` to the mount, for example `-v "$PWD:/data:Z"`.

## Conda/Mamba installation

Docker is the easiest reproducible route. For development, HPC modules, or direct R access, create the layered Conda environment from the repository.

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

The core environment intentionally excludes fastStructure because its Python dependency stack conflicts with modern R/Bioconductor builds on some platforms. Install it separately when needed:

```bash
mamba env create --file inst/conda/faststructure-environment.yml
conda activate popgenvcf-faststructure
bash inst/scripts/install-faststructure.sh
```

## Command-line use without Docker

Generate a configuration:

```bash
Rscript popgenVCF.R --write-config analysis.yml
```

Run an analysis:

```bash
Rscript popgenVCF.R --config analysis.yml
```

## R API

```r
analysis <- popgenVCF::run_pipeline("analysis.yml")
summary(analysis)

pca <- popgenVCF::get_analysis_result(analysis, "pca")
fst <- popgenVCF::get_analysis_result(analysis, "fst")
popgenVCF::validate_analysis(analysis)
```

## Analysis registry and artifact contracts

Analyses are registered modules with declared dependencies, results, validation, resource class, references, and optional publication artifacts.

```r
registry <- popgenVCF::default_analysis_registry()
popgenVCF::list_analyses(registry)
```

Publication-enabled modules return a `PopgenVCFArtifactManifest` containing stable identifiers for machine-readable tables, figures, methods, captions, source data, supplementary outputs, validation records, and provenance.

## Fixed QC contract

The default LD-pruning behavior is intentionally fixed and independently audited:

```r
SNPRelate::snpgdsLDpruning(
  gds,
  sample.id = sample_ids,
  maf = maf_threshold,
  missing.rate = 0.2,
  method = "corr",
  ld.threshold = sqrt(0.2),
  slide.max.bp = Inf,
  slide.max.n = 50L,
  start.pos = "first",
  autosome.only = FALSE,
  num.thread = min(requested_threads, 4L),
  verbose = FALSE
)
```

The package separately audits allele frequency, MAF, sample and variant missingness, and the final retained LD marker set.

## Scientific validation

```r
core <- popgenVCF::run_scientific_validation(
  integration = TRUE,
  threads = 4
)

structure <- popgenVCF::run_population_structure_validation(
  integration = TRUE
)

stopifnot(core$passed, structure$passed)
```

## Repository layout

```text
popgenVCF/
├── R/                  Package and analysis modules
├── inst/scripts/       Installed launchers and environment checks
├── inst/conda/         Core and optional Conda specifications
├── inst/extdata/       Tiny deterministic fixtures
├── tests/testthat/     Unit, contract, and numerical tests
├── validation/         Independent reference validation
├── docker/             Container entrypoint
├── Dockerfile          Production image definition
├── nextflow/           Nextflow DSL2 wrapper
├── docs/               Charter, architecture, roadmap, and guides
└── .github/workflows/  Package, validation, coverage, and GHCR CI
```

## Project documentation

- [Project charter](docs/PROJECT_CHARTER.md)
- [Architecture](docs/ARCHITECTURE.md)
- [Scientific validation policy](docs/SCIENTIFIC_VALIDATION.md)
- [Development guide](docs/DEVELOPMENT_GUIDE.md)
- [Style guide](docs/STYLE_GUIDE.md)
- [Roadmap](docs/ROADMAP.md)
- [Contributing](CONTRIBUTING.md)
- [Code of conduct](CODE_OF_CONDUCT.md)

## License

popgenVCF is released under the MIT License. External programs retain their own licenses; in particular, review ADMIXTURE licensing terms before redistribution or commercial use.
