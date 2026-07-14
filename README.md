# popgenVCF


> **popgenVCF is a comprehensive, reproducible, publication-quality population
> genomics toolkit for Variant Call Format datasets. It provides validated,
> modular analyses through one workflow that produces publication-ready tables,
> figures, and reports.**
**popgenVCF** is a modular R toolkit and command-line application for reproducible population-genomic analysis of diploid, biallelic SNP VCF files.

The project prioritizes statistical clarity, deterministic quality control, auditable outputs, stable interfaces, automated validation, and publication-ready reporting.

> Development release: **0.8.3**. The registry foundation is stable; each statistical module now has an enforceable validation contract. Independent numerical reference validation remains required before the 1.0 release.

## Foundation guarantees

- One canonical YAML configuration per analysis.
- A stable command-line interface with a deliberately small override surface.
- A serializable `PopgenVCFAnalysis` S3 state object.
- Exact, audited SNPRelate QC and LD-pruning semantics.
- Machine-readable tables as canonical outputs.
- Explicit separation of formal statistics from exploratory diagnostics.
- Unit tests, package checks, containers, workflow wrappers, and provenance records.

## Analysis registry

Statistical analyses are registered modules rather than hard-coded pipeline calls. Each module declares its prerequisites, enablement rule, runner, and description.

```r
registry <- popgenVCF::default_analysis_registry()
popgenVCF::list_analyses(registry)

analysis <- popgenVCF::run_pipeline(
  "analysis.yml",
  registry = registry,
  selected = c("pca", "fst", "dapc")
)
```

Dependencies are resolved automatically; selecting `dapc`, for example, also schedules its required diversity module. Custom registries can be constructed with `new_analysis_registry()` and `register_analysis()`.

## Installation

```r
install.packages(c("remotes", "BiocManager"))
BiocManager::install(c("SNPRelate", "gdsfmt"))
remotes::install_local("popgenVCF")
```

For development:

```bash
R CMD build popgenVCF
R CMD check --as-cran popgenVCF_0.8.3.tar.gz
R CMD INSTALL popgenVCF_0.8.3.tar.gz
```

## Reproducible Conda/Mamba environments

A single environment containing modern R/Bioconductor, fastStructure, and a
complete TeX distribution is not reliably solvable. These components have
independent release constraints, so popgenVCF uses a layered installation:

1. a **core environment** for R, CRAN packages, stable command-line tools, and
   report utilities;
2. Bioconductor packages installed inside that environment with
   `BiocManager`, which is the installation method recommended by
   Bioconductor;
3. an isolated optional environment for fastStructure;
4. system TeX or a future container image for complete PDF-report tooling.

This design is more reproducible than forcing incompatible packages into one
solver transaction.

### 1. Configure channels

Use strict priority and avoid mixing the Anaconda defaults channel with
conda-forge/Bioconda packages:

```bash
conda config --set channel_priority strict
```

The supplied YAML already declares:

```yaml
channels:
  - conda-forge
  - bioconda
  - nodefaults
```

### 2. Create the core environment

Mamba:

```bash
mamba env create \
  --file popgenVCF/inst/conda/environment.yml
conda activate popgenvcf
```

Micromamba:

```bash
micromamba create \
  --file popgenVCF/inst/conda/environment.yml
micromamba activate popgenvcf
```

Conda also works, but solves more slowly:

```bash
conda env create \
  --file popgenVCF/inst/conda/environment.yml
conda activate popgenvcf
```

The core environment contains R 4.5, CRAN dependencies, PLINK 1.9/2,
ADMIXTURE, bcftools, tabix/htslib, vcftools, Pandoc, qpdf, and HTML Tidy.
It intentionally excludes Bioconductor binary packages, fastStructure, and
TeX Live from the initial solve.

### 3. Install the matched Bioconductor release

With the core environment active:

```bash
Rscript popgenVCF/inst/scripts/install-bioconductor.R
```

This installs `gdsfmt`, `SNPRelate`, and `LEA` using `BiocManager`, matched to
the active R release. To omit the optional LEA/sNMF dependency:

```bash
POPGENVCF_INSTALL_LEA=false \
Rscript popgenVCF/inst/scripts/install-bioconductor.R
```

Validate the resulting Bioconductor installation with:

```bash
Rscript -e 'BiocManager::valid()'
```

### 4. Install popgenVCF

```bash
R CMD build popgenVCF
R CMD INSTALL popgenVCF_0.8.3.tar.gz
```

For an editable development installation:

```bash
Rscript -e 'remotes::install_local("popgenVCF", dependencies = FALSE, upgrade = "never")'
```

### 5. Verify the core environment

```bash
bash popgenVCF/inst/scripts/verify-environment.sh
```

### 6. Optional fastStructure environment

The current fastStructure package is intentionally isolated because its Python
and compiled-library constraints conflict with the modern R environment on
some platforms. Create its dedicated environment:

```bash
mamba env create \
  --file popgenVCF/inst/conda/faststructure-environment.yml
conda activate popgenvcf-faststructure
```

Then install the maintained Python-3 source port:

```bash
bash popgenVCF/inst/scripts/install-faststructure.sh
```

The installer prints the paths to `structure.py` and `chooseK.py`; record those
paths in the popgenVCF YAML configuration. fastStructure is optional: DAPC,
ADMIXTURE, and LEA/sNMF remain available without it.

### 7. PDF/LaTeX support

Pandoc, qpdf, and HTML Tidy are included in the core Conda environment. A
complete TeX distribution is deliberately not installed by Conda because
specialized packages such as `texlive-inconsolata` are not portable Conda
specifications and can destabilize the solve.

On Ubuntu/Debian, use the operating-system packages:

```bash
sudo apt update
sudo apt install \
  texlive-latex-base \
  texlive-latex-recommended \
  texlive-latex-extra \
  texlive-fonts-recommended \
  texlive-fonts-extra
```

Alternatively, install TeX Live or TinyTeX separately. The future Docker and
Apptainer images will include a known-working TeX stack.

### 8. Export resolved environments

After installation succeeds:

```bash
conda env export --no-builds > popgenvcf-core-resolved.yml
conda list --explicit > popgenvcf-core-linux-64.txt
```

For fastStructure, activate its environment and export it separately. Keeping
the lock files separate reflects the actual runtime boundaries and prevents
Python/R dependency conflicts.

### Why the all-in-one environment was removed

The previous environment combined R 4.5, Bioconda builds of SNPRelate/LEA,
fastStructure's older Python stack, and a nonexistent
`texlive-inconsolata` package. Depending on repodata state, this forced the
solver toward incompatible historical R, GSL, readline, PCRE2, and zlib builds.
The layered installation removes those artificial conflicts while retaining
all functionality.

### Future container image

The core YAML, Bioconductor installation script, fastStructure add-on, and
system TeX instructions now form the dependency specification for the future
Docker and Apptainer images. Containers will be the recommended route for a
single ready-to-run environment once published.

## Command-line use

Generate a configuration:

```bash
Rscript popgenVCF.R --write-config analysis.yml
```

Run the analysis:

```bash
Rscript popgenVCF.R --config analysis.yml
```

The installed launcher is also available at:

```r
system.file("scripts", "popgenVCF", package = "popgenVCF")
```

## R API

```r
analysis <- popgenVCF::run_pipeline("analysis.yml")
analysis
summary(analysis)

fst <- popgenVCF::get_analysis_result(analysis, "fst")
pca <- popgenVCF::get_analysis_result(analysis, "pca")
popgenVCF::validate_analysis(analysis)
```

Create an empty analysis state:

```r
state <- popgenVCF::new_popgen_vcf_analysis(popgenVCF::default_config())
```

## Fixed QC contract

The default LD-pruning contract is intentionally fixed:

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

The package independently audits MAF and missingness and verifies that every retained LD SNP belongs to the audited QC set.

## Repository layout

```text
popgenVCF/
├── R/                  Core package modules
├── inst/scripts/       Installed command-line launcher
├── inst/extdata/       Tiny reproducible example inputs
├── inst/doc/           Installed architecture documentation
├── tests/testthat/     Unit and contract tests
├── containers/         Docker and Apptainer definitions
├── nextflow/           Nextflow DSL2 wrapper
├── docs/               Governance and developer documentation
└── .github/workflows/  Automated checks and coverage
```

## Project policies

- [API stability policy](inst/doc/API_STABILITY.md)
- [Architecture](inst/doc/architecture.md)
- [Development roadmap](inst/doc/ROADMAP.md)
- [Contributing](CONTRIBUTING.md)
- [Code of conduct](CODE_OF_CONDUCT.md)

## Reports and system dependencies

Package build and check do not require Pandoc. Pandoc is required only when manuscript report rendering is enabled. PDF manuals require a complete LaTeX installation.

## Scientific validation

Run the bundled deterministic validation suite after installation:

```r
x <- popgenVCF::run_scientific_validation()
stopifnot(x$passed)
```

Use `integration = TRUE` to verify SNPRelate-backed QC and the exact designed LD-pruned marker set. External comparison runners live in `validation/reference`.

## Population-structure validation

Version 0.8.3 adds label-switching-aware comparisons and reproducibility checks for DAPC and ancestry-membership matrices:

```r
x <- popgenVCF::run_population_structure_validation(integration = TRUE)
print(x$checks)
stopifnot(x$passed)
```

Optional external engines are configured under `analyses.admixture`, `analyses.faststructure`, and `analyses.snmf`. Every external Q matrix must have an explicit sample-order source.
