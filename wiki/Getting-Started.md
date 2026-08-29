# Getting started

This page takes you from installation to a first analysis. Begin with a small
VCF and a new output directory. Do not start by enabling every optional module.

Already have popgenVCF installed in R? A real, bundled example dataset (160
real 1000 Genomes chromosome 22 samples) lets you see complete output in a
few minutes with no download and no Docker: see the
[quickstart vignette](https://duceppemo.github.io/popgenVCF/articles/quickstart.html)
(`vignette("quickstart", package = "popgenVCF")`). See
[Results and Interpretation](Results-and-Interpretation) for real figures
from that same run, and
[the example PDF report](https://github.com/duceppemo/popgenVCF/blob/main/docs/examples/chr22-quickstart-report.pdf)
for a complete preview.

## 1. Choose an installation path

### Docker

Docker is the simplest evaluation path because R, popgenVCF, BCFtools, and the
required system libraries are packaged together.

```bash
docker pull ghcr.io/duceppemo/popgenvcf:latest
```

For production, use the immutable digest recorded by the corresponding GitHub
Release:

```bash
docker pull ghcr.io/duceppemo/popgenvcf@sha256:<digest>
```

### Local R installation

Install R 4.3 or newer, BCFtools, and HTSlib first:

```bash
git clone https://github.com/duceppemo/popgenVCF.git
cd popgenVCF
Rscript install_popgenVCF.R
```

For Conda/Mamba or HPC, use [Deployment and Troubleshooting](Deployment-and-Troubleshooting).

## 2. Prepare input

The minimum input is a diploid VCF:

- `.vcf`;
- BGZF `.vcf.gz` with `.tbi` or `.csi`;
- BGZF `.vcf.gz` without an index;
- ordinary gzip `.vcf.gz`;
- sorted or unsorted input.

popgenVCF reuses a valid index. Otherwise it uses BCFtools to create a sorted,
BGZF-compressed and indexed working copy. Preserve the original input and its
checksum.

A raw VCF straight off a variant caller -- indels, multiallelic sites,
structural variants, and monomorphic records mixed in with genuine biallelic
SNPs -- does not need to be pre-filtered. VCF-to-GDS conversion
(`SNPRelate::snpgdsVCF2GDS(..., method = "biallelic.only")`) automatically
retains only biallelic, polymorphic SNPs and silently drops everything else
before any of this package's own analysis code runs; every downstream
table/figure only ever describes that retained biallelic-SNP set.

Metadata are optional. When supplied, the required `sample` column must match
every VCF sample identifier exactly and uniquely. An optional `alias` provides
a public label without changing the immutable VCF/GDS identity:

```text
sample	alias	population	latitude	longitude	location
Sample01	Ottawa_01	Ontario	45.4215	-75.6972	Ottawa
Sample02	NA	Ontario	45.4200	-75.6900	Ottawa
Sample03	Montreal_01	Quebec	45.5019	-73.5674	Montreal
Sample04	Montreal_02	Quebec	NA	NA	Montreal
```

Matching is case-sensitive. A mismatch is fatal because silent reordering could
attach population or location information to the wrong individual.

Coordinates must be signed decimal degrees in latitude-longitude order: north
and east are positive, south and west are negative. Do not use DMS, compass
suffixes, UTM, or projected coordinates.

Use `NA` for unavailable optional values and keep every sample row. Missing
aliases fall back to `sample`; incomplete coordinate pairs exclude only that
sample spatially, while any missing `population` disables population modules.

See the [User Guide metadata contract](User-Guide#metadata-file-contract) for
all supported columns, identity rules, and missing-data behavior.

## 3. Create a configuration

With Docker, run from the directory containing the VCF:

```bash
docker run --rm --user "$(id -u):$(id -g)" \
  -e HOME=/tmp -v "$PWD:/data" \
  ghcr.io/duceppemo/popgenvcf:latest \
  --write-config /data/analysis.yml
```

For a local installation:

```bash
Rscript -e 'popgenVCF::cli_main(c("--write-config", "analysis.yml"))'
```

`-v "$PWD:/data"` is a Docker bind mount: it maps whatever directory you ran `docker
run` from onto `/data` *inside* the container. The container cannot see any other part
of your filesystem, so every path in `analysis.yml` must start with `/data`, not your
real host path:

```
my_project/                         (this is $PWD -- where you ran `docker run`)
├── cohort.vcf.gz  <-->  /data/cohort.vcf.gz
├── metadata.tsv   <-->  /data/metadata.tsv
├── analysis.yml   <-->  /data/analysis.yml
└── results/       <-->  /data/results/         (created here by the pipeline)
```

Use container paths in a Docker configuration:

```yaml
input:
  vcf: /data/cohort.vcf.gz
  metadata: /data/metadata.tsv

output:
  directory: /data/results

compute:
  threads: 4
  seed: 42

qc:
  maf: 0.05
  max_sample_missing: 0.20

report:
  enabled: true
```

For VCF-only operation, omit `metadata` or set it to `null`.

## 4. Run

Docker:

```bash
docker run --rm --user "$(id -u):$(id -g)" \
  -e HOME=/tmp -v "$PWD:/data" \
  ghcr.io/duceppemo/popgenvcf:latest \
  --config /data/analysis.yml
```

Local R:

```bash
Rscript -e 'popgenVCF::cli_main(c("--config", "analysis.yml"))'
```

## 5. Check execution before interpreting biology

Open these files first:

- `analysis_capabilities.tsv` — modules available from the supplied input;
- `analysis_execution_plan.tsv` — dependency-resolved plan;
- `analysis_execution_ledger.tsv` — success, failure, blocking, retries, and
  timeouts;
- `analysis_validation.tsv` — module validation results;
- `analysis_summary.tsv` — stable run summary;
- `sessionInfo.txt` — R and package environment.

A skipped module is not a negative biological result. A failed, blocked, or
timed-out module must not be interpreted.

## 6. Continue

- [User Guide](User-Guide)
- [Configuration Reference](Configuration-Reference)
- [Results and Interpretation](Results-and-Interpretation)
- [Troubleshooting](Deployment-and-Troubleshooting)
- [Rendered first-analysis vignette](https://duceppemo.github.io/popgenVCF/articles/getting-started.html)
