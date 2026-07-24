# Getting started

This page takes you from installation to a first analysis. Begin with a small
VCF and a new output directory. Do not start by enabling every optional module.

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

The minimum input is a diploid, biallelic SNP VCF:

- `.vcf`;
- BGZF `.vcf.gz` with `.tbi` or `.csi`;
- BGZF `.vcf.gz` without an index;
- ordinary gzip `.vcf.gz`;
- sorted or unsorted input.

popgenVCF reuses a valid index. Otherwise it uses BCFtools to create a sorted,
BGZF-compressed and indexed working copy. Preserve the original input and its
checksum.

Metadata are optional. When supplied, the required `sample` column must match
every VCF sample identifier exactly and uniquely:

```text
sample	population	latitude	longitude	location
Sample01	Ontario	45.4215	-75.6972	Ottawa
Sample02	Ontario	45.4200	-75.6900	Ottawa
Sample03	Quebec	45.5019	-73.5674	Montreal
```

Matching is case-sensitive. A mismatch is fatal because silent reordering could
attach population or location information to the wrong individual.

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
