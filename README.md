<p align="center">
  <img src="man/figures/popgenVCF-logo.svg" width="560" alt="popgenVCF — Population Genomics Toolkit">
</p>

<p align="center">
  <a href="https://github.com/duceppemo/popgenVCF/actions/workflows/R-CMD-check.yaml"><img alt="R CMD check" src="https://github.com/duceppemo/popgenVCF/actions/workflows/R-CMD-check.yaml/badge.svg?branch=main"></a>
  <a href="https://github.com/duceppemo/popgenVCF/actions/workflows/scientific-validation.yaml"><img alt="Scientific validation" src="https://github.com/duceppemo/popgenVCF/actions/workflows/scientific-validation.yaml/badge.svg?branch=main"></a>
  <a href="https://github.com/duceppemo/popgenVCF/actions/workflows/test-coverage.yaml"><img alt="Test coverage" src="https://codecov.io/gh/duceppemo/popgenVCF/branch/main/graph/badge.svg"></a>
  <a href="LICENSE"><img alt="License: MIT" src="https://img.shields.io/badge/license-MIT-0B6B62.svg"></a>
  <a href="https://github.com/duceppemo/popgenVCF/wiki"><img alt="Documentation: Wiki" src="https://img.shields.io/badge/docs-wiki-123B4A.svg"></a>
  <a href="https://doi.org/10.5281/zenodo.22289055"><img alt="DOI" src="https://img.shields.io/badge/DOI-10.5281%2Fzenodo.22289055-blue.svg"></a>
</p>

**popgenVCF** is an R toolkit and command-line application for reproducible
population-genomic analysis of diploid, biallelic SNP VCF files. It provides
quality control, PCA, IBS/MDS, diversity, FST, kinship, genetic sex check,
runs of homozygosity, sliding-window genome scans, DAPC, AMOVA, population
structure, spatial genetics, publication outputs, and machine-readable
validation evidence.

> Development series: **1.0.9** — active development on `main` toward
> the next release, past the current stable release **1.0.8** (released;
> Zenodo DOI reconciliation for this release is still pending).
> No development build should be treated as release-approved unless its own
> production dossier reports `READY`.

## See it before you install it

Real figures and the complete PDF report from a real 160-sample, 8-population
1000 Genomes chromosome 22 run are in the
[Results and Interpretation wiki page](https://github.com/duceppemo/popgenVCF/wiki/Results-and-Interpretation)
and [`docs/examples/chr22-quickstart-report.pdf`](https://github.com/duceppemo/popgenVCF/blob/main/docs/examples/chr22-quickstart-report.pdf).
Already have the package installed? That same dataset ships with it --
`vignette("quickstart", package = "popgenVCF")` runs the full pipeline
locally in a few minutes with no download.

## Quick start with Docker

Docker is the simplest evaluation path:

```bash
docker pull ghcr.io/duceppemo/popgenvcf:latest

docker run --rm --user "$(id -u):$(id -g)" \
  -e HOME=/tmp -v "$PWD:/data" \
  ghcr.io/duceppemo/popgenvcf:latest \
  --write-config /data/analysis.yml
```

The generated file records the logical CPUs and memory visible to the
container. It lists every built-in analysis enabled; analyses needing
population or geographic metadata skip themselves automatically when that
metadata isn't available, while optional ancestry backends stay disabled
until explicitly requested since they need an external tool. Backend
`threads: auto` values inherit `compute.threads`.

Edit `analysis.yml` to use container paths such as `/data/cohort.vcf.gz` (see the
[Configuration Reference](https://github.com/duceppemo/popgenVCF/wiki/Configuration-Reference)), then run:

```bash
docker run --rm --user "$(id -u):$(id -g)" \
  -e HOME=/tmp -v "$PWD:/data" \
  ghcr.io/duceppemo/popgenvcf:latest \
  --config /data/analysis.yml
```

Use an immutable image digest from a GitHub Release for a reproducible
production analysis.

## Local installation

System requirements include R 4.3 or newer, BCFtools, and HTSlib:

```bash
git clone https://github.com/duceppemo/popgenVCF.git
cd popgenVCF
Rscript install_popgenVCF.R
```

Create and run a configuration through the R entry point:

```bash
Rscript -e 'popgenVCF::cli_main(c("--write-config", "analysis.yml"))'
Rscript -e 'popgenVCF::cli_main(c("--config", "analysis.yml"))'
```

Conda/Mamba, Apptainer, HPC, ancestry-backend, and development installation
paths are covered in the wiki.

## Documentation

Choose the section that matches your role:

| Audience | Start here |
| --- | --- |
| Users | [Getting started](https://github.com/duceppemo/popgenVCF/wiki/Getting-Started) · [User guide](https://github.com/duceppemo/popgenVCF/wiki/User-Guide) · [Results and interpretation](https://github.com/duceppemo/popgenVCF/wiki/Results-and-Interpretation) |
| Deployers | [Containers, HPC, and troubleshooting](https://github.com/duceppemo/popgenVCF/wiki/Deployment-and-Troubleshooting) |
| Validators and scientific reviewers | [Validation and scientific review](https://github.com/duceppemo/popgenVCF/wiki/Validation-and-Scientific-Review) |
| Developers and contributors | [Developer guide](https://github.com/duceppemo/popgenVCF/wiki/Developer-Guide) |
| Maintainers and release owners | [Release and governance](https://github.com/duceppemo/popgenVCF/wiki/Release-and-Governance) |

- [Wiki home](https://github.com/duceppemo/popgenVCF/wiki) — task-oriented
  documentation and project processes.
- [pkgdown reference](https://duceppemo.github.io/popgenVCF/) — generated API
  reference and long-form vignettes.
- [Documentation map](https://github.com/duceppemo/popgenVCF/wiki/Documentation-Map)
  — canonical repository documents and specifications.

## Contributing and citation

See [CONTRIBUTING.md](CONTRIBUTING.md) before proposing a change. Scientific
changes require estimator definitions, independent validation, and retained
evidence appropriate to their risk.

Citation metadata are available in [CITATION.cff](CITATION.cff). popgenVCF
1.0.8 is archived on Zenodo at
[10.5281/zenodo.22289055](https://doi.org/10.5281/zenodo.22289055)
(concept DOI [10.5281/zenodo.21747067](https://doi.org/10.5281/zenodo.21747067),
which always resolves to the latest version). popgenVCF is licensed under the
[MIT License](LICENSE).
