<p align="center">
  <img src="https://raw.githubusercontent.com/wiki/duceppemo/popgenVCF/popgenVCF-logo.svg" width="520" alt="popgenVCF — Population Genomics Toolkit">
</p>

# popgenVCF documentation

popgenVCF is an R toolkit and command-line application for reproducible
population-genomic analysis of diploid, biallelic SNP VCF files. This wiki is
the task-oriented manual. The
[pkgdown site](https://duceppemo.github.io/popgenVCF/) provides generated API
documentation and rendered vignettes.

> **Development status:** 0.10.0 is under release-conformance review. A green
> routine CI run is not production scientific approval. Only a complete
> candidate dossier can report `READY`.

## Choose your path

### Users

- [Getting Started](Getting-Started) — install, create a configuration, and run
  a first analysis.
- [User Guide](User-Guide) — inputs, metadata modes, outputs, and normal
  workflows.
- [Configuration Reference](Configuration-Reference) — the main YAML sections
  and safe defaults.
- [Results and Interpretation](Results-and-Interpretation) — how to read QC,
  PCA, diversity, FST, ancestry, AMOVA, and spatial outputs.
- [Deployment and Troubleshooting](Deployment-and-Troubleshooting) — Docker,
  Apptainer, Conda, HPC, and common failures.

### Validators and scientific reviewers

- [Validation and Scientific Review](Validation-and-Scientific-Review) —
  validation hierarchy, reviewer packet, manual checklist, approval decision,
  and return path.
- [Scientific validation contract](https://github.com/duceppemo/popgenVCF/blob/main/docs/SCIENTIFIC_VALIDATION.md)
  — normative methods and failure policy.

### Developers and maintainers

- [Developer Guide](Developer-Guide) — repository layout, development
  environment, tests, modules, documentation, and contribution standards.
- [Release and Governance](Release-and-Governance) — release gates, evidence,
  approvals, archival boundaries, and authorization.
- [Documentation Map](Documentation-Map) — canonical specifications and where
  each type of information belongs.

## Fastest evaluation path

```bash
docker pull ghcr.io/duceppemo/popgenvcf:latest

docker run --rm --user "$(id -u):$(id -g)" \
  -e HOME=/tmp -v "$PWD:/data" \
  ghcr.io/duceppemo/popgenvcf:latest \
  --write-config /data/analysis.yml
```

Edit `analysis.yml`, then run:

```bash
docker run --rm --user "$(id -u):$(id -g)" \
  -e HOME=/tmp -v "$PWD:/data" \
  ghcr.io/duceppemo/popgenvcf:latest \
  --config /data/analysis.yml
```

Use an immutable release digest rather than `latest` for reproducible
production work.

## Project links

- [Repository](https://github.com/duceppemo/popgenVCF)
- [API reference](https://duceppemo.github.io/popgenVCF/reference/)
- [Issue tracker](https://github.com/duceppemo/popgenVCF/issues)
- [Roadmap](https://github.com/duceppemo/popgenVCF/blob/main/docs/ROADMAP.md)
- [Contributing](https://github.com/duceppemo/popgenVCF/blob/main/CONTRIBUTING.md)
- [Citation](https://github.com/duceppemo/popgenVCF/blob/main/CITATION.cff)
