# Deployment and troubleshooting

## Docker

```bash
docker pull ghcr.io/duceppemo/popgenvcf:latest
docker run --rm --user "$(id -u):$(id -g)" \
  -e HOME=/tmp -v "$PWD:/data" \
  ghcr.io/duceppemo/popgenvcf:latest \
  --config /data/analysis.yml
```

Use `/data/...` paths in the configuration. For reproducibility, replace
`latest` with an immutable digest recorded by the matching release.

Full container guidance:

- [Container images](https://github.com/duceppemo/popgenVCF/blob/main/docs/user/container-images.md)
- [Rendered containers and HPC vignette](https://duceppemo.github.io/popgenVCF/articles/containers-and-hpc.html)

## Conda or Mamba

```bash
conda config --set channel_priority strict
mamba env create --file inst/conda/environment.yml
conda activate popgenvcf
Rscript inst/scripts/install-bioconductor.R
R CMD INSTALL .
bash inst/scripts/verify-environment.sh
```

Record the exported environment and installed package versions for retained
analyses.

## Apptainer

Use the maintained instructions for building, pulling, verifying, and running
the SIF:

- [Apptainer guide](https://github.com/duceppemo/popgenVCF/blob/main/docs/user/apptainer.md)

Retain the SIF checksum. A mutable tag is insufficient production provenance.

## HPC

Request threads and memory consistent with the configuration. Keep the VCF,
work cache, and output on filesystems appropriate for the cluster. Materialize
remote datasets and optional external software before offline jobs begin.

Retain scheduler metadata, job ID, node/partition, resource request, exit state,
elapsed time, environment modules, container digest, and logs.

## Ancestry backends

ADMIXTURE, fastStructure, and LEA/sNMF have separate runtime and provenance
requirements:

- [Ancestry backend guide](https://github.com/duceppemo/popgenVCF/blob/main/docs/user/ancestry-backends.md)

An executable being present does not validate its scientific output.

## Common failures

### VCF cannot be indexed

Confirm BCFtools is on `PATH`, the VCF is readable, contig/position fields are
valid, the filesystem is writable, and the file is BGZF rather than ordinary
gzip. Preserve the original and allow popgenVCF to make a sorted indexed working
copy.

### Metadata mismatch

Compare exact VCF sample IDs with the `sample` column. Check case, whitespace,
duplicate rows, missing samples, and unexpected extra samples. Do not use fuzzy
matching.

### Module skipped

Inspect `analysis_capabilities.tsv`. Missing population or coordinate metadata
causes dependent analyses to be skipped by design. Skipped is not a biological
result.

### Module blocked

Inspect the execution plan and ledger. A failed prerequisite blocks dependent
modules to prevent misleading partial output.

### External backend unavailable

Check the executable path, environment activation, permissions, input format,
and sample-order file. Retain version and checksum output.

### Report does not render

Confirm Pandoc or Quarto and the relevant R packages are available. Results and
machine-readable tables may still be valid when an optional presentation layer
fails; record the failure rather than hiding it.

### Results differ between machines

Compare input checksums, configuration, seed, package versions, BLAS, external
tool versions, thread counts, container digest, and sample/marker order. Use the
declared numerical tolerance and comparison method; do not expect signed PCA
vectors or exchangeable ancestry labels to match naively.

### Existing output causes a refusal

Use a new output directory or archive the prior run. Fail-closed workflows often
refuse non-empty directories to prevent mixing evidence from different runs.

## Diagnostic record

When reporting a problem, include:

- exact commit/package version and installation method;
- operating system or container digest;
- redacted configuration;
- input type, size, and checksums where shareable;
- execution ledger and validation table;
- complete error and relevant logs;
- the smallest reproducible non-sensitive example.

See the [troubleshooting vignette](https://duceppemo.github.io/popgenVCF/articles/troubleshooting.html).
