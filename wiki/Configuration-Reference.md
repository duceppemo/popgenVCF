# Configuration reference

Generate the version-matched default instead of copying a stale configuration:

```bash
Rscript -e 'popgenVCF::cli_main(c("--write-config", "analysis.yml"))'
```

The canonical example is
[`inst/example_config.yml`](https://github.com/duceppemo/popgenVCF/blob/main/inst/example_config.yml).

## Input

```yaml
input:
  vcf: /data/cohort.vcf.gz
  metadata: /data/metadata.tsv
  metadata_header: auto
```

`vcf` is required. `metadata` is optional. Inside Docker, all paths must use
container paths below the mounted directory.

## Output

```yaml
output:
  directory: /data/results
  figure_formats: [png, pdf]
```

Use a new or intentionally managed output directory. Do not mix outputs from
different datasets or candidate commits.

## Compute

```yaml
compute:
  threads: 8
  seed: 42
```

Record both values. A fixed seed controls supported stochastic operations but
does not make different software stacks or backends identical.

## Quality control

```yaml
qc:
  maf: 0.05
  max_sample_missing: 0.20
  max_variant_missing: 0.20
  ld_r2: 0.20
```

Thresholds are part of the scientific method. Choose them before examining the
desired result and report any deviation from a preregistered or validated
analysis plan.

## Analyses

The generated file contains supported module settings. Common controls include
the requested number of PCs, population analyses, spatial modules, bootstrap
settings, and ancestry backends.

Do not enable a module merely because the software supports it. The input,
metadata, sample size, estimator assumptions, and intended claim must justify
it. `analysis_capabilities.tsv` explains what was available or skipped.

## Reports

```yaml
report:
  enabled: true
```

Reports summarize retained results. They do not convert a failed, blocked, or
unvalidated module into a usable result.

## Ancestry backends

ADMIXTURE and fastStructure require a matching PLINK `.bed/.bim/.fam` prefix.
LEA/sNMF requires a `.geno` file. Every backend requires an explicit
sample-order file matching Q-matrix rows.

Use the maintained backend guide:

- [Ancestry backend installation](https://github.com/duceppemo/popgenVCF/blob/main/docs/user/ancestry-backends.md)

## Validation checklist

Before a long run:

- paths resolve in the actual environment;
- VCF sample IDs are unique;
- metadata IDs match exactly;
- output does not contain a prior incompatible run;
- thread and memory requests fit the scheduler allocation;
- seeds and thresholds are recorded;
- optional external executables are discoverable;
- backend input and sample-order checksums match;
- report generation dependencies are installed.

## CLI help

Run the version-matched help:

```bash
Rscript -e 'popgenVCF::cli_main(c("--help"))'
```

For programmatic configuration, see the
[API reference](https://duceppemo.github.io/popgenVCF/reference/).
