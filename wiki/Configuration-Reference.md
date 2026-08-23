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
  sample_column: null
  population_column: null
  geographic_columns: [latitude, longitude]
```

`vcf` is required. `metadata` is optional. Inside Docker, all paths must use
container paths below the mounted directory.

`metadata_header` accepts `auto`, `yes`/`true`, or `no`/`false`. A headered
tab-separated file is recommended. `geographic_columns` is ordered latitude
first and longitude second; the canonical column names shown above support
capability discovery.
Coordinates must be signed decimal degrees, not DMS, UTM, or projected units.

`sample_column`/`population_column` (default `null`) name a metadata column
to treat as `sample`/`population` when it is not already one of the
recognized synonyms, without renaming it in the file. An explicit value
always wins over auto-detection, even over an existing literal `sample`/
`population` column, and requires a headered metadata file.

See the [User Guide metadata contract](User-Guide#metadata-file-contract) for
aliases, identity fields, population completeness, coordinate ranges, and
missing-value behavior.

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

ADMIXTURE and fastStructure automatically generate matching PLINK
`.bed/.bim/.fam` files from the retained samples and LD-pruned SNPs. An
optional `plink_prefix` may select an existing compatible bundle instead.
The workflow records sample order from the selected PLINK `.fam` file so that
Q-matrix rows remain explicit. LEA/sNMF similarly generates a `.geno` file and
matching sample-order file from the same retained data. Existing sNMF files
may be supplied through `geno_file` and `q_sample_file` only as a compatible
pair of optional overrides.

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
