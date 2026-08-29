# Configuration reference

Generate the version-matched default instead of copying a stale configuration:

```bash
Rscript -e 'popgenVCF::cli_main(c("--write-config", "analysis.yml"))'
```

The canonical example is
[`inst/example_config.yml`](https://github.com/duceppemo/popgenVCF/blob/main/inst/example_config.yml) --
every key this version supports, each with an inline comment explaining what
it does. `--write-config` writes the same keys with the same defaults, just
without the comments (plain `yaml::write_yaml()` cannot preserve them). Copy
the canonical example when you want the commentary; use `--write-config` when
you just want a version-matched starting point to edit.

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
  ld_slide_max_bp: .inf
  ld_slide_max_n: 50
  ld_start_pos: first
```

Thresholds are part of the scientific method. Choose them before examining the
desired result and report any deviation from a preregistered or validated
analysis plan.

Every value here is genuinely configurable, with no forced override.
`max_variant_missing` feeds three places consistently: `variant_qc()`'s own
QC gate, `ld_prune_exact()`'s `SNPRelate::snpgdsLDpruning()` call, and the
ROH module's own missingness gate. `ld_r2`, `ld_slide_max_bp`,
`ld_slide_max_n`, and `ld_start_pos` are `SNPRelate::snpgdsLDpruning()`'s own
parameters -- distance/window/count bounds and where each chromosome's
pruning starts. The values shown above are the defaults every prior release
used, kept as defaults for continuity, not enforced as fixed.

## Analyses

Every `analyses.*` block below maps to one analysis module. `enabled: false`
(or the boolean flag shown) turns a module off outright; a module whose
required metadata is missing skips itself automatically instead (a WARNING,
plus a row in `analysis_capabilities.tsv` explaining why) -- toggling it off
explicitly is for modules you deliberately do not want, not a substitute for
having the right metadata.

Do not enable a module merely because the software supports it. The input,
metadata, sample size, estimator assumptions, and intended claim must justify
it. See the [Results and Interpretation guide](Results-and-Interpretation)
for how to read each module's output.

Each row's key is the module's main on/off flag; sub-parameters (thresholds,
replicate counts, K ranges, ...) sit beside it in `inst/example_config.yml`,
each with its own comment. **Gating:** `pop` = needs a metadata `population`
column; `pop2` = needs 2+ distinct populations; `geo` = needs complete
`geographic_columns` pairs; `opt-in` = off by default regardless of metadata.

| Key | Enables | Output | Gating |
| --- | --- | --- | --- |
| `diversity` | Heterozygosity, allelic richness, HWE, private alleles | `08_sample_diversity` | pop |
| `bottleneck` | Site-frequency spectrum + bottleneck screen | `49_bottleneck_mode_shift` | pop |
| `pca` | Principal component analysis | `12_PCA_scores` | -- |
| `ibs` | IBS similarity/distance + MDS | `16_IBS_MDS` | -- |
| `kinship` | KING-robust pairwise kinship | `33_kinship_matrix` | -- |
| `sex_check` | Genetic vs. recorded sex | `42_sex_check` | sex column |
| `roh` | Runs of homozygosity | `37_ROH_runs` | -- |
| `tree` | Individual NJ tree (IBS distance) | figure `52_IBS_tree` | -- |
| `population_tree` | Population NJ tree (Nei's D) | `46_population_genetic_distance` | pop2 |
| `tree_bootstrap` | Bootstrap support for both NJ trees above | -- | needs `tree`/`population_tree` |
| `ml_tree` | ML tree (GTR+Gamma, phangorn) | `54_ML_tree_summary` | opt-in, phangorn |
| `population_assignment` | Self-assignment test (Paetkau et al.) | `47_population_assignment` | pop2 |
| `fst` | Weir & Cockerham FST | `17_global_FST` | pop2 |
| `genome_scan` | Sliding-window FST/diversity scan | `39_genome_scan_fst` | pop2 |
| `pcadapt` | PCA-based outlier/selection scan, calibrated null | `59_pcadapt_outliers` | -- |
| `ld_decay` | Linkage-disequilibrium decay curve | `43_LD_decay` | -- |
| `ne_ld` | LD-based effective population size | `45_Ne_LD` | pop |
| `dapc` | Discriminant analysis of PCs, per K | `21_DAPC_diagnostics` | pop |
| `amova` | Analysis of molecular variance | `23_AMOVA_components` | pop2 |
| `clonality` | MLG matching, clonal diversity, MSN | `56_MLG_diversity_summary` | pop |
| `sexbias` | Sex-biased dispersal test | `60_sexbias_AIc_by_sample` | pop, sex column, hierfstat |
| `mantel` / `isolation_by_distance` | Mantel test + IBD regression (either flag enables) | `25_Mantel_IBD_summary` | geo |
| `spatial_autocorrelation` | Genetic autocorrelation correlogram | `50_spatial_autocorrelation` | geo |
| `chromosome_specific` | Per-chromosome PCA + FST | `chromosome_summary` | pop2 |
| `bootstrap` | CIs on `diversity`'s estimates | `11_diversity_bootstrap_CI` | pop |
| `structure` | DAPC reproducibility replicates (misleading name -- unrelated to the backends below) | `22c_DAPC_reproducibility_K<k>` | needs `dapc` |
| `admixture` | ADMIXTURE cluster analysis | `27_ADMIXTURE_CV` | opt-in, `admixture` on `PATH` |
| `faststructure` | fastStructure cluster analysis | `29_fastStructure_runs` | opt-in, `structure.py`/`chooseK.py` |
| `snmf` | sNMF cluster analysis (LEA) | `30_sNMF_cross_entropy` | opt-in, LEA package |

`analysis_capabilities.tsv` in every run's output records which modules
actually ran, were skipped, or were disabled, and why.

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
