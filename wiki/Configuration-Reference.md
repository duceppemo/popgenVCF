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
```

Thresholds are part of the scientific method. Choose them before examining the
desired result and report any deviation from a preregistered or validated
analysis plan.

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

**Gating column key:** "population" needs a metadata `population` column;
"2+ populations" needs at least two distinct recorded populations; "lat/long"
needs complete `geographic_columns` pairs; "opt-in" ships disabled by default
regardless of metadata.

| Key(s) | Enables | Key output(s) | Gating / notes |
| --- | --- | --- | --- |
| `diversity`, `diversity_allelic_richness`, `hwe_alpha` | Heterozygosity, allelic richness, HWE p-values, private alleles | `08_sample_diversity`, `09_population_diversity`, `32_private_alleles` | population |
| `bottleneck`, `bottleneck_n_bins` | Folded site-frequency spectrum + mode-shift bottleneck screen | `48_site_frequency_spectrum`, `49_bottleneck_mode_shift` | population |
| `pca`, `n_pcs`, `pca_loading_top_n`, `pca_metadata_color*` | Principal component analysis + per-metadata-column color panels | `12_PCA_scores`, `13_PCA_variance`, `31_PCA_loadings` | none |
| `ibs` | Identity-by-state similarity/distance + MDS | `14_IBS_similarity`, `15_IBS_distance`, `16_IBS_MDS` | none |
| `kinship`, `kinship_close_relative_threshold` | KING-robust pairwise kinship | `33_kinship_matrix`, `35_kinship_pairs`, `36_close_relatives` | none |
| `sex_check`, `sex_check_*` thresholds | Genetic sex vs. recorded sex (PLINK `--check-sex` convention) | `42_sex_check` | needs a metadata sex column |
| `roh`, `roh_gt_error_phred`, `roh_length_class_*` | Runs of homozygosity + length-class breakdown | `37_ROH_runs`, `38_ROH_sample_summary` | none |
| `tree` | Individual-level NJ tree from IBS distance | figure `52_IBS_tree`, `IBS_neighbor_joining.nwk` | none |
| `population_tree` | Population-level NJ tree from Nei's (1972) genetic distance | `46_population_genetic_distance`, figure `53_population_tree` | population, 2+ populations |
| `tree_bootstrap` | Locus-resampling bootstrap support for both NJ trees above | support values embedded in both trees | applies only when `tree` and/or `population_tree` are on |
| `ml_tree` | Maximum-likelihood tree (GTR+Gamma, phangorn) -- complements the NJ tree above | `54_ML_tree_summary` | **opt-in**; needs the optional phangorn package |
| `population_assignment` | Frequency-based self-assignment test (Paetkau et al. 1995, 2004) | `47_population_assignment` | population, 2+ populations |
| `fst` | Weir & Cockerham global/pairwise FST, bootstrap CIs | `17_global_FST`, `18_pairwise_FST`, `20_pairwise_FST_bootstrap_CI` | population, 2+ populations |
| `genome_scan`, `genome_scan_window_bp`, `genome_scan_step_bp`, `genome_scan_min_snps` | Sliding-window FST/diversity scan + exploratory outlier flagging | `39_genome_scan_fst`, `40_genome_scan_diversity`, `41_genome_scan_FST_outliers` | population, 2+ populations |
| `pcadapt`, `pcadapt_k`, `pcadapt_min_maf`, `pcadapt_fdr_alpha` | PCA-based outlier/selection scan with a calibrated null (unlike `genome_scan`'s outlier flags above) | `59_pcadapt_outliers`, `59b_pcadapt_significant_outliers` | none -- works from genotypes alone |
| `ld_decay`, `ld_decay_max_distance_bp`, `ld_decay_bin_bp`, `ld_decay_slide` | Linkage-disequilibrium decay curve | `43_LD_decay` | none |
| `ne_ld`, `ne_ld_max_snps` | LD-based effective population size (Waples 2006) | `45_Ne_LD` | population |
| `dapc`, `dapc_k`, `dapc_cross_validation`, `dapc_loading_top_n` | Discriminant analysis of principal components, per K | `21_DAPC_diagnostics`, `22_DAPC_coordinates_K<k>`, `22e_DAPC_K_selection_*` | population |
| `amova` | Analysis of molecular variance | `23_AMOVA_components`, `24_AMOVA_phi_statistics` | population, 2+ populations |
| `clonality`, `clonality_genotype_curve_replicates`, `clonality_ia_permutations` | Multilocus genotype (MLG) matching, clonal diversity, minimum spanning network | `56_MLG_diversity_summary`, `57_MLG_groups`, `58b_MSN_edges` | population |
| `sexbias`, `sexbias_test`, `sexbias_permutations` | Sex-biased dispersal test (Goudet, Perrin, and Waser 2002) | `60_sexbias_AIc_by_sample`, `60b_sexbias_test_summary` | population; needs the optional hierfstat package and a metadata sex column |
| `mantel`, `isolation_by_distance` | Mantel test + isolation-by-distance regression (either flag turns the module on) | `25_Mantel_IBD_summary`, `26_IBD_pairs` | lat/long |
| `spatial_autocorrelation`, `spatial_autocorrelation_bins`, `spatial_autocorrelation_permutations` | Distance-class genetic autocorrelation correlogram | `50_spatial_autocorrelation` | lat/long |
| `chromosome_specific`, `chromosome_min_snps` | Per-chromosome PCA + FST | `chromosome_summary` | population, 2+ populations |
| `bootstrap` (`enabled`, `replicates`, `unit`) | Locus-resampling confidence intervals on `diversity`'s estimates | `11_diversity_bootstrap_CI` | population (piggybacks on `diversity`) |
| `structure` (`replicates`, `seeds`, `reproducibility_rmse`, `minimum_cluster_correlation`) | **Not** the ADMIXTURE/fastStructure backends below, despite the name -- reproducibility replicates for DAPC's own K-fits | `22c_DAPC_reproducibility_K<k>` | applies only when `dapc` is on |
| `admixture` (`enabled`, `executable`, `plink_prefix`, `k`, `cv_folds`, ...) | ADMIXTURE cluster analysis | `27_ADMIXTURE_CV`, `28_ADMIXTURE_Q_K<k>` | **opt-in**; needs the `admixture` executable on `PATH` |
| `faststructure` (`enabled`, `structure_executable`, `choosek_executable`, `plink_prefix`, `k`, ...) | fastStructure cluster analysis | `29_fastStructure_runs`, `29_fastStructure_Q_K<k>` | **opt-in**; needs `structure.py`/`chooseK.py` |
| `snmf` (`enabled`, `geno_file`, `q_sample_file`, `k`, `repetitions`, `entropy`, ...) | sNMF cluster analysis (LEA) | `30_sNMF_cross_entropy`, `30_sNMF_Q_K<k>` | **opt-in**; needs the optional LEA package |

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
