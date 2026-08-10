# User guide

## Analysis modes

popgenVCF discovers available analyses from the VCF, metadata, configuration,
and external backends.

### VCF-only

Without metadata, sample names in the VCF are canonical identities. Available
analyses include sample and variant QC, filtering, LD pruning, PCA, IBS/MDS,
pairwise kinship (KING-robust), genetic sex check (X-chromosome
heterozygosity; compares against a metadata `sex` column when present), runs
of homozygosity, and configured ancestry backends.

### Sample metadata

Metadata can annotate outputs with fields such as location, collection date,
sex, species, or group. PCA and IBS/MDS do not require these annotations. The
optional `alias` column supplies a public label for tables, figures, trees, and
reports while the `sample` column remains the immutable VCF/GDS key. A missing
alias falls back to the original sample name.

Any repeated-value annotation column -- `sex` is a common example -- also
drives an extra PCA panel automatically: once a column has enough samples per
distinct value (`analyses.pca_metadata_color_min_group`, default 3) and not
too many distinct values (`analyses.pca_metadata_color_max_levels`, default
12), the report gains a panel recolouring the same PCA fit by that column
(`07b_PCA_PC1_PC2_by_<column>`). `population`, coordinates, and identifier-like
columns are excluded automatically since they are already handled elsewhere.
See [Results and Interpretation](Results-and-Interpretation#pca) for a real
example coloured by `sex`.

### Population metadata

A complete `population` column enables population diversity, FST, sliding-
window genome scans, DAPC, AMOVA, and population-level summaries.

### Spatial metadata

Valid `latitude` and `longitude` enable Mantel tests, isolation by distance,
and geographic figures. Supply signed decimal degrees in latitude-longitude
order: north/east are positive and south/west are negative. Latitude must be
between `-90` and `90`; longitude must be between `-180` and `180`. WGS84 is
recommended. Do not supply degree symbols, DMS notation, compass suffixes,
UTM, or projected coordinates.

Samples without a finite coordinate pair are excluded only from the spatial
calculation. At least four retained samples with complete pairs are required,
and population annotations must be complete.

## Identity contract

When metadata are supplied, popgenVCF requires:

- every metadata sample to exist in the VCF;
- every VCF sample to occur exactly once in metadata;
- no duplicate metadata sample identifiers;
- case-sensitive exact matching;
- deterministic reordering of metadata to VCF sample order.

Additional metadata columns are preserved.

## Metadata file contract

Use a headered, tab-separated UTF-8 file. CSV and whitespace-separated files
are accepted, but tabs handle empty optional fields and labels with spaces more
reliably. Keep one row for every VCF sample even when annotations are missing.

`metadata_header` accepts `auto`, `yes`/`true`, or `no`/`false`; use `auto` or
`yes` for the recommended headered format. Headerless files assign only column
1 to `sample` and column 2 to `population`.

| Column | Contract |
|---|---|
| `sample` | Required, unique, non-missing, exact case-sensitive VCF key |
| `alias` | Optional unique public label; missing values fall back to `sample` |
| `population` | Optional group, but must be complete to enable population and spatial modules |
| `latitude` | Signed decimal degrees north/south; missing pair members exclude that sample spatially |
| `longitude` | Signed decimal degrees east/west; missing pair members exclude that sample spatially |
| `individual` | Optional biological-individual grouping |
| `family` | Optional family/pedigree grouping; repeated values are allowed |
| `replicate` | Optional replicate grouping; repeated values are allowed |
| `display_order` | Optional positive integer; non-missing values must be unique |
| `sex` | Optional annotation (e.g. `male`/`female`); when enough samples share each value, drives an extra per-metadata-column PCA panel like any other qualifying column |
| Other columns | Preserved as descriptive annotations |

Header names are normalized to lowercase with underscores. Common sample-key
synonyms and `pop` are recognized, but new files should use `sample` and
`population` explicitly to avoid ambiguity with the `individual` grouping.


## Worked example

```text
sample	alias	population	latitude	longitude	location	individual	family	replicate	display_order
VCF_001	Birch_01	North	45.4215	-75.6972	Ottawa	Tree_01	Family_A	NA	1
VCF_002	NA	North	NA	NA	Ottawa	Tree_02	Family_A	NA	2
VCF_003	Birch_03	South	-33.8688	151.2093	Sydney	Tree_03	Family_B	Batch_2	3
VCF_004	Birch_04	South	48.8566	2.3522	Paris	Tree_04	Family_B	Batch_2	4
```

Here `VCF_002` uses its immutable sample key as the public label and remains in
non-spatial analyses despite its missing coordinates.

Aliases must be unique and cannot collide with another sample's public name.
The original key remains available as `vcf_sample`; the VCF/GDS is never
renamed. Optional `individual`, `family`, and `replicate` values may repeat.

## Missing values

Use the literal `NA` or an empty tab/CSV field for an unavailable optional
value. Keep the row and sample key; explicit `NA` is recommended.

Do not use `.`, `N/A`, `unknown`, `-999`, or `0` as generic missing markers;
they may be interpreted as real labels or coordinates.

| Situation | Behavior |
|---|---|
| Missing `sample` | Invalid |
| Missing `alias` | Falls back to the VCF sample ID |
| Missing `population` | Disables population-dependent and spatial modules |
| Missing latitude or longitude | Excludes that sample only from spatial calculations |
| Missing other optional field | Preserved; unrelated modules remain available |
| Omitted metadata row | Invalid because VCF and metadata sample sets differ |

Population annotation is all-or-nothing for population-level analysis: a
single missing value disables the capability. Complete the annotation or run
without population analyses; do not invent a population label.

Metadata missingness is separate from missing VCF genotypes such as `./.`.
Genotype missingness is controlled by `qc.max_sample_missing` and
`qc.max_variant_missing`; those thresholds do not fill or remove metadata
values.

Inspect `02_sample_metadata_match.tsv`, `01_sample_QC.tsv`, and
`analysis_capabilities.tsv` before interpreting results.

## Major analysis families

| Family | Purpose | Important limitation |
| --- | --- | --- |
| QC and LD pruning | Missingness, allele frequency, retained markers | Thresholds define the analyzed dataset and must be reported |
| PCA | Major axes of genotype variation | Axis signs are arbitrary; PCs are not populations |
| IBS/MDS | Pairwise genetic similarity and ordination | Relatedness, missingness, and ascertainment affect distances |
| Diversity | Ho, He, FIS and population summaries | Small and uneven sample sizes increase uncertainty |
| FST | Global and pairwise differentiation | Estimator, filtering, and population definition matter |
| DAPC | Discriminant structure conditional on groups | Can overfit; retain cross-validation evidence |
| AMOVA | Hierarchical variance partitioning | Hierarchy and distance definition must be explicit |
| Ancestry | ADMIXTURE, fastStructure, and sNMF | K and components are models, not literal ancestral populations |
| Spatial genetics | Mantel and isolation by distance | Spatial autocorrelation and sampling design constrain inference |

## Output contract

Every run retains execution and provenance evidence before results should be
interpreted:

- capability, plan, ledger, module-contract, validation, and summary tables;
- the complete `analysis_results.rds` object;
- declared artifacts and generated reports;
- configuration, seeds, software versions, and `sessionInfo()`.

Publication-oriented tables and figures do not replace the machine-readable
records from which they were generated.

## Reproducible use

For an analysis intended for publication or long-term comparison:

1. retain the exact VCF and metadata checksums;
2. use a fixed package version and immutable container digest;
3. archive the configuration and resolved execution plan;
4. preserve all warnings, failures, skipped modules, and validation rows;
5. record the biological sampling design and exclusions outside the software;
6. interpret results with estimator-specific limitations;
7. archive the complete result bundle rather than selected figures only.

## Related pages

- [Configuration Reference](Configuration-Reference)
- [Results and Interpretation](Results-and-Interpretation)
- [Deployment and Troubleshooting](Deployment-and-Troubleshooting)
- [Computational reproducibility vignette](https://duceppemo.github.io/popgenVCF/articles/reproducibility.html)
- [Publication gallery](https://duceppemo.github.io/popgenVCF/articles/publication-gallery.html)
