# User guide

## Analysis modes

popgenVCF discovers available analyses from the VCF, metadata, configuration,
and external backends.

### VCF-only

Without metadata, sample names in the VCF are canonical identities. Available
analyses include sample and variant QC, filtering, LD pruning, PCA, IBS/MDS,
and configured ancestry backends.

### Sample metadata

Metadata can annotate outputs with fields such as location, collection date,
sex, species, or group. PCA and IBS/MDS do not require these annotations.

### Population metadata

A complete `population` column enables population diversity, FST, DAPC, AMOVA,
and population-level summaries.

### Spatial metadata

Valid `latitude` and `longitude` enable Mantel tests, isolation by distance,
and geographic figures. Samples without coordinates are excluded only from the
spatial calculation, subject to minimum complete-pair requirements.

## Identity contract

When metadata are supplied, popgenVCF requires:

- every metadata sample to exist in the VCF;
- every VCF sample to occur exactly once in metadata;
- no duplicate metadata sample identifiers;
- case-sensitive exact matching;
- deterministic reordering of metadata to VCF sample order.

Additional metadata columns are preserved.

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
