# Sample aliases

The metadata `sample` column remains the immutable identifier used to match the VCF and GDS sample IDs exactly.

An optional `Alias` column may provide a clearer public name for each sample:

```text
sample	Alias	population
VH01903_AAGMVNWM5_S12_L001	Plant_01	Ontario
VH01903_AAGMVNWM5_S18_L001	Plant_02	Quebec
```

Column names are normalized to lowercase internally, so `Alias`, `alias`, and equivalent capitalization are accepted.

## Rules

- `sample` is still mandatory and must match the VCF sample name exactly.
- `Alias` is optional.
- Blank or missing aliases fall back to the original VCF sample ID.
- Non-missing aliases must be unique.
- The final public names must also be globally unique. For example, an alias cannot equal another sample's unaliased VCF ID.

## Output behavior

The public name is used in sample-level QC tables, PCA coordinates and labels, IBS/MDS matrices, neighbour-joining trees, diversity tables, DAPC coordinates and membership matrices, figures, and downstream report artifacts.

The original VCF identity is retained as `vcf_sample` in identity-sensitive tables and provenance. The VCF/GDS itself is never renamed, so exact genotype-to-metadata linkage remains auditable.

## LD pruning compatibility

An unbounded LD base-pair window is normalized to `.Machine$integer.max` before calling `SNPRelate::snpgdsLDpruning()`. This preserves effectively unbounded behavior without passing `Inf`, which some SNPRelate/R combinations coerce to `NA` or reject.
