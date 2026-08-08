# Results and interpretation

Interpret execution state before biology. Begin with
`analysis_execution_ledger.tsv` and `analysis_validation.tsv`. A module that is
failed, blocked, cancelled, timed out, or unavailable has no interpretable
biological result.

## Quality control

Report the input and retained sample/variant counts, missingness thresholds,
MAF threshold, LD-pruning rule, sample exclusions, and marker exclusions.
Inspect sequential and independent QC tables because a final count alone cannot
show which rule removed a sample or marker.

Filtering defines the analyzed dataset. Avoid choosing thresholds after seeing
the preferred population pattern.

## PCA

Check the variance table, scores, sample identities, missingness, and outliers.
PCA axes may change sign without changing the solution. Repeated or nearly
equal eigenvalues can rotate a subspace, so validation should use subspace or
residual comparisons rather than raw signed vectors.

Separation can reflect population history, relatedness, batch effects,
geography, uneven sampling, or technical artifacts. A principal component is
not itself a population.

`31_PCA_loadings.tsv` and the Manhattan/ranked loading figures report which
SNPs correlate most strongly with each retained component. Loadings are
signed (direction is preserved); "top contributing" ranks by magnitude, not
raw value. A large loading identifies a marker associated with an axis, not
a causal or functionally validated variant -- treat it as a lead for further
investigation, not a conclusion.

## IBS and MDS

Confirm sample order and matrix symmetry. Interpret low-dimensional MDS only
after inspecting the distance definition and retained eigenvalues. Close pairs
may reflect relatives or duplicates rather than population-level structure.

## Kinship

Kinship (`SNPRelate::snpgdsIBDKING()`, KING-robust, Manichaikul et al. 2010)
answers a different question than IBS/MDS above: not "how similar are these
samples overall" but "are these two individuals related, and how closely."
KING-robust is specifically chosen for robustness to population
stratification, unlike naive relatedness estimators that assume a single
homogeneous population -- a real requirement here, since this package
explicitly supports analyzing structured populations. The estimator needs
realistic marker density to behave sensibly: a handful of SNPs produces
values with no biological meaning. `relationship_degree` uses the standard
KING thresholds and is a screening classification, not a pedigree diagnosis --
a flagged pair warrants follow-up (checking for sample duplication, family
structure in the design, or a labeling error), not an automatic conclusion.
Kinship is bounded above at 0.5 (a self/duplicate pair) but is *not* bounded
below at -0.5 for arbitrarily divergent pairs; very negative values are
expected, not an error, when comparing highly differentiated populations.
Detected close relatives can bias FST, diversity, and structure analyses
computed from the same sample set -- consider whether to exclude or account
for them before interpreting those results.

## Runs of homozygosity

Runs of homozygosity (`bcftools roh`, an HMM-based method, Narasimhan et al.
2016) identify long stretches where a sample's genotype calls are
consistently homozygous -- a standard per-sample autozygosity/inbreeding
signal, distinct from both diversity's population-level FIS and kinship's
pairwise relatedness above. Allele frequencies are self-estimated from the
analyzed samples (there is no mechanism to attach an external reference-panel
frequency), which means informative site count -- and therefore sensitivity
-- drops with smaller cohorts; treat results from small sample sets
cautiously, the same caution as HWE and kinship above. `FROH` (total run
length divided by the analyzed genomic footprint) is scaled to the footprint
actually covered by the analyzed variants, **not the whole genome** -- a
single-region VCF and a whole-genome VCF give very different `FROH` for the
same underlying biology, so never compare `FROH` values across analyses that
covered different genomic spans. No short/medium/long run-length
classification is imposed; that convention is species- and study-specific
and is left for the analyst to apply if relevant.

## Diversity and FIS

Report the estimator, locus filters, missing-data handling, sample sizes, and
uncertainty. Negative or positive FIS does not identify a cause by itself;
technical artifacts, substructure, inbreeding, selection, and sample design can
produce similar summaries.

`hwe_pvalue` is a per-population exact test (Wigginton et al. 2005); it is
reporting-only and does not filter any SNP. A significant deviation has many
non-error causes (null alleles, genotyping artifacts, selection, non-random
mating, population substructure within the labeled group) and small sample
sizes have limited power to detect real deviations -- treat the p-value
distribution and the FDR-adjusted count as descriptive, not as an automatic
exclusion criterion. `private_allele` identifies alleles found in only one
retained population; it is not itself evidence of adaptive significance or
of any particular demographic history.

## FST

State the estimator explicitly. Global and pairwise values depend on population
definitions, sample size, marker ascertainment, missingness, and the genomic
region. A small numerical value can be statistically precise, while a larger
one can be uncertain.

## DAPC

DAPC is conditional on groups and retained PCs. Inspect cross-validation and
avoid retaining enough PCs to memorize individuals. Strong separation is not
independent evidence for groups when those groups defined the discriminant
analysis.

`22f_DAPC_loadings_K<k>.tsv` and the per-K Manhattan/ranked loading figures
report each SNP's contribution to every discriminant function (this
repository's DAPC groups are the unsupervised `find.clusters()` partition at
that K, not the raw metadata `population` column). A SNP with a large
contribution helped separate that K's clusters; it is not, by itself,
evidence that the SNP is biologically causal for whatever the clusters
represent.

## Ancestry backends

For ADMIXTURE, fastStructure, and sNMF:

- verify the sample-order file against every Q matrix;
- retain all replicates and fit statistics;
- align exchangeable cluster labels before comparison;
- report K-selection criteria and uncertainty;
- distinguish computational agreement from biological truth.

K is a model index, and colored components are not literal ancestral
populations without external evidence.

## AMOVA

Report the hierarchy, distance definition, permutations, missing-data handling,
and variance components. Interpret negative components transparently rather
than silently truncating them.

## Mantel and isolation by distance

Report geographic and genetic distance definitions, transformations,
permutations, complete sample pairs, and sampling design. A Mantel association
does not establish a causal spatial process and can be sensitive to
autocorrelation and clustered sampling.

## Figures and reports

Captions should identify the dataset, filters, estimator, sample size, software
version, and uncertainty or validation evidence. Publication-ready styling is
not scientific approval.

## Further reading

- [Interpreting results vignette](https://duceppemo.github.io/popgenVCF/articles/interpreting-results.html)
- [Publication gallery](https://duceppemo.github.io/popgenVCF/articles/publication-gallery.html)
- [Scientific validation](https://github.com/duceppemo/popgenVCF/blob/main/docs/SCIENTIFIC_VALIDATION.md)
