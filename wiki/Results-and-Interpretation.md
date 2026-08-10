# Results and interpretation

Every figure below is real output from the bundled quickstart dataset (160
real 1000 Genomes chromosome 22 samples across 8 populations -- see
[Getting Started](Getting-Started) to run it yourself in a few minutes) so
you can see exactly what popgenVCF produces before deciding whether it fits
your data. The complete PDF report from this same run is available at
[`docs/examples/chr22-quickstart-report.pdf`](https://github.com/duceppemo/popgenVCF/blob/main/docs/examples/chr22-quickstart-report.pdf).

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

![Minor allele frequency distribution from the quickstart example, with the retention threshold marked](figures/01_MAF.png)

On the quickstart example, MAF filtering (`maf = 0.05`) reduced 21,418
biallelic SNPs to 1,969, and LD pruning further reduced that to 357 SNPs
used for PCA/IBS/kinship.

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

![PCA of the quickstart example, coloured by population](figures/07_PCA_PC1_PC2.png)

![PCA SNP loadings by chromosome position, one panel per retained component (PC1..PC10), from the quickstart example](figures/17_PCA_loadings_manhattan.png)

![PCA SNP loadings ranked by descending magnitude within each component, from the quickstart example](figures/18_PCA_loadings_ranked.png)

The 8 sampled populations separate along the first two components largely by
continental origin, as expected for real, geographically diverse human data.

The report also draws one additional PC1/PC2 panel per other metadata column
that qualifies for colouring: enough samples per distinct value
(`analyses.pca_metadata_color_min_group`, default 3) and not too many
distinct values (`analyses.pca_metadata_color_max_levels`, default 12).
`population` itself, continuous coordinates (`latitude`/`longitude`), and
identifier-like columns are excluded automatically. In the quickstart
metadata, `sex` qualifies:

![PCA of the quickstart example, recoloured by the sex metadata column instead of population](figures/07b_PCA_PC1_PC2_by_sex.png)

Sex shows no separation here (ANOVA on this exact PCA fit: p = 0.24 on PC1,
p = 0.62 on PC2) -- itself the expected, useful result for an autosomal
analysis: population structure, not sex, drives this PCA. A
per-metadata-column panel that *does* show separation on your own data would
be a genuine signal worth investigating (a batch effect, a confound, or real
biology), not something to assume is noise.

## IBS and MDS

Confirm sample order and matrix symmetry. Interpret low-dimensional MDS only
after inspecting the distance definition and retained eigenvalues. Close pairs
may reflect relatives or duplicates rather than population-level structure.

![Pairwise IBS heatmap from the quickstart example, average-linkage clustered](figures/09_IBS_heatmap.png)

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

![Kinship heatmap from the quickstart example](figures/21_kinship_heatmap.png)

![KING-robust IBS0-vs-kinship diagnostic scatter from the quickstart example, coloured by relationship degree](figures/22_kinship_IBS0_vs_kinship.png)

The quickstart dataset deliberately includes two known real duplicate pairs
as a worked example: `HG03873`/`HG03998` (kinship 0.4525) -- individuals
labelled as two *different* populations (ITU and STU) and two *different*
sexes in the bundled metadata, that are nonetheless the same/an
identical-twin pair, exactly the kind of labeling issue this analysis is
meant to surface -- and `NA19331`/`NA19334` (kinship 0.4459, both LWK, both
recorded with the same sex, as expected for a genuine duplicate). Both pairs
are correctly classified `duplicate/MZ twin`.

## Sex check

Genetic sex inference from X-chromosome heterozygosity (Visscher et al. 2010's
F-statistic, the same convention PLINK's `--check-sex` uses) flags samples
whose metadata-reported `sex` disagrees with what their genotypes imply -- a
standard upstream QC check, since a mislabelled sex can also signal a sample
swap or contamination. `SNPRelate::snpgdsIndInb(method = "mom.visscher")`,
already used elsewhere in this package, is reused unchanged, restricted to
X-chromosome SNPs: hemizygous males show excess homozygosity (F close to 1)
relative to diploid females (F close to 0). Default thresholds (male F >
0.8, female F < 0.2) are PLINK's own documented defaults, verified on a
hand-built synthetic fixture with known male/female genotypes before
shipping this feature: simulated males gave F in [0.84, 1.13], simulated
females in [-0.06, 0.10] -- a clean separation either side of both
thresholds. Pseudoautosomal (PAR) regions are not excluded by default, since
their boundaries are genome-build-specific and this package accepts VCFs
from any build or organism; PAR SNPs dilute, not reverse, the separation.

This module needs only genotypes (no metadata required, though a `sex`
column enables the mismatch comparison) and is on by default
(`analyses.sex_check`). It skips transparently -- no table, no figure --
when fewer than 20 QC-passing X-chromosome SNPs are found, exactly like
Mantel/isolation-by-distance skipping without coordinates. **The quickstart
example does this**: its chromosome 22 region contains no X chromosome, so
sex check has nothing to compute here despite the dataset's real `sex`
metadata column.

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

![FROH by sample from the quickstart example](figures/24_ROH_FROH_by_sample.png)

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

![Observed heterozygosity by population from the quickstart example](figures/05_sample_heterozygosity.png)

## FST

State the estimator explicitly. Global and pairwise values depend on population
definitions, sample size, marker ascertainment, missingness, and the genomic
region. A small numerical value can be statistically precise, while a larger
one can be uncertain.

![Pairwise Weir-Cockerham FST from the quickstart example](figures/10_pairwise_FST.png)

The global FST estimate for the quickstart example is 0.0915, consistent
with real, moderate differentiation across 8 geographically diverse
populations at this marker density.

## Genome scans

Windowed FST and diversity (`39_genome_scan_fst.tsv`,
`40_genome_scan_diversity.tsv`) track the same statistics above along
physical position, rather than only as a single genome-wide number --
useful for spotting localized differentiation or diversity outliers.
Windows are non-overlapping by default (`analyses.genome_scan_step_bp`
equals `analyses.genome_scan_window_bp`); a smaller step gives a real
overlapping/sliding scan. `41_genome_scan_FST_outliers.tsv` is a
descriptive ranking (highest-FST windows), **not** a significance test --
no permutation or null distribution is computed, so a high-FST window is a
candidate worth further investigation, not a statistically confirmed
selection signal. Always check a window's `n_snps` before treating its
estimate as meaningful: windows below `analyses.genome_scan_min_snps`
(default 5) are reported as `NA` rather than a misleadingly precise value
from too few markers, and even windows just above that threshold can be
noisy.

![Sliding-window FST scan across the quickstart example's analyzed chr22 region, 50kb non-overlapping windows](figures/25_genome_scan_FST_manhattan.png)

The 20 windows range from FST 0.051 to 0.172. The single highest, 20.80-20.85 Mb (FST=0.172, 104 SNPs), is a candidate worth further investigation under the caveats above, not a confirmed selection signal -- especially since it is only moderately above the region's known global FST of 0.0915.

![Sliding-window diversity scan across the quickstart example's analyzed chr22 region, coloured by population](figures/26_genome_scan_diversity_manhattan.png)

Mean expected heterozygosity per window per population ranges from 0.099 to 0.398 across the 8 populations -- the same kind of real, biologically plausible spread reported for the genome-wide diversity summary above, now resolved by position.

## DAPC

DAPC is conditional on groups and retained PCs. Inspect cross-validation and
avoid retaining enough PCs to memorize individuals. Strong separation is not
independent evidence for groups when those groups defined the discriminant
analysis.

`22d_DAPC_K_selection.tsv`/`22e_DAPC_K_selection.tsv` and this figure report
the automatic cluster-number consensus across BIC, mean cross-validation
success, Calinski-Harabasz, Davies-Bouldin, and replicate-membership RMSE.
It is a starting point, not a substitute for domain judgment about how many
groups the data can support.

![DAPC cluster-number selection from the quickstart example](figures/12_DAPC_cluster_number_selection.png)

The full report (`22_DAPC_coordinates_K<k>.tsv`, `22f_DAPC_loadings_K<k>.tsv`,
and figures) contains one complete set of DAPC results for **every**
requested K (2 through 10 by default), not only the consensus K shown here.
Before trusting any given K's clustering, check its
`replicate_max_rmse` in `21_DAPC_diagnostics.tsv` against
`analyses.structure.reproducibility_rmse` (default 0.05): a K at or below
that threshold has reproducible replicate membership across independent
`find.clusters()`/`dapc()` runs; a K above it does not, and its clusters and
loadings should not be interpreted, only the consensus itself and the
diagnostics that led to it. In the quickstart example, the consensus K=3 has
`replicate_max_rmse` effectively 0 (fully reproducible); several other K
values in the same run exceed the threshold and are not shown here for that
reason.

`22f_DAPC_loadings_K<k>.tsv` and the per-K Manhattan/ranked loading figures
report each SNP's contribution to every discriminant function (this
repository's DAPC groups are the unsupervised `find.clusters()` partition at
that K, not the raw metadata `population` column). A SNP with a large
contribution helped separate that K's clusters; it is not, by itself,
evidence that the SNP is biologically causal for whatever the clusters
represent.

![DAPC scatter at the consensus K=3 from the quickstart example](figures/11_DAPC_K3.png)

![DAPC SNP loadings by chromosome position at K=3, one panel per discriminant function, from the quickstart example](figures/15_DAPC_loadings_manhattan_K3.png)

![DAPC SNP loadings ranked by descending contribution at K=3, from the quickstart example](figures/16_DAPC_loadings_ranked_K3.png)

## Ancestry backends

**ADMIXTURE, fastStructure, and sNMF are not run by default** (`analyses.admixture`, `analyses.faststructure`, and `analyses.snmf` all default to `enabled: false`). Unlike every other module on this page, each is an external, optional dependency -- ADMIXTURE and fastStructure are separate command-line programs, not R packages, and none is required for the rest of popgenVCF to work. See [Installing and configuring ancestry backends](https://github.com/duceppemo/popgenVCF/blob/main/docs/user/ancestry-backends.md) to enable one.

For ADMIXTURE, fastStructure, and sNMF:

- verify the sample-order file against every Q matrix;
- retain all replicates and fit statistics;
- align exchangeable cluster labels before comparison;
- report K-selection criteria and uncertainty;
- distinguish computational agreement from biological truth.

K is a model index, and colored components are not literal ancestral
populations without external evidence.

The figures below are from real ADMIXTURE, fastStructure, and sNMF runs
against the quickstart example (K=2..9 each, ADMIXTURE with 5-fold
cross-validation and sNMF with 5 repetitions per K) -- deliberately enabled
just for this documentation, not part of the bundled dataset's default
analysis. All three are run independently here, not cross-validated
against each other (a separate, heavier release-gate evidence process
covered in `docs/SCIENTIFIC_CONCORDANCE.md`); their three different
consensus K values below (7, 3, 6) are a real, expected illustration of why
this section says to distinguish computational agreement from biological
truth rather than average or otherwise reconcile them by default.

### ADMIXTURE

![ADMIXTURE cluster-number selection from the quickstart example](figures/13b_ADMIXTURE_cluster_number_selection.png)

Cross-validation error is lowest and flattens from K=6 through K=9; the
automatic consensus selects **K=7**. The full report
(`27_ADMIXTURE_CV.tsv`, `28_ADMIXTURE_Q_K<k>.tsv`, and figures) contains a
complete Q matrix and pair of figures for **every** requested K, not only
the one shown here.

![ADMIXTURE ancestry proportions at K=7 from the quickstart example, samples grouped by population](figures/14_ADMIXTURE_Q_K7.png)

![ADMIXTURE ancestry proportions at K=7 from the quickstart example, samples ordered by dominant inferred cluster instead of population label](figures/14_ADMIXTURE_Q_data_driven_K7.png)

### fastStructure

![fastStructure cluster-number selection from the quickstart example](figures/13d_fastStructure_cluster_number_selection.png)

Marginal likelihood peaks at K=3 (-0.849, versus -0.854 to -0.872 elsewhere
in the K=2..9 range); the automatic consensus selects **K=3**. The full
report (`29_fastStructure_runs.tsv`, `29_fastStructure_Q_K<k>.tsv`, and
figures) contains a complete Q matrix and pair of figures for **every**
requested K, not only the one shown here.

![fastStructure ancestry proportions at K=3 from the quickstart example, samples grouped by population](figures/14_fastStructure_Q_K3.png)

![fastStructure ancestry proportions at K=3 from the quickstart example, samples ordered by dominant inferred cluster instead of population label](figures/14_fastStructure_Q_data_driven_K3.png)

### sNMF

![sNMF cluster-number selection from the quickstart example](figures/13c_sNMF_cluster_number_selection.png)

Mean cross-entropy across replicates is lowest at K=6 (0.627, versus 0.628
to 0.650 elsewhere in the K=2..9 range); the automatic consensus selects
**K=6**. The full report (`30_sNMF_cross_entropy.tsv`,
`30_sNMF_Q_K<k>.tsv`, and figures) contains a complete Q matrix and pair of
figures for **every** requested K, not only the one shown here.

![sNMF ancestry proportions at K=6 from the quickstart example, samples grouped by population](figures/14_sNMF_Q_K6.png)

![sNMF ancestry proportions at K=6 from the quickstart example, samples ordered by dominant inferred cluster instead of population label](figures/14_sNMF_Q_data_driven_K6.png)

Comparing the population-organized and data-driven figures within each
backend above is the standard check described earlier in this section:
structure that persists regardless of sample ordering is more trustworthy
than structure only visible when samples are pre-sorted by their metadata
population label.

## AMOVA

Report the hierarchy, distance definition, permutations, missing-data handling,
and variance components. Interpret negative components transparently rather
than silently truncating them.

## Mantel and isolation by distance

Report geographic and genetic distance definitions, transformations,
permutations, complete sample pairs, and sampling design. A Mantel association
does not establish a causal spatial process and can be sensitive to
autocorrelation and clustered sampling.

The quickstart metadata includes real, documented population-level collection
coordinates (`inst/extdata/quickstart/README.md` has the exact source and the
GBR/ITU/STU shared-point caveat), so this analysis actually runs rather than
being skipped for lack of coordinates.

![Isolation by distance: genetic distance (IBS) against geographic distance, with a log-distance fit, from the quickstart example](figures/12_isolation_by_distance.png)

On the quickstart example, the Mantel test (Pearson, 999 permutations) gives
r = 0.3129, p = 0.001 (999-permutation floor), with a positive slope (0.00275,
R² = 0.137) across all 12,720 complete sample pairs -- genetic distance really
does increase with geographic distance across these 8 continentally diverse
populations, a real, biologically sensible isolation-by-distance signal. This
does not, by itself, establish a causal spatial process: the same 8
populations are also the units compared by FST and PCA above, so this is not
independent evidence from a fourth, unrelated experiment.

## Figures and reports

Captions should identify the dataset, filters, estimator, sample size, software
version, and uncertainty or validation evidence. Publication-ready styling is
not scientific approval.

## Further reading

- [Interpreting results vignette](https://duceppemo.github.io/popgenVCF/articles/interpreting-results.html)
- [Publication gallery](https://duceppemo.github.io/popgenVCF/articles/publication-gallery.html)
- [Scientific validation](https://github.com/duceppemo/popgenVCF/blob/main/docs/SCIENTIFIC_VALIDATION.md)
