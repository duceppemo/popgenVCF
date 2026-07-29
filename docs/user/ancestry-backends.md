# Installing and configuring ancestry backends

popgenVCF provides one ancestry workflow over ADMIXTURE, fastStructure, and LEA/sNMF. Install and test each enabled backend before running an analysis configuration.

Installation success establishes runtime availability only. The 0.10.0 release gate additionally requires one approved real-data case executed through all three backends, with commands, versions, inputs, sample order, Q matrices, K-selection evidence, alignment diagnostics, tolerances, logs, and scientific approval retained in the release-candidate dossier.

## Shared input requirements

ADMIXTURE and fastStructure use a PLINK binary prefix containing matching `.bed`, `.bim`, and `.fam` files. LEA/sNMF uses a LEA `.geno` file. By default, popgenVCF generates both formats from the retained samples and LD-pruned SNPs and caches them under the analysis output.

Every enabled backend records sample identifiers in exactly the row order used by its Q matrix. popgenVCF derives this order from the selected PLINK `.fam` file or from the GDS extraction used to create the sNMF input. Explicit sample order prevents silently assigning ancestry coefficients to the wrong samples.

Do not reuse a sample-order file unless its checksum and ordering were verified against the exact backend input.

## Main Conda environment

ADMIXTURE and fastStructure are both installed by the main popgenVCF environment:

```bash
mamba env create --file inst/conda/environment.yml
conda activate popgenvcf

command -v admixture
command -v structure.py
command -v chooseK.py
```

To update an existing environment after either dependency was added:

```bash
mamba env update --file inst/conda/environment.yml --prune
```

The current Bioconda fastStructure recipe is a Python 3 build and installs the `structure.py`, `chooseK.py`, and `distruct.py` entry points directly into the activated environment.

## ADMIXTURE

A minimal configuration is:

```yaml
analyses:
  admixture:
    enabled: true
    executable: admixture
    k: "2:10"
    threads: auto
    cv_folds: 5
```

`threads: auto` inherits `compute.threads`. A positive integer remains supported
when ADMIXTURE should use only part of the global CPU budget.

When using a manually installed binary, set `executable` to its absolute path. Record:

```bash
command -v admixture
sha256sum "$(command -v admixture)"
admixture 2>&1 | head
```

Retain the binary checksum when the executable is not supplied by a checksum-locked environment.

## fastStructure

Install or update the Bioconda package in the active popgenVCF environment:

```bash
conda activate popgenvcf
mamba install bioconda::faststructure

structure.py 2>&1 | head
chooseK.py 2>&1 | head
```

The default configuration uses those commands directly:

```yaml
analyses:
  faststructure:
    enabled: true
    structure_executable: structure.py
    choosek_executable: chooseK.py
    k: "2:10"
```

Absolute executable paths remain supported for custom or manually managed installations. Retain the Conda package manifest, executable paths, commands, and logs as release evidence.

To prefer an existing PLINK bundle for either ADMIXTURE or fastStructure, set
`plink_prefix` to the common path before `.bed`, `.bim`, and `.fam`. The bundle
is used only when its sample order and variant count match the retained
analysis data; otherwise popgenVCF logs a warning and generates the canonical
cached bundle.

Official Bioconda recipe:

<https://bioconda.github.io/recipes/faststructure/README.html>

popgenVCF reads fastStructure's Q matrices directly and creates membership
figures with its shared R plotting code. It does not invoke `distruct.py`.
ADMIXTURE, fastStructure, and sNMF membership figures label every bar with the
sample's public display name. Labels remain attached to the correct Q-matrix
row when samples are ordered for display. A configured metadata alias is used
when available; otherwise the immutable VCF sample ID is shown. DAPC
posterior-membership figures use the same labeling convention. DAPC
discriminant-coordinate figures and the other population-colored figures use
the same high-contrast, color-blind-friendly qualitative palette so a
population keeps a stable color across outputs.

For every K, each backend now produces two views of the same fitted membership
matrix. The standard figure organizes and facets samples by the metadata
`population` column. The additional `_data_driven_` figure does not use
population metadata: it groups samples by their dominant inferred cluster and
orders them within each cluster by membership strength. The population column
does not constrain model fitting in ADMIXTURE, fastStructure, sNMF, or DAPC
`find.clusters`; it is used only to organize the standard figure and calculate
descriptive agreement diagnostics. The two figures are alternate presentations
of one fit, not independent analysis replicates. Vertical lines in the
data-driven bar graph delimit adjacent dominant-cluster groups.

## Selecting the number of clusters

Cluster-number selection reuses diagnostics already produced by each backend;
it does not launch another round of model fitting. For each available numeric
criterion, popgenVCF records the raw optimum, an elbow estimate, and either a
one-standard-error choice or the simplest model on a near-optimum plateau.
DAPC combines existing cross-validation success with Calinski-Harabasz and
Davies-Bouldin scores calculated from its shared principal-component scores;
an available Bayesian information criterion is also retained.
Variation in replicate membership root mean squared error contributes an
additional DAPC stability vote; constant diagnostics are ignored.
ADMIXTURE uses cross-validation error, sNMF uses
replicate cross-entropy and its standard error, and fastStructure retains both
native recommendations reported by `chooseK.py`.

Each method casts one transparent vote. The final recommendation is the
majority choice; ties are resolved first by mean normalized support across the
available numeric diagnostics and then in favor of the simpler model. Method
votes, normalized score curves, vote counts, the tie-breaking rule, and the
final consensus are written to tab-separated tables. A faceted figure shows
the diagnostic support panels beside the method-vote panel, with the consensus
number marked in every panel.

The consensus is a reproducible summary of the available diagnostics, not
proof that the recommended number is the true number of biological
populations. Inspect neighboring values, membership stability, and biological
context before interpreting the selected model.

## LEA/sNMF

The project Bioconductor installer installs LEA by default:

```bash
conda activate popgenvcf
Rscript inst/scripts/install-bioconductor.R
Rscript -e 'stopifnot(requireNamespace("LEA", quietly = TRUE)); print(packageVersion("LEA"))'
```

To omit LEA from a minimal installation, set `POPGENVCF_INSTALL_LEA=false`. For ancestry validation it must remain installed.

Bioconductor’s supported installation method is:

```r
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}
BiocManager::install("LEA")
```

Official package page:

<https://bioconductor.org/packages/LEA/>

Configure sNMF:

```yaml
analyses:
  snmf:
    enabled: true
    k: "2:10"
    repetitions: 5
    entropy: true
    threads: auto
```

LEA receives the resolved thread count through its native `CPU` argument and can
run independent K/repetition jobs concurrently. `threads: auto` inherits
`compute.threads`; set a smaller positive integer to reserve resources for other
work. The explicit analysis seed is also forwarded to LEA.

To prefer an existing sNMF input, configure both `geno_file` and
`q_sample_file`. The `.geno` dimensions and sample order must match the
retained analysis data. If either file is missing or incompatible, popgenVCF
logs a warning and generates the canonical cached `.geno` and sample-order
files instead.

Record the R version, Bioconductor version, LEA version, package-library manifest, seeds, threads, repetitions, and entropy setting.

## Parallel execution boundaries

DAPC computes its genotype PCA once and reuses it for clustering and the final
discriminant fit at every K. On systems that support forked workers, independent
K values run concurrently with at most `min(compute.threads, number of K values)`
workers. Each K retains the configured deterministic replicate-seed sequence,
and systems without fork support fall back to sequential execution.

sNMF uses native parallelism for K/repetition jobs. ADMIXTURE supports multiple
threads within each K. fastStructure K jobs remain sequential for now because
concurrent processes first require isolated working directories and a final
barrier before `chooseK.py` runs.

Do not multiply independent worker counts by backend thread counts beyond
`compute.threads`. Concurrent genotype analyses also duplicate working memory,
so CPU count alone is not a safe worker limit. `compute.memory_mb` records the
available memory budget in the configuration and provenance for current and
future worker-budget decisions.

## Backend discovery in R

Inspect the default registry:

```r
registry <- popgenVCF::default_ancestry_backend_registry()
popgenVCF::ancestry_backend_status(registry)
```

A backend reported as unavailable must remain unavailable or skipped. Do not replace a missing executable with precomputed output unless the evidence record explicitly identifies the source command, version, checksum, sample order, and review role.

## Cross-backend release evidence

The approved three-backend case must use:

- the same checksum-pinned biological dataset;
- the same immutable sample identities;
- documented backend-specific input conversions;
- deterministic seed and replicate schedules where supported;
- a declared K range;
- retained raw Q matrices and fit statistics;
- label-alignment and consensus evidence;
- backend-version and command records;
- scientifically justified tolerances;
- named review and approval.

Agreement is evidence about numerical and structural consistency. It is not proof that the inferred K or ancestry components are biologically correct.
