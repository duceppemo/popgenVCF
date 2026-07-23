# Installing and configuring ancestry backends

popgenVCF provides one ancestry workflow over ADMIXTURE, fastStructure, and LEA/sNMF. Install and test each enabled backend before running an analysis configuration.

Installation success establishes runtime availability only. The 0.10.0 release gate additionally requires one approved real-data case executed through all three backends, with commands, versions, inputs, sample order, Q matrices, K-selection evidence, alignment diagnostics, tolerances, logs, and scientific approval retained in the release-candidate dossier.

## Shared input requirements

ADMIXTURE and fastStructure use a PLINK binary prefix containing matching `.bed`, `.bim`, and `.fam` files. LEA/sNMF uses a LEA `.geno` file.

Every enabled backend also requires a sample-order file containing identifiers in exactly the row order used by its Q matrix. The file prevents silently assigning ancestry coefficients to the wrong samples.

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
    plink_prefix: /data/cohort
    q_sample_file: /data/cohort.samples.txt
    k: "2:10"
    threads: 4
    cv_folds: 5
```

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
    plink_prefix: /data/cohort
    q_sample_file: /data/cohort.samples.txt
    k: "2:10"
```

Absolute executable paths remain supported for custom or manually managed installations. Retain the Conda package manifest, executable paths, commands, and logs as release evidence.

Official Bioconda recipe:

<https://bioconda.github.io/recipes/faststructure/README.html>

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
    geno_file: /data/cohort.geno
    q_sample_file: /data/geno_sample_order.txt
    k: "2:10"
    repetitions: 5
    entropy: true
```

Record the R version, Bioconductor version, LEA version, package-library manifest, seeds, repetitions, and entropy setting.

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
