# Installing and configuring ancestry backends

popgenVCF provides one ancestry workflow over ADMIXTURE, fastStructure, and LEA/sNMF, but each backend has a different runtime. Install and test each backend before enabling it in an analysis configuration.

Installation success establishes runtime availability only. The 0.10.0 release gate additionally requires one approved real-data case executed through all three backends, with commands, versions, inputs, sample order, Q matrices, K-selection evidence, alignment diagnostics, tolerances, logs, and scientific approval retained in the release-candidate dossier.

## Shared input requirements

ADMIXTURE and fastStructure use a PLINK binary prefix containing matching `.bed`, `.bim`, and `.fam` files. LEA/sNMF uses a LEA `.geno` file.

Every enabled backend also requires a `q_sample_file` containing sample identifiers in exactly the row order used by its Q matrix. The file prevents silently assigning ancestry coefficients to the wrong samples.

Do not reuse a sample-order file unless its checksum and ordering were verified against the exact backend input.

## ADMIXTURE

The main Conda environment already declares the Bioconda `admixture` package:

```bash
mamba env create --file inst/conda/environment.yml
conda activate popgenvcf
command -v admixture
admixture 2>&1 | head
```

The upstream ADMIXTURE project currently publishes Linux x86-64 binaries and its manual from the official download page:

<https://dalexander.github.io/admixture/download.html>

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

fastStructure is isolated from the main R environment because its Python and compiled-library constraints can conflict with the current R stack.

Create the supported build environment:

```bash
mamba env create --file inst/conda/faststructure-environment.yml
conda activate popgenvcf-faststructure
```

Install the maintained Python 3 port using the project installer:

```bash
bash inst/scripts/install-faststructure.sh \
  "$HOME/.local/opt/fastStructure3"
```

The installer prints the resulting executable paths. Confirm them:

```bash
python "$HOME/.local/opt/fastStructure3/structure.py" --help
python "$HOME/.local/opt/fastStructure3/chooseK.py" --help
git -C "$HOME/.local/opt/fastStructure3" rev-parse HEAD
```

Bioconda also publishes a `faststructure` recipe and container images:

<https://bioconda.github.io/recipes/faststructure/README.html>

Use the project’s isolated environment and installer for the validated popgenVCF path unless a different runtime is explicitly recorded and tested.

Configure absolute paths:

```yaml
analyses:
  faststructure:
    enabled: true
    structure_executable: /home/user/.local/opt/fastStructure3/structure.py
    choosek_executable: /home/user/.local/opt/fastStructure3/chooseK.py
    plink_prefix: /data/cohort
    q_sample_file: /data/cohort.samples.txt
    k: "2:10"
```

Retain the Git commit, Python version, environment export, commands, and logs.

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
    q_sample_file: /data/cohort.samples.txt
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
