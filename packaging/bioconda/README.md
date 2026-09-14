# bioconda recipe

`r-popgenvcf/meta.yaml` and `r-popgenvcf/build.sh` are the maintained source
of the recipe submitted to [bioconda](https://bioconda.github.io/), so `conda
install r-popgenvcf` / `mamba install r-popgenvcf` becomes possible. They are
not part of the R package build (excluded via `.Rbuildignore`); the copy
actually built by bioconda's CI lives in the PR below, under
[bioconda/bioconda-recipes](https://github.com/bioconda/bioconda-recipes)'s
own `recipes/r-popgenvcf/`, not in this repository.

## Status

**Submitted**: [bioconda/bioconda-recipes#69238](https://github.com/bioconda/bioconda-recipes/pull/69238)
(2026-09-14), targeting `v1.0.13` (the first release built from the shrunk
source tarball, commit `5110c76`: 64.7 MB -> 22.0 MB). Linted clean with the
real `bioconda-utils lint` (bioconda-utils 2.14.0, "All checks OK") before
submission -- not just structurally checked. Review and merge timing is up to
external bioconda maintainers, not this repository.

Once merged and built, the real install command is `mamba install
r-popgenvcf` (bioconda's naming convention for a non-CRAN/Bioconductor R
package), not literally `mamba install popgenVCF`; the recipe folder itself
must also be named `r-popgenvcf` to match (`bioconda-utils lint`'s
`folder_and_package_name_must_match` check caught this before submission --
the original `popgenvcf/` folder name here was renamed to match).

## Every dependency already has a proven working path

`inst/conda/environment.yml` at the repo root is not a draft -- it is the
real dependency set used to build the Conda-based Apptainer/container image,
so every `r-*`/`bioconductor-*` package this recipe lists is already known to
resolve and install correctly through the same `bioconda`/`conda-forge`
channels.

## If a new release needs re-pinning

`meta.yaml`'s `version`/`sha256` will need re-pinning if bioconda review asks
for an update to a newer release before merging, or for any future version
bump after this one lands. Get the sha256 from that release's own
`release-SHA256SUMS.txt` asset (and independently re-verify by hashing a
freshly downloaded copy of the actual tarball, not just trusting the
manifest), then push the update to the same PR branch
(`duceppemo/bioconda-recipes`, branch `add-r-popgenvcf`) if the PR is still
open, or open a new `update r-popgenvcf` PR once it has merged.

## Deliberately lighter than the container image

Unlike `inst/conda/environment.yml` (which reproduces the full container
environment, including ADMIXTURE and fastStructure), this recipe's `run:`
dependencies omit both -- they are genuinely optional ancestry backends, each
a heavyweight compiled tool in its own right, only invoked when explicitly
configured. A user who needs them installs them separately
(`mamba install admixture faststructure`) the same way the container image
documents. BCFtools, HTSlib, and Pandoc stay as required `run` dependencies:
DESCRIPTION's own `SystemRequirements` lists the first two as required (VCF
sorting, BGZF compression, indexing, ROH detection), and Pandoc is exercised
by every report-rendering code path, not a true optional extra.

## `run_exports`

Pins the major version only (`max_pin="x"`), matching bioconda's own
guidance for a package on real semantic versioning (not a 0.x.x series):
popgenVCF's public-API-contract check (`inst/api-contract/`) re-verifies its
exported R API on every release, patch included, with zero drift ever
recorded -- API/ABI/CLI breakage is reserved for major-version bumps.
