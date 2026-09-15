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

## Re-pinning to a new release

`meta.yaml`'s `version`/`sha256` needs re-pinning for every release after the
one it currently targets -- easy to forget, since it's an out-of-band step
`scripts/bump_release_version.R` cannot do inline (the new release's real
GitHub Release assets don't exist yet at bump time). Two safeguards:

1. **`scripts/bump_release_version.R` prints a loud reminder** on every
   single version bump (cut or dev-resume) whenever this recipe's pinned
   version has fallen behind the current release -- impossible to miss
   without reading the script's own output, which the established release
   process already requires reviewing for its self-check result.
2. **`scripts/update_bioconda_recipe.R X.Y.Z`** does the actual re-pin once
   that release's assets exist: fetches `release-SHA256SUMS.txt`,
   independently re-verifies by downloading and hashing the real tarball
   (never trusts the manifest alone), and rewrites only `meta.yaml`'s two
   `{% set %}` lines. It deliberately does **not** touch the bioconda-recipes
   fork or PR -- that stays a deliberate, reviewed action each time. After
   running it: review the diff, re-lint if `bioconda-utils` is available,
   commit this repository's copy, then push the update to the existing PR
   branch (`duceppemo/bioconda-recipes`, branch `add-r-popgenvcf`) if still
   open, or open a fresh `update r-popgenvcf` PR once it has merged.

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
