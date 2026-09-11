# bioconda recipe (draft)

`popgenvcf/meta.yaml` and `popgenvcf/build.sh` are a maintained draft for
submitting popgenVCF to [bioconda](https://bioconda.github.io/), so `conda
install r-popgenvcf` / `mamba install r-popgenvcf` becomes possible. They are
not part of the R package build (excluded via `.Rbuildignore`) and are not
picked up by conda-build until copied into an actual PR against
[bioconda/bioconda-recipes](https://github.com/bioconda/bioconda-recipes).

## Status

Drafted, not yet submitted, and not yet linted or built against real
bioconda-utils tooling (not installed in this environment -- `bioconda-utils
lint`/`build` requires the actual bioconda-recipes CI image). Treat this as a
structurally-checked starting point, not a validated recipe: the YAML parses
correctly after Jinja rendering and every listed dependency was confirmed to
resolve on `bioconda`/`conda-forge` today (`mamba search`), but conda-build's
own linter, a real build, and the recipe's `test:` commands have not actually
been run.

## Every dependency already has a proven working path

`inst/conda/environment.yml` at the repo root is not a draft -- it is the
real dependency set used to build the Conda-based Apptainer/container image,
so every `r-*`/`bioconductor-*` package this recipe lists is already known to
resolve and install correctly through the same `bioconda`/`conda-forge`
channels.

## Before actually submitting

1. **Re-pin `version` and `sha256`** in `meta.yaml` to whichever release is
   actually being submitted -- ideally the first release cut after commit
   `5110c76` (which shrank the built source tarball from 64.7 MB to 22.0 MB;
   `v1.0.12` itself still carries the larger, pre-fix tarball). Get the
   sha256 from that release's own `release-SHA256SUMS.txt` asset, never by
   hashing a manually re-downloaded copy.
2. **Install `bioconda-utils`** (see bioconda's own contributor docs) and run
   its linter and a real local build against this recipe. Fix anything it
   flags before opening a PR -- this has not been done yet.
3. **Fork `bioconda/bioconda-recipes`**, copy `popgenvcf/` under its
   `recipes/` directory, and open a PR there. Review is by external bioconda
   maintainers on their own timeline, not something this repository
   controls. They may ask why the package isn't on CRAN/Bioconductor first --
   worth having an answer ready (this toolkit's hard runtime dependency on
   BCFtools/HTSlib as external system binaries is a real fit for bioconda,
   less so for CRAN's self-containment expectations).
4. Once merged and built, the real install command is `mamba install
   r-popgenvcf` (bioconda's naming convention for a non-CRAN/Bioconductor R
   package), not literally `mamba install popgenVCF`.

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
