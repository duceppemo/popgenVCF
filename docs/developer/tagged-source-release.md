# Tagged source-package release publication

The tagged source release workflow converts an exact Git revision into a checked R
source package and a complete set of scientific release evidence. It implements the
release-publication requirements tracked in issue #107.

## Triggers

The workflow runs in three modes:

- pull requests that change the release workflow or its supporting files run a
  non-publishing rehearsal;
- `workflow_dispatch` runs a rehearsal by default and may publish only when
  `publish` is explicitly enabled;
- a pushed `v*` tag runs the complete workflow and publishes the resulting files
  to the matching GitHub Release.

The accepted release identifier is always `v<Version>`, where `Version` is read
from `DESCRIPTION`. A mismatched manual identifier or Git tag fails before the
package is built.

## Release gates

Publication occurs only after all of the following succeed:

1. `R CMD build` creates the source package from the checked-out revision.
2. `R CMD check --as-cran` succeeds against that built tarball.
3. The checked tarball is installed.
4. The end-to-end scientific release integration workflow completes.
5. Session and installed-package manifests are recorded.
6. Every payload file is hashed into `release-manifest.json`.
7. The payload and manifest checksums are verified.
8. A deliberate modified-file test proves that tampering is rejected.

A failure at any gate prevents the GitHub Release upload step.

## Published layout

`release-assets/` contains deterministic filenames:

- `popgenVCF_<version>.tar.gz`;
- `popgenVCF-check-results.tar.gz`;
- `popgenVCF-scientific-release.tar.gz`;
- `scientific-release-integration-summary.json`;
- `scientific-release-determinism.tsv`;
- `scientific-validation.tsv`;
- `population-structure-validation.tsv`;
- `session-info.txt`;
- `r-package-manifest.tsv`;
- `release-manifest.json`;
- `release-SHA256SUMS.txt`.

All publishable payload files are listed with their byte size and SHA-256 digest
in `release-manifest.json`. The manifest is authenticated by
`release-SHA256SUMS.txt`. The checksum file is the terminal control record and
therefore does not recursively checksum itself.

## Manual rehearsal

Open **Actions → Tagged source-package release → Run workflow**. Leave `publish`
disabled. The release identifier may be omitted; the workflow derives
`v<DESCRIPTION version>`. The resulting workflow artifact has the exact same
asset layout as a tag build, but nothing is attached to a GitHub Release.

To exercise a prospective version, update `DESCRIPTION` in a branch and run the
workflow on that branch with its matching identifier.

## Publishing a version

Before tagging:

1. update `DESCRIPTION` and release notes;
2. merge all release changes into `main`;
3. verify that required checks on `main` are green;
4. create an annotated tag matching the package version exactly.

Example:

```bash
git switch main
git pull --ff-only
git tag -a v0.9.0 -m "popgenVCF 0.9.0"
git push origin v0.9.0
```

The workflow creates the GitHub Release when needed and uploads every file in
`release-assets/`. Re-running the same tag replaces assets using `--clobber`,
which supports recovery from an interrupted upload while keeping deterministic
asset names.

## Failure recovery

Do not move or recreate a published version tag after users may have consumed
it. For a workflow infrastructure failure that occurred before successful
publication, fix the workflow on `main`, delete the unpublished tag locally and
remotely, recreate it at the intended commit, and push it again.

For an already published release with incorrect scientific or software content,
increment the package version and publish a new release.

## Scientific boundary

The workflow preserves package checks, validation evidence, release identities,
serialization, and checksums. It does not independently certify biological
interpretation or replace external real-data validation and peer review.
