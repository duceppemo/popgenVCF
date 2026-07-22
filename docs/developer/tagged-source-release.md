# Tagged source-package release publication

The tagged source release workflow converts an exact Git revision into a checked R source package and a complete set of scientific, archival, and supply-chain evidence. It implements the release-publication requirements tracked through issues #1 and #297.

## Triggers

The workflow runs in three modes:

- pull requests that change the release workflow or its supporting files run a non-publishing rehearsal;
- `workflow_dispatch` runs a rehearsal by default and may publish only when `publish` is explicitly enabled;
- a pushed `v*` tag runs the complete workflow and publishes the resulting files to the matching GitHub Release.

The accepted release identifier is always `v<Version>`, where `Version` is read from `DESCRIPTION`. A mismatched manual identifier or Git tag fails before the package is built.

## Release gates

Publication occurs only after all of the following succeed:

1. `R CMD build` creates the source package from the checked-out revision.
2. DOI-ready `.zenodo.json`, CFF, CodeMeta, and reproducibility metadata are collected without adding an unpublished DOI or date.
3. a pinned Syft action creates an SPDX JSON SBOM from the exact source tarball and its document structure is validated.
4. `R CMD check --as-cran` succeeds against that built tarball.
5. the checked tarball is installed.
6. the end-to-end scientific release integration workflow completes.
7. session and installed-package manifests are recorded.
8. `source-release-provenance.json` binds the source archive, SBOM, metadata, tag, commit, and workflow identity.
9. every payload file is hashed into `release-manifest.json`.
10. the payload and manifest checksums are verified.
11. a deliberate modified-file test proves that tampering is rejected.

A failure at any gate prevents the GitHub Release upload step.

## Published layout

`release-assets/` contains deterministic filenames and the archival metadata directory:

- `popgenVCF_<version>.tar.gz`;
- `popgenVCF-check-results.tar.gz`;
- `popgenVCF-scientific-release.tar.gz`;
- `popgenVCF-source-sbom.spdx.json`;
- `source-release-provenance.json`;
- `archive-metadata/.zenodo.json`;
- `archive-metadata/CITATION.cff`;
- `archive-metadata/codemeta.json`;
- `archive-metadata/reproducibility.md`;
- `scientific-release-integration-summary.json`;
- `scientific-release-determinism.tsv`;
- `scientific-validation.tsv`;
- `population-structure-validation.tsv`;
- `session-info.txt`;
- `r-package-manifest.tsv`;
- `release-manifest.json`;
- `release-SHA256SUMS.txt`.

All payload files are listed with their byte size and SHA-256 digest in `release-manifest.json`. The manifest is authenticated by `release-SHA256SUMS.txt`. The checksum file is the terminal control record and therefore does not recursively checksum itself.

## Manual rehearsal

Open **Actions → Tagged source-package release → Run workflow**. Leave `publish` disabled. The release identifier may be omitted; the workflow derives `v<DESCRIPTION version>`. The resulting workflow artifact has the exact same asset layout as a tag build, but nothing is attached to a GitHub Release or deposited to Zenodo.

Verify the extracted artifact:

```bash
cd release-assets
sha256sum --check release-SHA256SUMS.txt
python -m json.tool release-manifest.json >/dev/null
python -m json.tool source-release-provenance.json >/dev/null
python -m json.tool popgenVCF-source-sbom.spdx.json >/dev/null
```

To exercise a prospective version, update `DESCRIPTION` and all canonical metadata in a branch, then run the workflow on that branch with its matching identifier.

## Publishing a version

Before tagging:

1. complete production scientific validation and approval;
2. update `DESCRIPTION`, canonical software metadata, and release notes;
3. merge all release changes into `main`;
4. verify that required checks on `main` are green;
5. perform and review a non-publishing release rehearsal;
6. create an annotated tag matching the package version exactly.

Example:

```bash
git switch main
git pull --ff-only
git tag -a v0.10.0 -m "popgenVCF 0.10.0"
git push origin v0.10.0
```

The workflow creates the GitHub Release when needed and recursively uploads every file in `release-assets/`. Re-running the same unpublished tag replaces assets using `--clobber`, which supports recovery from an interrupted upload while keeping deterministic asset names.

The container workflow then publishes the image by immutable digest with BuildKit SBOM and provenance attestations. Zenodo deposition and DOI finalization follow the reviewed process in [release archival readiness](release-archival-readiness.md).

## Failure recovery

Do not move or recreate a published version tag after users may have consumed it. For a workflow infrastructure failure that occurred before successful publication, fix the workflow on `main`, delete the unpublished tag locally and remotely, recreate it at the intended commit, and push it again.

For an already published release with incorrect scientific or software content, increment the package version and publish a new release. Do not overwrite a Zenodo record or reuse a DOI to disguise changed release content.

## Scientific boundary

The workflow preserves package checks, validation evidence, release identities, SBOMs, provenance, serialization, and checksums. It does not independently certify biological interpretation, approve production real-data evidence, or replace external validation and peer review.
