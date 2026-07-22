# Release archival readiness

Phase 0.9.30.4 prepares popgenVCF releases for independent verification and archival deposition. It does not assign or claim a DOI. A DOI, Zenodo record identifier, concept DOI, publication date, or release date may be added only after the corresponding deposition has been published and the identifier resolves to the deposited object.

## Release evidence chain

A tagged source-package release produces:

- the checked R source tarball;
- archived `R CMD check` results;
- deterministic scientific-release evidence;
- runtime and installed-package manifests;
- a standalone SPDX JSON SBOM generated from the exact source tarball;
- DOI-ready `.zenodo.json`, `CITATION.cff`, CodeMeta, and reproducibility metadata;
- `source-release-provenance.json`, binding the release/tag/commit/workflow identity to the source tarball, SBOM, and archival metadata;
- `release-manifest.json`, recording the byte size and SHA-256 digest of every payload file;
- `release-SHA256SUMS.txt`, authenticating every payload digest and the manifest digest.

The checksum file is the terminal control record and intentionally does not recursively hash itself. The cryptographic chain is:

```text
source, SBOM, metadata, validation evidence, provenance
                         ↓
               release-manifest.json
                         ↓
             release-SHA256SUMS.txt
```

OCI image evidence remains a separate distribution identity. The container workflow publishes the exact image digest, BuildKit SPDX SBOM attestation, and maximum SLSA provenance attestation for the same release tag and Git commit.

## Rehearse without publishing

Run **Actions → Tagged source-package release → Run workflow** with publication disabled. Download the resulting workflow artifact and verify its contents before creating a tag.

From the extracted `release-assets` directory:

```bash
sha256sum --check release-SHA256SUMS.txt
```

The checksum file covers the release manifest. The manifest in turn requires an exact payload inventory and rejects missing, unexpected, resized, or modified files through `scripts/build_release_manifest.R`.

Inspect key JSON documents:

```bash
python -m json.tool release-manifest.json >/dev/null
python -m json.tool source-release-provenance.json >/dev/null
python -m json.tool popgenVCF-source-sbom.spdx.json >/dev/null
python -m json.tool archive-metadata/.zenodo.json >/dev/null
```

The source SBOM must identify an SPDX document and should be retained even when an OCI image is also published; the two SBOMs describe different artifacts.

## Verify OCI attestations

The container workflow builds with BuildKit SBOM generation and maximum provenance enabled. After publication, use the immutable digest from `container-digest.txt` rather than a movable tag.

```bash
image='ghcr.io/duceppemo/popgenvcf@sha256:<digest>'
docker buildx imagetools inspect "$image" --format '{{ json .SBOM.SPDX }}' \
  > container-sbom.spdx.json
docker buildx imagetools inspect "$image" --format '{{ json .Provenance.SLSA }}' \
  > container-provenance.slsa.json
```

Verify that both documents are valid JSON and retain them with `container-metadata.json`, `container-digest.txt`, the source-release assets, and institutional release records.

## Zenodo deposition sequence

Zenodo's GitHub integration reads `.zenodo.json` when present and uses it instead of `CITATION.cff` for GitHub release metadata. Consequently, `.zenodo.json` is a complete, validated software record rather than a partial override.

Before deposition:

1. keep `.zenodo.json`, the canonical software identity, CFF, CodeMeta, and `DESCRIPTION` synchronized;
2. keep DOI, concept DOI, record IDs, publication date, and release date absent while the package is in development;
3. enable the repository in the project owner's Zenodo GitHub integration;
4. complete the release rehearsal and scientific approval gates;
5. merge the exact release candidate into `main` and create the matching annotated version tag;
6. allow the tagged GitHub Release and container workflows to finish successfully;
7. confirm that all GitHub Release assets and checksums are present;
8. confirm the Zenodo draft metadata, creators, license, files, and version before publication;
9. publish the Zenodo record only after the deposited files match the approved release assets.

After Zenodo publishes the record, create a separate metadata reconciliation change that records the real DOI, concept DOI where applicable, record URL, and publication date across the canonical identity, `.zenodo.json`, `CITATION.cff`, CodeMeta, installed citation, README badge or citation section, and release notes. Never insert a reserved, anticipated, placeholder, or non-resolving DOI.

## Institutional archive

An institutional archive should retain, at minimum:

- the complete GitHub Release asset set;
- the terminal SHA-256 file and release manifest;
- the source and OCI SBOMs;
- source and OCI provenance records;
- exact Git tag and commit;
- container digest and SIF checksum when Apptainer is used;
- canonical scientific release certificate and approval evidence;
- Zenodo DOI and record metadata after publication;
- access, license, retention, and confidentiality policies applicable to any analysis data.

Archive software evidence separately from restricted genomic inputs when licensing, privacy, or institutional policy requires it. The existence of checksums, SBOMs, or a DOI establishes artifact identity and traceability; it does not establish biological correctness or scientific approval.
