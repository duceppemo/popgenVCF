# Reproducible container images

Tagged popgenVCF releases publish OCI images to GitHub Container Registry (GHCR). Each image is built from the exact Git tag, validated during the image build, and tested again after publication by its immutable digest.

## Pull a release

Use an explicit version for routine reproducibility:

```bash
docker pull ghcr.io/duceppemo/popgenvcf:0.9.0
```

The floating `latest` tag points to the newest non-prerelease version:

```bash
docker pull ghcr.io/duceppemo/popgenvcf:latest
```

For archival or publication workflows, pin the digest recorded in the GitHub Release assets:

```bash
docker pull ghcr.io/duceppemo/popgenvcf@sha256:<digest>
```

A digest identifies the exact OCI manifest and cannot move to a different image.

## Run an analysis

```bash
docker run --rm \
  --user "$(id -u):$(id -g)" \
  -e HOME=/tmp \
  -v "$PWD:/data" \
  ghcr.io/duceppemo/popgenvcf:0.9.0 \
  --config /data/analysis.yml
```

Paths in the configuration must refer to container paths under `/data`.

## Release tags

A stable release publishes the following aliases:

- full version, for example `0.9.0`;
- major/minor version, for example `0.9`;
- major version, for example `0`;
- full commit tag, prefixed with `sha-`;
- `latest` for the newest non-prerelease release.

The version aliases are convenient, but only the digest is immutable.

## Release metadata

Each GitHub Release receives:

- `container-metadata.json`, containing the image name, release version, source commit, published tags, and canonical digest;
- `container-digest.txt`, a compact digest record for scripts and archival systems.

BuildKit also publishes an SBOM and provenance attestations with the OCI image. These can be inspected with OCI-aware tooling such as Docker Buildx, GitHub attestations, or `cosign`.

## Apptainer and Singularity

GHCR images can be consumed directly on HPC systems:

```bash
apptainer pull popgenvcf_0.9.0.sif docker://ghcr.io/duceppemo/popgenvcf:0.9.0
```

For a fully pinned image:

```bash
apptainer pull popgenvcf.sif docker://ghcr.io/duceppemo/popgenvcf@sha256:<digest>
```

## Verification performed by CI

Before publication, the image build runs both scientific validation suites. After the image is pushed, CI pulls the exact digest and verifies:

1. the installed package version matches `DESCRIPTION`;
2. the command-line entry point responds successfully;
3. core scientific validation passes;
4. population-structure validation passes.

Container publication is triggered from the GitHub `release.published` event, so the source-package release workflow must complete successfully before the OCI workflow begins.
