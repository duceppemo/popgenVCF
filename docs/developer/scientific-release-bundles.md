# Scientific release bundles

Phase 7.3.16 closes the publication and reproducibility phase with a single immutable root record for a complete popgenVCF scientific release.

## Contract

`new_scientific_release_bundle()` records explicit software, Git, release-date, R-platform, dependency, artifact, and scientific-component identities. The required digest chain covers:

- analysis registry;
- provenance DAG;
- artifact lineage;
- FAIR bundle;
- manuscript;
- regeneration plan;
- regeneration execution;
- regeneration verification;
- benchmark record;
- scientific validation record.

The constructor stores identities rather than embedding the linked objects. This keeps the release record compact while making every component independently verifiable and replaceable only through creation of a new release identity.

## Determinism

Callers must supply the release date and source-control identities explicitly. Dependency rows, artifact rows, and digest-chain components are canonicalized before the release SHA256 is computed. Runtime timestamps and temporary paths are excluded from the identity.

## Integrity

Artifact entries contain normalized release-relative paths, byte sizes, and SHA256 identities. Duplicate paths, duplicate artifact hashes, incomplete digest chains, malformed hashes, unsafe paths, and modified release objects are rejected.

## Output

`write_scientific_release_bundle()` writes:

- `scientific-release.json`;
- `scientific-release.md`;
- `scientific-release.tsv`;
- `scientific-release-manifest.sha256`.

Overwrite protection is enabled by default. Directory validation verifies every checksum in the release manifest.

## Scientific boundary

A scientific release bundle binds immutable evidence and validation identities. It does not independently certify biological interpretation, infer missing metadata, modify linked artifacts, or replace the scientific validation records it references.
