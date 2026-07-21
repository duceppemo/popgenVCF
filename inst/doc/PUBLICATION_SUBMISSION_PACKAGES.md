# Deterministic submission packages and supplementary indexes

Phase 0.9.4 assembles existing publication contracts into journal-ready submission manifests without changing manuscript content, scientific values, rendered outputs, or source artifacts.

## Package specification

`new_publication_submission_package_spec()` binds a stable package identity to a validated journal profile, publication layout, figure style, and report specification. It records the archive format, root directory, required logical roles, version, and a deterministic SHA-256 fingerprint.

The contract currently supports `zip` and `tar.gz` archive layouts. Archive creation remains downstream of the canonical manifest: tools must preserve the manifest ordering, paths, checksums, and metadata.

## File manifest

Each package entry records:

- deterministic package path;
- logical role;
- media type;
- byte size;
- SHA-256 digest;
- source-artifact fingerprint.

Entries are normalized by path, and duplicate paths fail closed. The package manifest must contain every role required by its specification.

## Supplementary index

`new_publication_supplementary_index()` adds stable labels, titles, and manuscript references to supplementary file entries. Labels and paths must be unique. Every indexed supplementary path must also exist in the package manifest, preventing orphaned supplementary records.

## Binding and drift detection

Package specifications bind to journal, layout, figure-style, and report fingerprints. Package manifests bind to the specification, report execution, output manifest, and supplementary index. Mutation of any canonical record invalidates its fingerprint and validation fails closed.

## Architectural boundary

Submission packaging composes existing manuscript, renderer, artifact, and provenance contracts. It does not alter scientific results, regenerate figures, rewrite manuscripts, or introduce a competing artifact registry.
