# Phase 10.2.3 release conformance

Phase 10.2.3 composes the canonical public API descriptor, compatibility record, migration plan, evolution policy, and release-channel identities into one deterministic release-conformance manifest.

## Required release channels

Every manifest covers exactly one identity for each authoritative channel:

- package;
- container;
- Apptainer;
- documentation;
- scientific validation.

All channels must agree on the package release version and canonical public API descriptor fingerprint. Artifact digests remain channel-specific and are included in the manifest fingerprint.

## Release gating

A manifest is release-ready only when:

1. descriptor, compatibility, migration, and policy evidence validate;
2. all evidence fingerprints match exactly;
3. every required channel is present exactly once;
4. all channels agree on release and API identity;
5. breaking compatibility drift has explicit approval;
6. the manifest fingerprint verifies after normalization.

`assert_phase10_release_conformance()` fails closed when any blocker remains.

## Architectural boundary

The conformance layer records and validates release evidence. It does not build packages or containers, execute scientific validation, replace the existing release workflows, or introduce another API or schema registry.
