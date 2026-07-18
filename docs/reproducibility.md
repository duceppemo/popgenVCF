# Reproducibility statement

popgenVCF is designed to make computational population-genomic analyses inspectable, repeatable, and archivable. Reproducibility depends on preserving the exact software revision, runtime environment, inputs, configuration, random seeds, and generated validation evidence.

## What the project preserves

Tagged releases provide a checked R source package, SHA-256 checksums, a machine-readable release manifest, scientific-validation records, session information, and an installed R-package manifest. The release workflow verifies that the Git tag and `DESCRIPTION` version agree before publication.

Versioned OCI images are built from the exact release tag and published to GHCR with semantic-version and commit-SHA tags. Each published image is verified again by its immutable digest. BuildKit attaches software-bill-of-materials and provenance attestations. Native Apptainer builds and OCI-to-SIF conversion are supported for HPC environments.

The repository retains human-editable environment specifications and validation workflows. Analyses should record the package version or Git commit, container digest where applicable, configuration file, input checksums, thread count, and random seed.

## Determinism and numerical validation

The scientific validation suites exercise representative core and population-structure calculations and fail when expected numerical or structural invariants are violated. Release integration checks repeat selected workflows and compare deterministic evidence.

Exact byte-for-byte identity is expected only for artifacts explicitly covered by deterministic release checks. Floating-point results can vary slightly across processor architectures, numerical libraries, external tools, or thread schedules. Such variation must remain within the tolerances encoded by the relevant validation test.

## Input and provenance responsibilities

Users remain responsible for preserving original VCF and metadata files, recording any preprocessing performed outside popgenVCF, and ensuring that sample identifiers, population assignments, geographic coordinates, filtering thresholds, and external-program inputs are scientifically appropriate.

For archival work, use immutable identifiers whenever possible:

- a Git release tag and commit SHA;
- the source-package checksum;
- the OCI image digest rather than only a floating tag;
- input-file checksums;
- the complete analysis configuration and generated run records.

## Scientific boundary

Computational reproducibility does not establish biological correctness. The automated checks detect software regressions, metadata drift, packaging failures, environment mismatches, and selected numerical inconsistencies. They do not independently validate sampling design, taxonomic identification, population definitions, marker ascertainment, model assumptions, causal interpretation, or suitability of a method for a particular dataset.

Results therefore still require domain-expert review, appropriate controls, sensitivity analyses, and—where applicable—independent replication and peer review.

## Reporting a reproducibility problem

Open a GitHub issue containing the popgenVCF version or commit, container digest if used, platform, configuration, input characteristics that can be shared, exact command, and relevant logs or validation records. Do not upload confidential or restricted biological data to a public issue.
