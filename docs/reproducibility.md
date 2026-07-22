# Reproducibility statement

popgenVCF is designed to make computational population-genomic analyses inspectable, repeatable, and archivable. Reproducibility depends on preserving the exact software revision, runtime environment, inputs, configuration, random seeds, execution resources, external tools, and generated validation evidence.

## What the project preserves

The software provides contracts for a checked R source package, SHA-256 checksums, machine-readable release manifests, scientific-validation records, session information, installed-package manifests, immutable project bundles, artifact lineage, and FAIR research-object metadata. The release workflow verifies that the Git tag and `DESCRIPTION` version agree before publication.

A source-release rehearsal generates an SPDX JSON software bill of materials from the exact built source tarball and a deterministic provenance record linking the source archive, SBOM, archival metadata, release tag, Git commit, and workflow identity. The release manifest hashes every payload file, including the provenance record, and the terminal checksum file authenticates the manifest.

Versioned OCI images are built from the exact source revision and can be identified by semantic-version, commit-SHA, and immutable digest references. BuildKit generates separate SPDX SBOM and maximum SLSA provenance attestations attached to the published OCI image. Native Apptainer builds and OCI-to-SIF conversion support HPC environments; a SIF remains a separate artifact and requires its own checksum.

The repository retains human-editable environment specifications, DOI-ready but unpublished Zenodo metadata, and validation workflows. These software contracts make required evidence representable and testable; they do not assert that the production 0.10.0 release evidence has already been executed, approved, published, or deposited.

## Minimum reproducibility record

A result should not be treated as independently reproducible unless its record identifies at least:

- the popgenVCF package version or Git commit;
- the source-package checksum when a source archive was installed;
- the source-package SBOM and source-release provenance record for an archived release;
- the immutable container digest, image SBOM, and build provenance when a container was used;
- the complete configuration file and checksum;
- all input-file checksums, including VCF, metadata, and external reference inputs;
- the resolved sample identity table and any aliases or exclusions;
- every random seed and stochastic replicate identifier;
- the thread count and other material resource settings;
- external-tool versions and commands, including relevant environment variables;
- the R, operating-system, package, compiler, and numerical-library environment manifest;
- the analysis execution ledger, artifact manifest, and validation evidence;
- the canonical dataset, tolerance, approval, and release-certificate identifiers when a result contributes to release gating.

The portable `.popgenvcf` project, publication companion, FAIR bundle, release archive, or equivalent institutional archive should retain these records together rather than relying on a narrative methods section alone.

## Determinism and numerical validation

The scientific validation suites exercise representative core and population-structure calculations and fail when expected numerical or structural invariants are violated. Release integration checks repeat selected workflows and compare deterministic evidence.

Exact byte-for-byte identity is expected only for artifacts explicitly covered by deterministic release checks. Floating-point results can vary slightly across processor architectures, numerical libraries, external tools, or thread schedules. Such variation must remain within the tolerances encoded by the relevant validation contract and must not be silently reclassified as equivalent.

## Release-evidence boundary

Computational reproducibility infrastructure is distinct from approved scientific release evidence. A schema, validator, benchmark budget, concordance contract, release-certificate class, SBOM, provenance record, checksum manifest, or DOI-ready metadata file demonstrates that evidence can be represented and checked; it does not demonstrate that the canonical real dataset was processed successfully or that a scientist approved the resulting evidence.

The 0.10.0 release may be described as release-ready only after the exact tagged commit has complete checksum-linked canonical validation, quantitative baseline, external-tool concordance, performance history, source-package, container, Apptainer, archive, and approval records. Development metadata intentionally omits a release date, DOI, concept DOI, and archive record identifier until the corresponding deposited object has been published and resolves publicly.

## Input and provenance responsibilities

Users remain responsible for preserving original VCF and metadata files, recording preprocessing performed outside popgenVCF, and ensuring that sample identifiers, population assignments, geographic coordinates, filtering thresholds, and external-program inputs are scientifically appropriate.

For archival work, use immutable identifiers whenever possible:

- a Git release tag and commit SHA;
- the source-package checksum and SPDX SBOM;
- the source-release provenance and release manifest;
- the OCI image digest rather than only a floating tag;
- the OCI SBOM and provenance attestations;
- a checksum for any derived Apptainer SIF;
- input-file checksums;
- the complete analysis configuration and generated run records;
- persistent archive or DOI identifiers only after the deposited object exists.

## Scientific boundary

Computational reproducibility does not establish biological correctness. Automated checks detect software regressions, metadata drift, packaging failures, environment mismatches, and selected numerical inconsistencies. They do not independently validate sampling design, taxonomic identification, population definitions, marker ascertainment, model assumptions, causal interpretation, or suitability of a method for a particular dataset.

Results therefore still require domain-expert review, appropriate controls, sensitivity analyses, and—where applicable—independent replication and peer review.

## Reporting a reproducibility problem

Open a GitHub issue containing the popgenVCF version or commit, source and container artifact identities, container digest if used, platform, configuration, input characteristics that can be shared, exact command, external-tool versions, and relevant logs or validation records. Do not upload confidential or restricted biological data to a public issue.
