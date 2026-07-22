# Reproducibility statement

popgenVCF is designed to make computational population-genomic analyses inspectable, repeatable, and archivable. Reproducibility depends on preserving the exact software revision, runtime environment, inputs, configuration, random seeds, execution resources, external tools, and generated validation evidence.

## What the project preserves

The software provides contracts for a checked R source package, SHA-256 checksums, machine-readable release manifests, scientific-validation records, session information, installed-package manifests, immutable project bundles, artifact lineage, and FAIR research-object metadata. The release workflow verifies that the Git tag and `DESCRIPTION` version agree before publication.

Versioned OCI images are built from the exact source revision and can be identified by semantic-version, commit-SHA, and immutable digest references. BuildKit supports software-bill-of-materials and provenance attestations. Native Apptainer builds and OCI-to-SIF conversion support HPC environments.

The repository retains human-editable environment specifications and validation workflows. These software contracts make required evidence representable and testable; they do not assert that the production 0.10.0 release evidence has already been executed, approved, or deposited.

## Minimum reproducibility record

A result should not be treated as independently reproducible unless its record identifies at least:

- the popgenVCF package version or Git commit;
- the source-package checksum when a source archive was installed;
- the immutable container digest when a container was used;
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

Computational reproducibility infrastructure is distinct from approved scientific release evidence. A schema, validator, benchmark budget, concordance contract, or release-certificate class demonstrates that evidence can be represented and checked; it does not demonstrate that the canonical real dataset was processed successfully or that a scientist approved the resulting evidence.

The 0.10.0 release may be described as release-ready only after the exact tagged commit has complete checksum-linked canonical validation, quantitative baseline, external-tool concordance, performance history, source-package, container, Apptainer, archive, and approval records. Development metadata intentionally omits a release date and DOI until the corresponding archive is published.

## Input and provenance responsibilities

Users remain responsible for preserving original VCF and metadata files, recording preprocessing performed outside popgenVCF, and ensuring that sample identifiers, population assignments, geographic coordinates, filtering thresholds, and external-program inputs are scientifically appropriate.

For archival work, use immutable identifiers whenever possible:

- a Git release tag and commit SHA;
- the source-package checksum;
- the OCI image digest rather than only a floating tag;
- input-file checksums;
- the complete analysis configuration and generated run records;
- persistent archive or DOI identifiers only after the deposited object exists.

## Scientific boundary

Computational reproducibility does not establish biological correctness. Automated checks detect software regressions, metadata drift, packaging failures, environment mismatches, and selected numerical inconsistencies. They do not independently validate sampling design, taxonomic identification, population definitions, marker ascertainment, model assumptions, causal interpretation, or suitability of a method for a particular dataset.

Results therefore still require domain-expert review, appropriate controls, sensitivity analyses, and—where applicable—independent replication and peer review.

## Reporting a reproducibility problem

Open a GitHub issue containing the popgenVCF version or commit, container digest if used, platform, configuration, input characteristics that can be shared, exact command, external-tool versions, and relevant logs or validation records. Do not upload confidential or restricted biological data to a public issue.
