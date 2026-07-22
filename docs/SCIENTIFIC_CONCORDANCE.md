# External-tool scientific concordance

Phase 0.9.28 turns external-reference comparisons into reviewable, release-gated scientific evidence.

## Scope

A concordance record binds one canonical dataset and analysis to:

- the external implementation and exact version;
- the exact command or reproducible invocation;
- a named tolerance profile and scientific role;
- the full long-form numerical comparison table;
- environment and container provenance;
- interpretation and citations;
- an explicit proposed or approved state.

`equivalence` comparisons are release gating. Failed, skipped, errored, or unapproved equivalence records prevent release readiness. `diagnostic` comparisons preserve scientifically meaningful cross-method differences without incorrectly requiring numerical identity between methods that estimate different quantities.

## Required production inventory

The full-validation workflow should cover scientifically appropriate combinations of:

- PLINK 2 for PCA, allele-frequency, IBS, and distance checks;
- SNPRelate for PCA, IBS, and LD-sensitive checks;
- hierfstat for diversity and F-statistics;
- adegenet for PCA and DAPC-oriented checks;
- a documented practical AMOVA reference implementation.

Not every tool is authoritative for every analysis. Each comparison must state whether it tests implementation equivalence or supplies diagnostic context.

## Evidence

`write_scientific_concordance_evidence()` writes deterministic TSV, JSON, and Markdown methods artifacts. Production finalization uses `require_release_ready = TRUE`, which refuses evidence finalization unless every equivalence comparison passed and received explicit scientific approval.

Ordinary pull-request CI remains synthetic, offline, and fast. Tool installation, canonical dataset acquisition, command execution, logs, and approval-ready proposals belong in opt-in or scheduled full-validation CI.
