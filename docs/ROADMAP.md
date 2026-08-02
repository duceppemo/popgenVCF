# Development Roadmap

The roadmap is governed by the [Project Charter](PROJECT_CHARTER.md). Scientific correctness and validation take priority over schedule or feature count.

## Completed foundation: 0.4–0.8

The completed foundation includes registry-managed analysis modules, serializable project state, deterministic QC and LD pruning, numerical and population-structure validation, reproducible environments, package CI, scientific-validation CI, and validated container publishing.

## Phase 8: deterministic execution and operational reliability

Phase 8 established the deterministic, recoverable, and scientifically auditable execution runtime.

### Completed milestones

- [x] **8.1 — Unified analysis execution engine**
- [x] **8.2 — Dependency-aware failure propagation and execution records**
- [x] **8.3 — Deterministic checkpoints and resume support**
- [x] **8.4 — Deterministic retry and recovery orchestration**
- [x] **8.5 — Deterministic execution timeouts**
- [x] **8.6 — Deterministic cancellation and graceful shutdown**
- [x] **8.7 — Resource policies and execution admission**
- [x] **8.8 — Supervised external-process execution**
- [x] **8.9 — Portable deterministic concurrent scheduling**
- [x] **8.10 — Execution hardening and stable runtime API**

## Phase 9: unified publication-quality module runtime

Phase 9 connected stable plugin, scientific-object, schema, cache, checkpoint, migration, validation, provenance, and publication contracts to the Phase 8 runtime.

### Completed implementation milestones

- [x] **9.1 — Finalize the module plugin contract**
- [x] **9.2 — Canonical result and publication-artifact contracts**
- [x] **9.3 — Canonical reusable scientific data objects**
- [x] **9.4 — Centralized schema validation and compatibility enforcement**
- [x] **9.5 — Deterministic scientific object caching**
- [x] **9.6 — Deterministic module execution planning and orchestration**
- [x] **9.7 — Deterministic execution checkpoints and recovery**
- [x] **9.8 — Unified module execution integration**
- [x] **9.9 — Executable vertical slice and Phase 8 runtime adapter**
- [x] **9.10 — First built-in module runtime integration**
- [x] **9.11 — First production analysis module migration**
- [x] **9.12 — Production module migration registry and staged cutover**
- [x] **9.13 — Legacy runtime deprecation and unified release readiness**
- [x] **9.14 — Final closure-review and roadmap-handoff contracts**
- [x] **9.14.1 — Closure evidence assembly and roadmap synchronization**

See [Phase 9 closure evidence and roadmap handoff](PHASE9_CLOSURE.md).

## Phase 10: stable public scientific interface

- [x] **10.1 — Canonical public analysis and artifact API**
- [x] **10.2 — Public API compatibility and release conformance**

## 0.10: publication-quality release candidate

The authoritative development package version is **0.10.0**.

### Completed implementation milestones

- [x] **0.9.1 — Deterministic publication report rendering**
- [x] **0.9.2 — Journal presets and deterministic publication layouts**
- [x] **0.9.3 — Accessible and grayscale-safe figure modes**
- [x] **0.9.4 — Deterministic submission packages and supplementary indexes**
- [x] **0.9.5 — Publication PCA and ordination outputs**
- [x] **0.9.6 — Publication DAPC outputs**
- [x] **0.9.7 — Publication FST outputs**
- [x] **0.9.8 — Publication diversity outputs**
- [x] **0.9.9 — Publication ancestry outputs**
- [x] **0.9.10 — Publication AMOVA outputs**
- [x] **0.9.11 — Publication isolation-by-distance outputs**
- [x] **0.9.12 — Publication spatial-genetics outputs**
- [x] **0.9.13 — Canonical real-data validation contracts**
- [x] **0.9.14 — Release-state and public-API reconciliation**
- [x] **0.9.15 — Public API contract hardening**
- [x] **0.9.16 — Canonical API baseline and CI enforcement**
- [x] **0.9.17 — Publication IBS and MDS outputs**
- [x] **0.9.18 — Canonical real-data materialization and external-comparison contracts**
- [x] **0.9.19 — Canonical dataset registry and approval gate**
- [x] **0.9.20 — First approved canonical dataset integration**
- [x] **0.9.21 — Canonical validation suites**
- [x] **0.9.22 — Quantitative canonical baseline metrics**
- [x] **0.9.23 — Longitudinal canonical drift detection**
- [x] **0.9.24 — Canonical scientific change approval and reconciliation**
- [x] **0.9.25 — Canonical release-readiness gate**
- [x] **0.9.26 — Canonical release bundle integration**
- [x] **0.9.27 — Canonical real-data baseline adoption contract**
- [x] **0.9.28 — External-tool scientific concordance contract**
- [x] **0.9.29 — Continuous release benchmarking contract**
- [x] **0.9.30a — Source-package and R CMD check hygiene** (#285)
- [x] **0.9.30b — Generated API documentation and canonical interface reconciliation** (#284)
- [x] **0.9.30.1 — User tutorials, scientific interpretation, deployment, and troubleshooting** (#288)
- [x] **0.9.30.2 — Publication narrative completeness and maintained gallery** (#68)
- [x] **0.9.30.3 — Citation, software, and reproducibility metadata reconciliation** (#291)
- [x] **0.9.30.4 — DOI-ready archival metadata, SBOMs, checksums, and provenance** (#297)
- [x] **0.9.31 — Release-candidate closure policy, reviewer dossier, and workflow** (#299)
- [x] **0.9.32 — Autosomal production validation execution and evidence** (#22)

### Phase 0.9.32 execution status

- [x] approve and checksum-pin the 1000 Genomes Phase 3 chromosome 22 source;
- [x] execute and retain candidate-bound structural validation for the complete chromosome 22 source;
- [x] define a bounded chromosome 22 QC, LD-pruning, and PCA proposal workflow;
- [x] execute the quantitative proposal workflow from the reviewed implementation and retain an importable, filename-bound proposal snapshot;
- [x] scientifically review and approve or revise the proposed metric values and tolerances;
- [x] complete external-tool concordance and remaining full-validation evidence.

These milestones complete the software, documentation, metadata, archival-readiness, and release-candidate decision contracts, and -- as of the 2026-08-01/2026-08-02 closure below -- the production real-data baseline, external-tool concordance suite, cross-backend ancestry evidence, release benchmark history, exact distribution evidence, and final 0.10.0 release authorization have all been executed, reviewed, approved, deposited, and published.

**2026-08-01 progress note:** Marc-Olivier Duceppe scientifically reviewed and approved all six registered chr22 autosomal baseline metrics (see `docs/developer/canonical-autosomal-baseline-proposal.md`); that evidence was already durably retained as a checksum-verified GitHub Actions artifact. In the same session, `external_concordance` and `ancestry_three_backend` were each executed for the first time against real chr22 data and reviewed (see `docs/SCIENTIFIC_CONCORDANCE.md` and `docs/user/ancestry-backends.md`), and `benchmark_history`'s evidence-generation pipeline was reconciled, executed through real CI, and reviewed (see `docs/CONTINUOUS_RELEASE_BENCHMARKING.md`). At that point none of the last three were checked off above: their evidence had not yet been produced through a durable, retained production path, their underlying evidence objects had not been formally transitioned from `approval: proposed` to `approved`, and `benchmark_history` was further blocked on this project's first published GitHub Release.

**2026-08-01 continued:** the missing `read_`/`approve_` functions for the ancestry and concordance evidence contracts were added (`read_canonical_ancestry_three_backend_evidence()`, `approve_scientific_concordance_record()`, `read_scientific_concordance_suite()`), and used to formally transition `production_baseline`, `external_concordance` (its three equivalence records), and `ancestry_three_backend` from `proposed` to `approved` against their already-recorded reviewer determinations. A new production evidence-index assembly script (`scripts/build_release_candidate_evidence_index.R`, the real-evidence counterpart to the synthetic-only `build_release_candidate_rehearsal.R`) checksum-bound that approved evidence into a real `release-candidate-evidence-index.json`, evaluated end to end through `build_release_candidate_dossier.R`: `release_ready: false`, `3 / 15` required gates passed. That index, its four gate-evidence artifacts, the dossier, and a freshly rehearsed source-release bundle were published as assets on this project's first GitHub Release, `0.10.0-rc1` (explicitly a pre-release, not v0.10.0) -- durably retaining the three approved gates publicly for the first time, and giving `release-benchmark-archive.yml` a real prior release to compare future benchmark runs against. `container.yml` was hardened to skip its GHCR publish step for GitHub pre-releases (it previously fired on every published release regardless of tag), so no container image was pushed for this checkpoint. `benchmark_history` remains `blocked`, not `passed` -- a real trend-comparison run against `0.10.0-rc1` has not yet been executed -- and the remaining 11 gates (software, distribution, archival, and the two final approval gates) remain `not_run`: most already have real CI evidence produced elsewhere in this repository, but none of it is wired into the checksum-bound evidence-index format yet.

**2026-08-01, release closure:** the remaining gates were wired in one at a time against real CI evidence -- `metadata_consistency`, `public_api_contract`, `source_package_check`, `scientific_validation`, `source_distribution`, `apptainer_distribution`, `canonical_validation`, `benchmark_history` (after fixing a missing `GH_TOKEN` bug that had silently broken every baseline-discovery attempt), and `oci_distribution` (a real registry push to GHCR) -- each verified end to end through `build_release_candidate_dossier.R`. Marc-Olivier Duceppe, as named scientific reviewer, then approved the complete scientific evidence set and, as release owner, authorized tagging, publication, container-image publication, and Zenodo deposition. The real GitHub Release `v0.10.0` was tagged and published (superseding the `0.10.0-rc1` pre-release, which was deleted once the real release existed), Zenodo deposition completed, and `archival_assets` closed out the dossier: **`READY`, 15 / 15 required gates passed**, DOI [10.5281/zenodo.21747548](https://doi.org/10.5281/zenodo.21747548) (concept DOI [10.5281/zenodo.21747067](https://doi.org/10.5281/zenodo.21747067)), 2026-08-01.

**2026-08-01/2026-08-02, generic tooling and issue closure:** the hand-authored, 0.10.0-specific evidence-index script was replaced with a generic, self-describing `<gate_id>-gate-record.json` fragment contract (`inst/scripts/release_candidate_gate_record.R`) that any CI workflow or reviewer script can write. Six CI workflows now write their own fragments as a normal part of their run; `.github/workflows/release-candidate-collector.yml` dispatches and collects all six in parallel for a future candidate ref; `scripts/write_scientific_review_gate_record.R` lets the named reviewer materialize a determination for the 4 gates on their standing assignment (`production_baseline`, `external_concordance`, `ancestry_three_backend`, `benchmark_history`) without hand-computing checksums. Re-run against the real `v0.10.0` evidence, the new tooling independently reproduced the same `12 / 15` result the hand-authored path had proven, with the 3 one-off, release-specific gates (`scientific_approval`, `release_authorization`, `archival_assets`) correctly left `not_run` for a not-yet-cut future candidate. With the release itself closed out, issues #1, #22, #24, and #43 were closed against this real evidence; #43 was closed with an explicit note that the `canonical`/`medium`/`large` benchmark-dataset tiers and a trend dashboard remain unimplemented, deferred rather than dropped (see "Validation datasets and benchmarking" below). #4 remains open as the umbrella tracker for the still-open 1.0 decision (see "1.0: stable scientific release").

### Completed stabilization gate

Repository health and release-candidate infrastructure are reconciled:

- [x] eliminate false release-readiness paths in concordance and benchmark evidence;
- [x] replace runtime namespace mutation with explicit public exports;
- [x] report roxygen, NAMESPACE, Rd, API-baseline, release-metadata, LICENSE, Zenodo, and action-pin drift deterministically;
- [x] synchronize README, NEWS, roadmaps, and issue trackers;
- [x] retire obsolete competing roadmap material;
- [x] eliminate avoidable package-check notes and source-package hygiene defects;
- [x] establish deterministic roxygen generation and a 613-entry canonical installed API baseline;
- [x] pin external GitHub Actions to immutable commits with controlled update automation;
- [x] generate source and OCI SBOM/provenance evidence and checksum-linked archival manifests;
- [x] define a checksum-verified 15-gate production evidence contract and deterministic reviewer dossier;
- [x] run the complete relevant CI matrix on the reconciled state.

### Completed Phase 0.9.30 sequence

1. **Opening maintenance**
   - [x] eliminate avoidable `R CMD check` notes and source-package hygiene defects (#285);
   - [x] reconcile roxygen ownership, generated `NAMESPACE`/Rd files, S3 registrations, and the canonical API baseline (#284).
2. **Documentation, metadata, and archival readiness**
   - [x] complete the first-analysis tutorial, scientific interpretation guide, troubleshooting guide, reproducibility guide, and GHCR/Apptainer/HPC deployment guidance (#288);
   - [x] complete the maintained publication gallery and end-to-end narrative integration (#68);
   - [x] reconcile citation, installed-package, CodeMeta, FAIR software, and reproducibility metadata with development-safe release boundaries (#291);
   - [x] add DOI-ready Zenodo metadata, source and OCI SBOMs, provenance evidence, release checksums, manifests, and archival instructions (#297).

### Completed Phase 0.9.31 closure mechanism

- [x] define a canonical 15-gate release-candidate policy;
- [x] require exactly one checksum-verifiable evidence record per gate;
- [x] distinguish deliberately blocked rehearsal evaluation from production evaluation;
- [x] require named reviewer identity and review dates for approval-gated evidence;
- [x] reject missing, duplicate, malformed, traversing, resized, or checksum-mismatched evidence;
- [x] generate deterministic gate, blocker, artifact, JSON, Markdown, and terminal checksum records;
- [x] add a pull-request rehearsal and manual production-evaluation workflow;
- [x] document backend-specific ADMIXTURE, fastStructure, and LEA/sNMF installation and provenance;
- [x] preserve the 613-entry public API and all publication boundaries.

### Completed 0.10.0 production evidence and publication

- [x] execute full canonical validation and external-tool concordance (#22);
- [x] approve the production quantitative baseline and real-data cross-backend ancestry evidence (#22, #24);
- [x] publish approved release benchmark history and supporting trend evidence (#43);
- [x] validate the exact source, OCI, and Apptainer distribution artifacts from clean environments;
- [x] assemble and review the complete archival evidence release;
- [x] evaluate a production dossier for the exact candidate commit and obtain `READY` status;
- [x] obtain named scientific approval and final release authorization;
- [x] tag, publish, deposit, and assign the real DOI (#1).

v0.10.0 is published: DOI [10.5281/zenodo.21747548](https://doi.org/10.5281/zenodo.21747548), 2026-08-01. The production dossier for the released commit reports `READY`, 15 / 15 required gates passed.

### Validation datasets and benchmarking

- [x] retain tiny synthetic fixtures in every CI run;
- [x] adopt a licensed, checksum-pinned canonical dataset under an approved registry entry;
- [x] define production baseline, scientific concordance, performance-budget, and release-evidence contracts;
- [x] execute and retain the first reviewable production quantitative baseline proposal from the canonical real dataset;
- [x] scientifically approve or revise the production quantitative baseline proposal;
- [x] execute and publish complete external-tool scientific concordance evidence;
- [x] execute and approve a real-data three-backend ancestry validation case;
- [x] publish runtime, memory, scaling, and historical regression artifacts per release;
- [ ] publish or externally host a checksum-pinned medium or large benchmark tier when licensing and storage policy permit (deferred; `canonical`/`medium`/`large` tiers remain unimplemented placeholders in `R/benchmark_datasets.R`, and no trend dashboard has been published -- see the 0.10.0 release note above, "0.9.29", and the #43 closure comment).

### Documentation and metadata

- [x] first-analysis tutorial, scientific interpretation, troubleshooting, reproducibility, and deployment/HPC guide set;
- [x] maintained publication figure gallery and end-to-end narrative example;
- [x] backend-specific ancestry installation, configuration, smoke-check, and provenance guidance;
- [x] canonical `CITATION.cff`, `codemeta.json`, installed citation, FAIR software identity, and reproducibility statement;
- [x] DOI-ready, development-safe Zenodo deposition metadata;
- [x] source and OCI SBOMs, checksums, provenance, and archival verification instructions;
- [x] release-candidate evidence-bundle and reviewer-dossier operating guidance;
- [x] record the real release date, DOI, concept DOI, and archive identifiers after successful publication.

## Open tracking issues and deferred enhancements

- **#4 — Publication-quality platform:** the only remaining open tracker; umbrella issue for the explicit 1.0 decision (see below). Its 0.10.0 checklist is fully satisfied by the released state.
- **#22, #24, #43, #1:** closed 2026-08-02 against the real, checksum-verified v0.10.0 evidence and the `READY` 15 / 15 production dossier. #43 was closed with a note that the `canonical`/`medium`/`large` benchmark tiers and a trend dashboard remain deferred, unimplemented follow-up work, not a release blocker.

## 1.0: stable scientific release

Release 1.0 requires stable CLI, YAML, R API, module and output contracts; validated core modules and canonical real-data results; a complete report engine; validated container and Apptainer artifacts; complete documentation and citation metadata; and reproducible release artifacts with checksums, SBOM, provenance, and persistent archive identifiers.

As of the released v0.10.0 state (2026-08-01), every one of those technical criteria is met: the Phase 10 public API/CLI/YAML contracts are stable and CI-enforced; canonical real-data validation, external-tool concordance, and cross-backend ancestry evidence are approved; the Phase 9 publication report engine is complete; container and Apptainer distributions are validated; documentation and citation metadata are complete; and release artifacts (checksums, SBOMs, provenance, DOI) are real and published. What remains before 1.0 is not a technical gate but an explicit decision: 1.0 is a semver commitment (the public API is now stable; future breaking changes require a major version bump), not something CI can prove on its own. Tracked in #4.

## Beyond 1.0

Potential post-1.0 work includes selection scans, genomic landscapes, spatial resistance models, GWAS interoperability, community plugins, interactive exploration, optional Docker Hub publication, and cloud/workflow-platform execution.
