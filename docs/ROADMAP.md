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
- [ ] **0.9.32 — Autosomal production validation execution and evidence** (#22)

### Phase 0.9.32 execution status

- [x] approve and checksum-pin the 1000 Genomes Phase 3 chromosome 22 source;
- [x] execute and retain candidate-bound structural validation for the complete chromosome 22 source;
- [x] define a bounded chromosome 22 QC, LD-pruning, and PCA proposal workflow;
- [x] execute the quantitative proposal workflow from the reviewed implementation and retain an importable, filename-bound proposal snapshot;
- [ ] scientifically review and approve or revise the proposed metric values and tolerances;
- [ ] complete external-tool concordance and remaining full-validation evidence.

These milestones complete the software, documentation, metadata, archival-readiness, and release-candidate decision contracts. They do not substitute for executing, reviewing, approving, depositing, and publishing the first production real-data baseline, external-tool concordance suite, cross-backend ancestry evidence, release benchmark history, exact distribution evidence, or final 0.10.0 release authorization.

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

### Remaining 0.10.0 production evidence and publication

- [ ] execute full canonical validation and external-tool concordance (#22);
- [ ] approve the production quantitative baseline and real-data cross-backend ancestry evidence (#22, #24);
- [ ] publish approved release benchmark history and supporting trend evidence (#43);
- [ ] validate the exact source, OCI, and Apptainer distribution artifacts from clean environments;
- [ ] assemble and review the complete archival evidence release;
- [ ] evaluate a production dossier for the exact candidate commit and obtain `READY` status;
- [ ] obtain named scientific approval and final release authorization;
- [ ] tag, publish, deposit, and assign the real DOI only after every required gate passes (#1).

### Validation datasets and benchmarking

- [x] retain tiny synthetic fixtures in every CI run;
- [x] adopt a licensed, checksum-pinned canonical dataset under an approved registry entry;
- [x] define production baseline, scientific concordance, performance-budget, and release-evidence contracts;
- [x] execute and retain the first reviewable production quantitative baseline proposal from the canonical real dataset;
- [ ] scientifically approve or revise the production quantitative baseline proposal;
- [ ] execute and publish complete external-tool scientific concordance evidence;
- [ ] execute and approve a real-data three-backend ancestry validation case;
- [ ] publish runtime, memory, scaling, and historical regression artifacts per release;
- [ ] publish or externally host a checksum-pinned medium or large benchmark tier when licensing and storage policy permit.

### Documentation and metadata

- [x] first-analysis tutorial, scientific interpretation, troubleshooting, reproducibility, and deployment/HPC guide set;
- [x] maintained publication figure gallery and end-to-end narrative example;
- [x] backend-specific ancestry installation, configuration, smoke-check, and provenance guidance;
- [x] canonical `CITATION.cff`, `codemeta.json`, installed citation, FAIR software identity, and reproducibility statement;
- [x] DOI-ready, development-safe Zenodo deposition metadata;
- [x] source and OCI SBOMs, checksums, provenance, and archival verification instructions;
- [x] release-candidate evidence-bundle and reviewer-dossier operating guidance;
- [ ] record the real release date, DOI, concept DOI, and archive identifiers only after successful publication.

## Open tracking issues and deferred enhancements

- **#4 — Publication-quality platform:** umbrella tracker for remaining 0.10 and 1.0 release work.
- **#22 — Canonical real-data validation:** retains the uncompleted production baseline, external-tool execution, approval, and full-validation work.
- **#24 — Unified ancestry platform:** retains approved real-data three-backend execution, comparison, and release evidence; installation guidance is complete.
- **#43 — Continuous scientific benchmarks:** retains approved historical baselines, dashboards, and published release benchmark artifacts.
- **#1 — Reproducibility and release infrastructure:** remains open until a production dossier is ready and tagging, deposition, DOI assignment, and publication are complete.

## 1.0: stable scientific release

Release 1.0 requires stable CLI, YAML, R API, module and output contracts; validated core modules and canonical real-data results; a complete report engine; validated container and Apptainer artifacts; complete documentation and citation metadata; and reproducible release artifacts with checksums, SBOM, provenance, and persistent archive identifiers.

## Beyond 1.0

Potential post-1.0 work includes selection scans, genomic landscapes, spatial resistance models, GWAS interoperability, community plugins, interactive exploration, optional Docker Hub publication, and cloud/workflow-platform execution.
