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

These milestones complete the software contracts and fail-closed evidence models. They do not substitute for executing, reviewing, approving, and publishing the first production real-data baseline, external-tool concordance suite, and release benchmark history.

### Current stabilization gate

Repository health reconciliation must complete before feature work resumes:

- [ ] eliminate false release-readiness paths in concordance and benchmark evidence;
- [ ] replace runtime namespace mutation with explicit public exports;
- [ ] report roxygen, NAMESPACE, Rd, API-baseline, and release-metadata drift deterministically;
- [ ] synchronize README, NEWS, roadmaps, and issue trackers;
- [ ] close completed umbrella issues and retain only work with unmet acceptance criteria;
- [ ] run the complete CI matrix on the reconciled state.

### Authoritative sequence after stabilization

1. **0.9.30 — Documentation, metadata, and archival readiness**
   - complete pkgdown tutorials, interpretation guides, publication gallery, citation and software metadata, reproducibility statements, GHCR and Apptainer usage, HPC guidance, Zenodo configuration, DOI-ready metadata, SBOM, checksum, and provenance instructions;
   - resolve remaining advisory roxygen-to-namespace drift and regenerate all derived package documentation from source (#284);
   - eliminate avoidable `R CMD check` notes and source-package hygiene defects (#285).
2. **0.9.31 — 0.10.0 release-candidate closure**
   - synchronize DESCRIPTION, NEWS, roadmap, and trackers;
   - execute full canonical validation and external-tool concordance, approve the production quantitative baseline, publish release benchmarks, and issue the real release certificate;
   - validate container and Apptainer artifacts, assemble archives, run source and distribution install tests, and produce a reviewer-ready release-readiness report;
   - tag and publish 0.10.0 only after every required gate passes.

### Validation datasets and benchmarking

- [x] retain tiny synthetic fixtures in every CI run;
- [x] adopt a licensed, checksum-pinned canonical dataset under an approved registry entry;
- [x] define production baseline, scientific concordance, performance-budget, and release-evidence contracts;
- [ ] execute, approve, and retain the first production quantitative baseline snapshot from the canonical real dataset;
- [ ] execute and publish complete external-tool scientific concordance evidence;
- [ ] publish runtime, memory, scaling, and historical regression artifacts per release;
- [ ] publish or externally host a checksum-pinned medium or large benchmark tier when licensing and storage policy permit.

### Documentation and metadata

- [ ] pkgdown website, tutorials, interpretation guides, and figure gallery;
- [ ] verify and finalize `CITATION.cff`, `codemeta.json`, and the reproducibility statement;
- [ ] GHCR usage, Apptainer definition, and HPC guidance;
- [ ] Zenodo configuration and DOI-ready archive metadata;
- [ ] SBOM, checksums, and provenance instructions.

## Open tracking issues and deferred enhancements

- **#4 — Publication-quality platform:** umbrella tracker for remaining 0.10 and 1.0 release work.
- **#22 — Canonical real-data validation:** retains the uncompleted production baseline, external-tool execution, approval, and full-validation workflow work.
- **#43 — Continuous scientific benchmarks:** retains CI/release integration, approved historical baselines, dashboards, and published release benchmark artifacts.
- **#68 — Analysis-specific publication narratives:** retains any module-specific narrative, citation, and supplementary integration not yet demonstrated end to end.
- **#284 — Generated API documentation reconciliation:** resolves remaining roxygen, namespace, Rd, and API-baseline drift.
- **#285 — R CMD check note cleanup:** removes source-package hygiene, internal namespace, NSE/import, and Rd formatting notes.
- **#1 — Reproducibility and release infrastructure:** remains open until documentation, metadata, archival integration, and release automation are complete.

## 1.0: stable scientific release

Release 1.0 requires stable CLI, YAML, R API, module and output contracts; validated core modules and canonical real-data results; a complete report engine; validated container and Apptainer artifacts; complete documentation and citation metadata; and reproducible release artifacts with checksums, SBOM, and provenance.

## Beyond 1.0

Potential post-1.0 work includes selection scans, genomic landscapes, spatial resistance models, GWAS interoperability, community plugins, interactive exploration, optional Docker Hub publication, and cloud/workflow-platform execution.
