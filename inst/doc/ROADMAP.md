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

Phase 8 is complete when every terminal state is deterministic, auditable, fail closed, resumable where scientifically safe, and governed by a versioned runtime contract.

## Phase 9: unified publication-quality module runtime

Phase 9 connects stable plugin, scientific-object, schema, cache, checkpoint, migration, validation, provenance, and publication contracts to the Phase 8 runtime without introducing a competing executor.

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

Phase 9 closure evidence binds the completed milestones, release-readiness, migration, deprecation, CI, and roadmap-handoff records. See [Phase 9 closure evidence and roadmap handoff](PHASE9_CLOSURE.md).

## Phase 10: stable public scientific interface

Phase 10 exposes the unified runtime through a stable, documented user-facing scientific API while preserving all Phase 8 and Phase 9 guarantees.

### Completed milestones

- [x] **10.1 — Canonical public analysis and artifact API**
- [x] **10.2 — Public API compatibility and release conformance**

## 0.10: publication-quality release candidate

The authoritative development package version is **0.10.0**. This series reconciles the completed public interface with the remaining scientific-validation and release-evidence work required before 1.0.

### Completed milestones

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
  - fingerprinted licensed-dataset manifests with source, version, checksum, and license metadata;
  - deterministic sample, population, locus, expected-result, tolerance, and external-tool comparison inventories;
  - CI-safe synthetic fixtures, opt-in canonical integration fixtures, provenance, and fail-closed drift detection.

### Active milestone

- [ ] **0.9.14 — Release-state and public-API reconciliation**
  - retain 0.10.0 as the authoritative development release identity;
  - audit exports, S3 registrations, Rd aliases, and generated documentation;
  - emit deterministic reconciliation evidence and fail closed on release-state drift;
  - synchronize README, NEWS, DESCRIPTION, roadmap copies, and tracking issues.

### Planned sequence after reconciliation

1. canonical contract hardening;
2. IBS/MDS publication outputs;
3. licensed canonical real-data integration and external comparisons;
4. release evidence, metadata, benchmark publication, and container/HPC documentation;
5. 0.10.0 release-readiness review.

### Validation datasets and benchmarking

- [x] retain tiny synthetic fixtures in every CI run;
- [ ] adopt a licensed canonical real dataset for documentation and integration tests;
- [ ] publish a checksum-pinned large benchmark dataset externally;
- [ ] compare runtime, memory, and numerical agreement with established tools;
- [ ] publish benchmark artifacts per release.

### Documentation and metadata

- [ ] pkgdown website, tutorials, interpretation guides, and figure gallery;
- [ ] `CITATION.cff`, `codemeta.json`, and reproducibility statement;
- [ ] GHCR usage, Apptainer definition, and HPC guidance;
- [ ] Zenodo integration for stable releases.

## Open tracking issues and deferred enhancements

The following open issues remain authoritative tracking containers and are intentionally not closed:

- **#4 — publication-quality platform:** umbrella tracker for remaining 0.10 and 1.0 release work.
- **#20 — Core publication artifact contracts:** remains open for IBS/MDS and remaining publication artifacts.
- **#22 — Canonical real-data validation:** covers licensed datasets, expected values, external-tool comparisons, and CI/full-validation workflows.
- **#24 — Unified ancestry platform:** covers backend/runtime enhancements beyond the completed publication ancestry contract.
- **#43 — Continuous scientific benchmarks:** covers historical regression archives, cross-tool comparisons, resource/scaling measurements, dashboards, and release benchmark artifacts.
- **#68 — Analysis-specific publication narratives:** covers remaining module-specific methods, legends, citations, and supplementary narratives.
- **#254 — Release-state and public-API reconciliation:** owns the active 0.9.14 implementation and acceptance criteria.

The reproducibility and release-infrastructure tracker **#1** also remains open until the documentation/metadata and release-automation items above are complete. Optional Docker Hub publishing is a later enhancement dependent on credentials and is not required for 1.0; Zenodo integration remains a post-stable-release task.

## 1.0: stable scientific release

Release 1.0 requires stable CLI, YAML, R API, module and output contracts; validated core modules and canonical real-data results; a complete report engine; validated container and Apptainer artifacts; complete documentation and citation metadata; and reproducible release artifacts with checksums, SBOM, and provenance.

## Beyond 1.0

Potential post-1.0 work includes selection scans, genomic landscapes, spatial resistance models, GWAS interoperability, community plugins, interactive exploration, optional Docker Hub publication, and cloud/workflow-platform execution. These do not displace the validation and stability requirements of the core toolkit.
