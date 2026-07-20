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

### Closure milestone

- [ ] **9.14.1 — Closure evidence assembly and roadmap synchronization**
  - bind milestones 9.1–9.14 to immutable merge commits;
  - assemble all required closure evidence domains;
  - bind release-readiness, migration-registry, deprecation-portfolio, and CI identities;
  - reject closure with missing evidence or unresolved blockers;
  - generate a deterministic closure report and Phase 10.1 handoff;
  - synchronize operator guidance and both packaged roadmap copies.

Phase 9 is declared complete only when the assembled closure review is approved. Merged implementation milestones alone do not satisfy this gate. See [Phase 9 closure evidence and roadmap handoff](PHASE9_CLOSURE.md).

## Phase 10: stable public scientific interface

Phase 10 exposes the unified runtime through a stable, documented user-facing scientific API while preserving all Phase 8 and Phase 9 guarantees.

### Next milestone

- [ ] **10.1 — Canonical public analysis and artifact API**
  - define stable public entry points for module execution, canonical results, publication artifacts, reports, and provenance;
  - separate supported public contracts from internal orchestration details;
  - version public schemas and define compatibility and migration behavior;
  - provide deterministic inspection, serialization, and report-regeneration interfaces;
  - audit exports, S3 methods, errors, status names, and metadata semantics;
  - add focused API, compatibility, documentation, and end-to-end tests.

### Entry criteria

Phase 10.1 starts only after the Phase 9 closure review is approved, required CI and scientific-validation evidence is immutable, migration blockers are empty, and rollback and compatibility obligations are documented.

## 0.9: publication-quality analysis platform

### Publication system

- [ ] automatic HTML, PDF, and DOCX reports;
- [ ] generated Methods and Results text sourced from canonical tables;
- [ ] numbered figures, tables, captions, and supplementary outputs;
- [ ] software citations and reference bibliography;
- [ ] journal presets for general, Nature-style, G3, Molecular Ecology, and PLOS layouts;
- [ ] accessible and grayscale-safe figure modes;
- [ ] complete provenance and reproducibility appendix.

### Core analysis presentation

- [ ] publication PCA and ordination outputs with stable source-data exports;
- [ ] DAPC optimization, cross-validation, confusion matrices, and membership plots;
- [ ] global and pairwise FST with uncertainty, heatmaps, and supplementary results;
- [ ] population diversity, private alleles, frequency spectra, and confidence intervals;
- [ ] unified ancestry manifests, deterministic replicates, alignment, consensus, and K diagnostics;
- [ ] AMOVA, isolation-by-distance, and later spatial-genetics adapters.

### Validation datasets and benchmarking

- [ ] retain tiny synthetic fixtures in every CI run;
- [ ] adopt a licensed canonical real dataset for documentation and integration tests;
- [ ] publish a checksum-pinned large benchmark dataset externally;
- [ ] compare runtime, memory, and numerical agreement with established tools;
- [ ] publish benchmark artifacts per release.

### Documentation and metadata

- [ ] pkgdown website, tutorials, interpretation guides, and figure gallery;
- [ ] `CITATION.cff`, `codemeta.json`, and reproducibility statement;
- [ ] GHCR usage, Apptainer definition, and HPC guidance;
- [ ] Zenodo integration for stable releases.

## 1.0: stable scientific release

Release 1.0 requires stable CLI, YAML, R API, module and output contracts; validated core modules and canonical real-data results; a complete report engine; validated container and Apptainer artifacts; complete documentation and citation metadata; and reproducible release artifacts with checksums, SBOM, and provenance.

## Beyond 1.0

Potential post-1.0 work includes selection scans, genomic landscapes, spatial resistance models, GWAS interoperability, community plugins, interactive exploration, and cloud/workflow-platform execution. These do not displace the validation and stability requirements of the core toolkit.
