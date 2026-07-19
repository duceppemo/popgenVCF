# Development Roadmap

The roadmap is governed by the [Project Charter](PROJECT_CHARTER.md). Scientific correctness and validation take priority over schedule or feature count.

## Completed foundation: 0.4–0.8

- registry-managed analysis modules and dependency resolution;
- serializable `PopgenVCFAnalysis` state;
- deterministic QC and exact SNPRelate LD-pruning contract;
- numerical validation of QC, IBS, MDS, PCA, diversity, and FST;
- population-structure validation with label-switching-aware Q alignment;
- DAPC reproducibility and synthetic classification validation;
- optional ADMIXTURE, fastStructure, and LEA/sNMF adapters;
- reproducible Conda/Mamba environment;
- R package CI, coverage, scientific-validation CI, and validated GHCR container publishing.

## Phase 8: deterministic execution and operational reliability

Phase 8 turns the analysis registry into a deterministic, recoverable, and scientifically auditable execution runtime. Every terminal condition must be explicit, preserve valid prior work, reject incomplete outputs, and leave sufficient records for review or safe recovery.

### Completed milestones

- [x] **8.1 — Unified analysis execution engine**
  - canonical execution planning and module ordering;
  - shared execution contracts and result validation;
  - deterministic execution metadata.
- [x] **8.2 — Dependency-aware failure propagation and execution records**
  - explicit success, failure, and blocked states;
  - dependency-aware fail-closed propagation;
  - auditable execution ledgers.
- [x] **8.3 — Deterministic checkpoints and resume support**
  - validated checkpoint objects;
  - deterministic reconstruction of execution plans;
  - safe resume without silently changing completed work.
- [x] **8.4 — Deterministic retry and recovery orchestration**
  - bounded retry policies;
  - preserved attempt ledgers;
  - reuse of validated successful prerequisites;
  - explicit recovery metadata.
- [x] **8.5 — Deterministic execution timeouts**
  - global and named per-module elapsed-time budgets;
  - fail-closed timeout handling;
  - `timed_out` final and attempt-ledger states;
  - retry integration and timeout-policy metadata;
  - explicit documentation of interruptible-R and external-process boundaries.
- [x] **8.6 — Deterministic cancellation and graceful shutdown**
  - cooperative cancellation at safe module-launch boundaries;
  - explicit `cancelled` ledger states and cancellation provenance;
  - checkpoint-on-cancel and safe resumability;
  - strict separation of cancellation, timeout, and failure.
- [x] **8.7 — Resource policies and execution admission**
  - validated thread, memory, temporary-storage, and process requirements;
  - deterministic exact-capacity admission and execution batching;
  - explicit `resource_unavailable` decisions;
  - resource-policy provenance in execution records.
- [x] **8.8 — Supervised external-process execution**
  - canonical immutable command and normalized result contracts;
  - deterministic workspaces, staged-input manifests, and lifecycle ledgers;
  - timeout, cancellation, resource admission, and process-tree cleanup;
  - synchronous and asynchronous `processx` supervision APIs.
- [x] **8.9 — Portable deterministic concurrent scheduling**
  - cross-platform `multisession` scheduling for dependency-ready modules;
  - deterministic L'Ecuyer-CMRG worker RNG streams;
  - resource-aware dispatch with serial-module exclusivity;
  - validation and merge in planned module order regardless of completion timing;
  - dispatch, completion, merge, worker, backend, and scheduler provenance.

### Planned milestone

- [ ] **8.10 — Execution hardening and stable runtime API**
  - freeze and document the public execution-runtime surface, including execution, resume, cancellation, resource, subprocess, scheduling, and inspection entry points;
  - version execution, attempt-ledger, checkpoint, scheduler, resource, process-result, workspace, and lifecycle-event schemas;
  - define explicit backward-compatibility, migration, and unsupported-future-schema behavior;
  - add corruption, truncation, checksum, stale-reference, and incompatible-schema handling for checkpoints and persisted execution records;
  - complete state-transition coverage across success, validation failure, dependency blocking, retry, timeout, cancellation, resource rejection, subprocess failure, and concurrent execution;
  - add interaction tests for cancellation during asynchronous processes, timeout escalation, retry after process failure, resume after partial concurrent completion, and fail-fast scheduling;
  - add deterministic replay tests proving stable planned ordering, accepted-result ordering, fingerprints, and ledger semantics across sequential, multicore, and multisession backends;
  - add concurrency stress tests for worker completion races, equal-time tie-breaking, resource saturation, serial exclusivity, and repeated seeded runs;
  - establish performance and memory regression benchmarks for planning, ledger growth, checkpoint serialization, process supervision, and scheduler overhead;
  - audit exported functions, S3 methods, validation helpers, error classes, status names, and metadata fields for consistent naming and stable semantics;
  - publish a complete execution-runtime reference, lifecycle/state diagrams, compatibility policy, migration guide, and extension guidance for module and external-tool authors;
  - require the full package, scientific-validation, documentation, release, container, and benchmark matrix to pass before Phase 8 is declared complete.

### Phase 8 completion criterion

Phase 8 is complete when success, validation failure, dependency blocking, timeout, cancellation, resource exhaustion, external-process failure, and concurrent execution are deterministic, auditable, fail closed, resumable where scientifically safe, and governed by a documented versioned runtime contract.

## 0.9: publication-quality analysis platform

### Architecture and shared artifact contracts

- [ ] finalize the module plugin contract;
- [ ] enforce canonical result, table, figure, methods, caption, and validation outputs;
- [ ] reuse genotype, frequency, distance, and metadata objects across modules;
- [ ] add schema validation for module results and artifact manifests;
- [ ] add resumable stage-level caching with input/configuration hashes.

### Publication system

- [ ] automatic HTML, PDF, and DOCX reports;
- [ ] generated Methods and Results text sourced from canonical tables;
- [ ] numbered figures, tables, captions, and supplementary outputs;
- [ ] software citations and reference bibliography;
- [ ] journal presets for general, Nature-style, G3, Molecular Ecology, and PLOS layouts;
- [ ] accessible and grayscale-safe figure modes;
- [ ] complete provenance and reproducibility appendix.

### PCA and ordination

- [ ] scree and cumulative-variance plots;
- [ ] publication scatterplots and biplots;
- [ ] confidence ellipses and centroids with explicit exploratory labeling;
- [ ] sample labels and collision-safe annotation;
- [ ] stable SVG/PDF/PNG and source-data exports.

### DAPC

- [ ] retained-PC optimization and alpha-score workflow;
- [ ] repeated cross-validation and leave-one-out options;
- [ ] confusion matrices and posterior-membership tables;
- [ ] reproducibility summaries and publication membership plots.

### FST and population differentiation

- [ ] global and pairwise Weir-Cockerham FST reporting;
- [ ] chromosome/block bootstrap confidence intervals;
- [ ] heatmaps, dendrograms, and population-network views;
- [ ] per-locus supplementary results and uncertainty diagnostics.

### Diversity

- [ ] Ho, He, unbiased He, FIS, nucleotide diversity, and allelic richness;
- [ ] private alleles and frequency spectra;
- [ ] Shannon and Simpson diversity where scientifically appropriate;
- [ ] population and chromosome confidence intervals;
- [ ] neutrality statistics as separately validated later modules.

### Population structure

- [ ] unified ADMIXTURE, fastStructure, and sNMF run manifests;
- [ ] replicate execution and deterministic seed management;
- [ ] label alignment, consensus Q matrices, and stability metrics;
- [ ] CV/cross-entropy/BIC K diagnostics without conflating criteria;
- [ ] publication structure plots with explicit sample ordering.

### AMOVA and spatial genetics

- [ ] hierarchical AMOVA with permutation tests;
- [ ] variance-component tables and methods text;
- [ ] Mantel and partial Mantel analyses;
- [ ] geographic distance and isolation-by-distance plots;
- [ ] spatial autocorrelation and resistance-distance adapters in later milestones.

### Validation datasets and benchmarking

- [ ] retain tiny synthetic fixtures in every CI run;
- [ ] adopt a licensed canonical real dataset for documentation and integration tests;
- [ ] publish a checksum-pinned large benchmark dataset externally;
- [ ] compare runtime, memory, and numerical agreement with PLINK, SNPRelate, adegenet, hierfstat, and ADMIXTURE;
- [ ] publish benchmark artifacts per release.

### Documentation and metadata

- [ ] pkgdown website and analysis tutorials;
- [ ] statistical interpretation guides and figure gallery;
- [ ] `CITATION.cff`, `codemeta.json`, and reproducibility statement;
- [ ] GHCR usage, Apptainer definition, and HPC guidance;
- [ ] Zenodo integration for stable releases.

## 1.0: stable scientific release

Release 1.0 requires:

- stable CLI, YAML schema, R API, module contract, and canonical output schemas;
- all core modules meeting the charter’s definition of done;
- canonical real-data validation and published expected results;
- complete report engine and manuscript-oriented outputs;
- validated release container and Apptainer image;
- complete user manual, tutorials, citation metadata, and migration policy;
- reproducible source and binary artifacts with checksums, SBOM, and provenance.

## Beyond 1.0

Potential post-1.0 work includes selection scans, genomic landscapes, spatial resistance models, GWAS interoperability, community plugins, interactive exploration, and cloud/workflow-platform execution. These do not displace the validation and stability requirements of the core toolkit.
