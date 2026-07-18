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

### Planned milestones

- [ ] **8.6 — Deterministic cancellation and graceful shutdown**
  - user- and system-requested cancellation at safe execution boundaries;
  - explicit `cancelled` ledger states and cancellation metadata;
  - checkpoint-on-cancel and safe resumability;
  - clear separation of cancellation, timeout, and failure.
- [ ] **8.7 — Resource policies and execution admission**
  - declarative thread, memory, temporary-storage, and process requirements;
  - conservative admission and concurrency decisions;
  - explicit resource-unavailable and resource-limit states;
  - resource-policy provenance in execution metadata.
- [ ] **8.8 — Supervised external-process execution**
  - a canonical subprocess runner for PLINK, ADMIXTURE, fastStructure, bcftools, and related tools;
  - process-level timeouts and process-tree termination;
  - normalized exit codes, stdout, stderr, command provenance, and cleanup;
  - deterministic external-tool failure records.
- [ ] **8.9 — Execution observability and run telemetry**
  - structured execution events and standardized logging;
  - module and attempt timing;
  - warnings, progress, and compact run summaries;
  - machine-readable timelines and dependency-aware visualizations.
- [ ] **8.10 — Execution hardening and stable runtime API**
  - complete state-transition and interaction testing;
  - checkpoint compatibility and corruption handling;
  - timeout, retry, cancellation, resource, and subprocess integration tests;
  - performance regression benchmarks;
  - versioned execution schemas and stable public documentation.

### Phase 8 completion criterion

Phase 8 is complete when success, validation failure, dependency blocking, timeout, cancellation, resource exhaustion, and external-process failure are deterministic, auditable, fail closed, and resumable where scientifically safe.

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
