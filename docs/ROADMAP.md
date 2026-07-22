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

### Active milestone

- [ ] **0.9.27 — Canonical real-data baseline adoption**
  - extend the approved licensed canonical dataset integration with production scientific validation descriptors and complete sample metadata;
  - generate the first quantitative baseline snapshot only from the checksum-verified real dataset;
  - record metric versions, comparators, tolerances, workflow provenance, commit SHA, and dataset artifact digests;
  - require explicit scientific review before a proposed snapshot becomes approved;
  - run acquisition and computation only in opt-in or scheduled full-validation CI;
  - keep ordinary pull-request CI synthetic, deterministic, offline, and fast;
  - publish descriptors, observations, comparisons, logs, and snapshot evidence as workflow artifacts.

### Authoritative sequence after real-data baseline adoption

1. **0.9.28 — External-tool scientific concordance**
   - compare canonical popgenVCF results with established implementations such as PLINK 2, SNPRelate, hierfstat, adegenet, and practical AMOVA references;
   - publish machine-readable comparisons, explicit tolerance profiles, methods, commands, logs, and approval-ready baseline proposals.
2. **0.9.29 — Continuous release benchmarking**
   - measure runtime, peak memory, thread scaling, dataset tiers, historical regressions, and configurable performance budgets;
   - publish deterministic JSON, TSV, and Markdown benchmark evidence and block releases only for confirmed regressions.
3. **0.9.30 — Documentation, metadata, and archival readiness**
   - complete pkgdown tutorials, interpretation guides, publication gallery, citation and software metadata, reproducibility statements, GHCR and Apptainer usage, HPC guidance, Zenodo configuration, DOI-ready metadata, SBOM, checksum, and provenance instructions.
4. **0.9.31 — 0.10.0 release-candidate closure**
   - synchronize DESCRIPTION, NEWS, roadmap, and trackers;
   - regenerate documentation, run full canonical validation and benchmarking, issue the real release certificate, validate container and Apptainer artifacts, assemble archives, run source and distribution install tests, and produce a reviewer-ready release-readiness report;
   - tag and publish 0.10.0 only after every required gate passes.

### Validation datasets and benchmarking

- [x] retain tiny synthetic fixtures in every CI run;
- [x] adopt a licensed, checksum-pinned canonical dataset under an approved registry entry;
- [ ] approve and retain the first production quantitative baseline snapshot from the canonical real dataset;
- [ ] publish complete external-tool scientific concordance evidence;
- [ ] publish runtime, memory, scaling, and historical regression artifacts per release;
- [ ] publish or externally host a checksum-pinned medium or large benchmark tier when licensing and storage policy permit.

### Documentation and metadata

- [ ] pkgdown website, tutorials, interpretation guides, and figure gallery;
- [ ] `CITATION.cff`, `codemeta.json`, and reproducibility statement;
- [ ] GHCR usage, Apptainer definition, and HPC guidance;
- [ ] Zenodo configuration and DOI-ready archive metadata;
- [ ] SBOM, checksums, and provenance instructions.

## Open tracking issues and deferred enhancements

- **#4 — publication-quality platform:** umbrella tracker for remaining 0.10 and 1.0 release work.
- **#20 — Core publication artifact contracts:** IBS/MDS is complete; retain only for any genuinely remaining publication-artifact work and close when its acceptance criteria are fully satisfied.
- **#22 — Canonical real-data validation:** tracks production real-data baselines, expected values, external-tool comparisons, and full-validation workflows beyond the completed dataset adoption infrastructure.
- **#24 — Unified ancestry platform:** covers backend/runtime enhancements beyond the completed publication ancestry contract.
- **#43 — Continuous scientific benchmarks:** tracks historical regression archives, cross-tool comparisons, resource/scaling measurements, dashboards, and release benchmark artifacts.
- **#68 — Analysis-specific publication narratives:** covers remaining module-specific methods, legends, citations, and supplementary narratives.

The reproducibility and release-infrastructure tracker **#1** remains open until documentation, metadata, and release automation are complete.

## 1.0: stable scientific release

Release 1.0 requires stable CLI, YAML, R API, module and output contracts; validated core modules and canonical real-data results; a complete report engine; validated container and Apptainer artifacts; complete documentation and citation metadata; and reproducible release artifacts with checksums, SBOM, and provenance.

## Beyond 1.0

Potential post-1.0 work includes selection scans, genomic landscapes, spatial resistance models, GWAS interoperability, community plugins, interactive exploration, optional Docker Hub publication, and cloud/workflow-platform execution.
