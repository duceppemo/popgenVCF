# popgenVCF Project Charter

## Mission

popgenVCF exists to provide a reproducible, scientifically validated, publication-quality population-genomics toolkit for diploid, biallelic SNP data stored in VCF files.

The project should enable researchers to move from validated input data to interpretable tables, figures, reports, methods text, and supplementary outputs through one auditable workflow.

> Scientifically correct. Reproducible by design. Publication ready by default.

## Vision

popgenVCF is not intended to replace every specialist population-genetics package. It provides a unified analysis engine that integrates trusted methods behind a consistent configuration, execution, validation, and reporting interface.

A complete analysis should be expressible as:

```bash
popgenVCF run analysis.yml
```

The resulting analysis must be traceable from the source VCF and metadata through every transformation, parameter, random seed, software version, result, figure, table, and narrative statement.

## Core principles

### 1. Scientific correctness over feature count

A smaller validated implementation is preferable to a broader unvalidated one. Statistical assumptions, estimators, limitations, and interpretation boundaries must be explicit.

### 2. Independent numerical validation

Every numerical analysis requires at least one independent reference: a hand-calculated fixture, a trusted external implementation, published reference values, or a separately derived algorithm. Agreement criteria must be encoded as tests.

### 3. Reproducibility by default

Analyses record configuration, software versions, package versions, source revision, random seeds, runtime environment, input checksums, and execution timestamps. Stochastic analyses must be reproducible from recorded seeds.

### 4. Publication-ready outputs

Every complete module must produce machine-readable results, publication-quality tables, vector and raster figures, methods text, captions, supplementary outputs, and provenance.

### 5. Deterministic and explicit workflows

The same inputs and configuration should produce the same results. Defaults must be documented. Silent parameter changes, implicit sample reordering, and hidden filtering are prohibited.

### 6. Transparent intermediate state

Intermediate results must remain inspectable and exportable. Formal statistical results must be separated from exploratory diagnostics and interpretation aids.

### 7. Modular architecture

Analyses are registry-managed modules with declared dependencies, inputs, outputs, validation rules, plotting functions, table generators, and methods generators. Modules reuse the shared analysis state instead of reloading or recomputing data unnecessarily.

### 8. Stable public interfaces

Before 1.0, interfaces may change when necessary to achieve a coherent design. Changes must be documented. After 1.0, configuration, CLI, and R API stability become release requirements.

### 9. Performance through sound design

Priority order is correctness, maintainability, then performance. Optimization must be measured and must not alter validated scientific results.

### 10. Automation of repeated scientific work

Repeated manual tasks should become deterministic software features, including methods generation, citations, captions, software inventories, supplementary tables, and release provenance.

### 11. The software should teach as well as analyze

Reports and documentation should explain what each statistic measures, what assumptions it makes, and what conclusions it does not support. Interpretation aids must never replace scientific judgment.

## Scientific completion standard

An analysis module is complete only when it has:

- a documented statistical definition and assumptions;
- an implementation with explicit inputs and outputs;
- unit tests and integration tests;
- deterministic validation fixtures;
- an independent numerical comparison;
- machine-readable and publication-ready tables;
- publication-quality SVG, PDF, and PNG figures where applicable;
- generated methods text and captions;
- CLI, YAML, and R API integration;
- user documentation, references, and interpretation guidance;
- provenance and validation artifacts.

## Release standard

A release must not be published unless required jobs pass:

- `R CMD check`;
- unit and integration tests;
- core scientific validation;
- population-structure validation;
- container build and smoke tests;
- documentation build;
- provenance and SBOM generation;
- release artifact checksum generation.

## Governance

Scientific claims and public interfaces require review proportional to their impact. Changes to numerical estimators, filtering semantics, sample ordering, or result interpretation require explicit validation updates.

The charter is a living project constitution. Changes should be proposed through a pull request that explains the scientific or engineering rationale.