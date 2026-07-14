# Architecture

## Overview

popgenVCF is an R package, command-line application, and reproducible workflow engine for population-genomic analyses. Its architecture is centered on one shared `PopgenVCFAnalysis` state object and a registry of analysis modules.

```text
VCF + metadata + YAML
          |
          v
  input validation
          |
          v
PopgenVCFAnalysis state
          |
          v
 analysis registry and dependency graph
          |
  +-------+--------+--------+---------+
  |       |        |        |         |
  QC   diversity  PCA/IBS   FST   structure ...
  |       |        |        |         |
  +-------+--------+--------+---------+
          |
          v
 tables + figures + methods + report + provenance
```

## Architectural boundaries

### Input layer

Responsible for configuration parsing, schema validation, VCF/GDS conversion, metadata validation, sample-order enforcement, checksums, and provenance capture.

### Analysis state

`PopgenVCFAnalysis` is the canonical state passed between modules. It stores configuration, validated sample metadata, reusable genotype representations, module results, output paths, diagnostics, provenance, and validation status.

Modules must not silently reload the VCF or independently reorder samples. Expensive reusable objects such as GDS handles, dosage matrices, allele-frequency summaries, and distance matrices should be computed once and referenced through the analysis state.

### Registry and dependency graph

Each analysis is registered with a stable identifier and declares:

- prerequisites and dependent modules;
- enablement rules;
- configuration schema;
- required state inputs;
- result validator;
- runner;
- table, figure, methods, caption, and supplementary generators.

The registry resolves dependencies before execution and rejects cycles, missing requirements, incompatible configurations, and invalid outputs.

### Output layer

Canonical numerical results are machine-readable. Presentation artifacts are derived from canonical results, never the reverse.

Recommended module output contract:

```text
results/<module>/<module>.rds
tables/<module>/<module>.tsv
tables/publication/Table_<n>_<module>.docx
figures/<module>/Figure_<n>_<name>.svg
figures/<module>/Figure_<n>_<name>.pdf
figures/<module>/Figure_<n>_<name>.png
supplementary/<module>/...
methods/<module>.md
validation/<module>.json
```

## Target module interface

The exact R interface may evolve before 1.0, but every module should conceptually implement:

```r
module_spec()
module_validate_config()
module_dependencies()
module_run()
module_validate_result()
module_tables()
module_figures()
module_methods()
module_captions()
module_supplementary()
```

A module runner returns scientific results and diagnostics, not side effects scattered across the filesystem. Artifact writers consume the validated result afterward.

## Scientific and exploratory separation

Formal estimators, hypothesis tests, and confidence intervals must be clearly separated from exploratory clustering, visual grouping, and interpretation aids. Examples:

- `1 - IBS` is an IBS-derived distance, not FST;
- PCA or DAPC clusters are not ancestry proportions;
- Q matrices from ADMIXTURE, fastStructure, and sNMF require explicit sample-order provenance;
- label alignment changes component labels, not membership values.

## External engines

External tools are adapters behind the same module contract. Each adapter must capture executable version, command line, exit status, input order, output checksums, seeds, and logs.

External engines must be optional unless required by the selected analysis. Their absence should produce an actionable configuration error rather than a partial or silently substituted analysis.

## Concurrency

Concurrency is allowed only where tasks are independent and deterministic. Thread limits must account for nested parallelism. SNPRelate operations use the validated safe thread cap. External replicate runs may execute concurrently when output directories and seeds are isolated.

## Error handling

Errors must identify the module, input, configuration field, and remediation. Scientific validation failures are fatal. Optional presentation failures may be reported separately only when canonical results remain valid and the release policy permits it.

## Extensibility

Third-party modules should eventually be registerable without editing the core pipeline. A public plugin API must include versioned contracts, namespace rules, configuration validation, artifact conventions, and compatibility checks.

## Non-goals before 1.0

- distributed execution inside the R package;
- a web application as the primary interface;
- automatic biological conclusions;
- silent conversion between incompatible estimators;
- preserving provisional APIs at the expense of a coherent 1.0 design.