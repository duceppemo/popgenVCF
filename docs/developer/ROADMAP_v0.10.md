# popgenVCF development roadmap toward v0.10

## 1. Vision

`popgenVCF` is intended to become a reproducible, publication-oriented population-genomics platform built around VCF-derived data and explicit scientific contracts.

The project exists because population-genetics analyses are often assembled as fragile collections of scripts, package-specific objects, manually edited plots, and undocumented intermediate files. That approach can produce valid results, but it makes analyses difficult to audit, reproduce, compare, resume, publish, and extend.

The long-term goal is not to replace established scientific packages. Packages such as `adegenet`, `SNPRelate`, `hierfstat`, `dartR`, `LEA`, and related tools provide mature algorithms and domain-specific capabilities. `popgenVCF` is designed to coordinate such capabilities behind stable interfaces and to preserve the full analytical context around them.

What differentiates `popgenVCF` is the layer it builds above individual algorithms:

- canonical, backend-independent result contracts;
- immutable analysis and project objects;
- deterministic serialization and reporting;
- optional but auditable metadata integration;
- explicit artifact lineage and provenance DAGs;
- scientific regression testing and release certification;
- FAIR research-object export;
- publication-ready narratives, citations, bundles, and manuscript assets;
- a future dependency-aware execution engine capable of replay, caching, resume, and distributed execution.

The intended end state is a system in which an analysis can be executed, inspected, compared, reproduced, partially replayed, packaged for publication, and independently validated without relying on undocumented session state.

## 2. Design principles

### Canonical result contracts

Each analysis should return a stable, validated result object whose scientific meaning does not depend on a plotting function, transient environment, or a specific backend implementation.

### Immutable analysis objects

Completed analyses, artifacts, project records, and provenance entities should be treated as immutable records. New work creates new records rather than silently changing historical state.

### Metadata optional

Core analyses must remain usable with VCF sample identities alone. Rich metadata should improve labels, grouping, interpretation, reporting, and publication without becoming an unnecessary prerequisite.

### Backend abstraction

Scientific algorithms may be supplied by different packages or implementations. Public contracts should isolate users and downstream components from backend-specific object structures whenever practical.

### Deterministic outputs

Equivalent inputs and parameters should produce stable tables, manifests, identifiers, checksums, narrative text, file layouts, and serialized objects, subject to explicitly documented stochastic behavior.

### Reproducibility first

Inputs, parameters, software identity, random-number streams, execution records, artifacts, and derived results should be captured as part of the analysis rather than reconstructed afterward.

### FAIR by default

Projects should be findable, accessible, interoperable, and reusable through stable identifiers, machine-readable metadata, explicit licensing, creator identities, provenance links, and portable research-object exports.

### Publication-oriented

Scientific outputs should flow naturally into methods, captions, tables, supplementary materials, bibliographies, manuscript sections, and submission packages.

### Scientific validation before optimization

Correctness, invariants, reproducibility, and regression protection take priority over speed. Performance optimization should follow measurement and must preserve validated scientific behavior.

### Conservative scientific interpretation

The software should report what was computed, how it was computed, and the limits of the analysis. It should not overstate biological interpretation or convert exploratory outputs into unsupported conclusions.

## 3. Completed milestones

The phase labels below summarize the major architectural progression through Phase 7.2. Individual pull requests and release notes remain the authoritative implementation record.

### Phase 0 — Project foundation

Established the package structure, development conventions, core utilities, documentation layout, testing foundation, and initial VCF-oriented workflows.

### Phase 1 — Input, metadata, and quality-control foundations

Built the ingestion and validation layer for VCF/GDS data, sample matching, optional metadata, filtering, quality-control summaries, and stable input handling.

### Phase 2 — Canonical population-genetic analyses

Introduced standardized analysis interfaces and canonical outputs for core exploratory and population-genetic methods, including ordination, relatedness, distance, trees, diversity, differentiation, and population structure.

### Phase 3 — Reporting and visualization contracts

Separated scientific results from presentation, standardized tables and figures, improved metadata-aware labeling, and created repeatable reporting outputs.

### Phase 4 — Backend abstraction and scientific hardening

Reduced coupling to package-specific objects, formalized validation rules and invariants, strengthened deterministic behavior, and expanded scientific regression coverage.

### Phase 5 — Validation, benchmarking, and release confidence

Created scientific validation suites, performance records, stable benchmark contracts, and release-oriented checks designed to detect meaningful changes before publication.

### Phase 5.6 — Scientific regression archives and certification

Added append-only release benchmark archives, comparison reports, automated release archive workflows, golden-output regression stores, checksum verification, and gating rules for scientific regressions.

### Phase 6 — Reproducible project architecture

Introduced portable `PopgenVCFProject` objects and `.popgenvcf` bundles containing software identity, inputs, parameters, random-number state, canonical results, artifacts, reports, and integrity manifests.

### Phase 6.2 — Provenance DAGs and canonical sample identity

Added validated acyclic provenance graphs, producer/consumer relationships, stable node and edge tables, canonical sample identities, aliases, display ordering, and richer identity mappings while preserving immutable VCF sample keys.

### Phase 6.3 — Immutable artifact lineage

Made generated artifacts first-class immutable provenance entities with content hashes, unique producers, consumer relationships, lineage validation, and export to TSV, JSON, GraphML, and DOT.

### Phase 6.4 — FAIR research-object export

Added RO-Crate, CodeMeta, DataCite JSON, `CITATION.cff`, ORCID-aware creators, license propagation, stable project and artifact identifiers, and validated FAIR bundles integrated with `.popgenvcf` projects.

### Phase 7 — Publication companion foundation

Created deterministic publication bundles containing methods, software records, parameters, captions, citation manifests, artifact manifests, checksums, supplementary structures, and optional FAIR and project materials.

### Phase 7.2 — Analysis-specific publication narratives

Added deterministic analysis-specific methods text, legends, citation keys, and BibTeX support for PCA, IBS/MDS, neighbour-joining trees, diversity, FST, AMOVA, DAPC, isolation by distance, and ancestry analyses. These narratives are integrated into publication methods, captions, provenance, checksums, and publication bundles while preserving metadata-optional operation and conservative interpretation boundaries.

## 4. Current work

### Phase 7.3 — Automatic manuscript generation

The current objective is to assemble validated project and publication components into a coherent manuscript source.

Expected capabilities include:

- deterministic manuscript structure;
- automatic assembly of title-page metadata and author information;
- methods sections generated from executed analyses;
- insertion of result tables, figures, captions, and cross-references;
- bibliography and citation integration;
- standard declarations, data-availability text, software-availability text, and reproducibility statements;
- supplementary-material indexes;
- clear separation between generated factual content and author-supplied scientific interpretation;
- source formats suitable for later rendering to journal and office-document formats.

## 5. Planned roadmap

### Short-term

#### Manuscript assembly

Complete Phase 7.3 by generating a structured manuscript from canonical project records and publication bundles.

#### Submission packages

Produce deterministic submission directories containing manuscript sources, figures, tables, supplementary files, metadata, checksums, and journal-required supporting documents.

#### CSL support

Support Citation Style Language files for journal-specific citation and bibliography rendering without changing canonical citation identities.

#### JATS

Generate Journal Article Tag Suite XML suitable for archival, publisher, and repository workflows.

#### DOCX

Provide high-quality Word output with stable headings, captions, tables, references, and author-editable narrative sections.

### Medium-term

#### Workflow execution engine

Introduce an explicit execution layer that turns analysis definitions and provenance dependencies into runnable workflows.

#### Pipeline resume

Resume interrupted workflows from validated completed nodes rather than restarting the full analysis.

#### Incremental recomputation

Invalidate and recompute only nodes downstream of changed inputs, parameters, software identities, or analysis definitions.

#### Parallel scheduling

Schedule independent workflow nodes concurrently while preserving deterministic dependency resolution, logging, provenance, and reproducibility.

### Long-term

#### Cloud execution

Support portable execution in managed cloud environments with immutable inputs, containers, remote artifact stores, and reproducible job records.

#### HPC execution

Integrate with schedulers such as SLURM and other batch systems for dependency-aware execution across compute clusters.

#### Plugin ecosystem

Allow external developers to contribute analyses, result contracts, validators, narratives, plots, and workflow nodes without modifying the core package.

#### Interactive desktop and web interfaces

Provide user interfaces for project construction, execution, inspection, visualization, provenance exploration, comparison, and publication export while retaining scriptable and headless operation.

## 6. Deferred ideas

Deferred items are intentional architectural opportunities, not abandoned commitments. They should be implemented only when their scientific contracts and integration points are sufficiently mature.

### Sample identity

Implemented:

- immutable VCF/GDS sample keys;
- optional aliases;
- canonical public display names;
- individual, family, replicate, and display-order concepts;
- validation of identity collisions and mappings.

Still planned:

- hierarchical populations;
- sampling sites and geographic sampling structures;
- experimental groups and treatment designs;
- arbitrary display and annotation metadata;
- ontology mapping for samples, locations, traits, and experimental terms;
- explicit many-to-many grouping models where scientifically justified.

### Provenance

Implemented:

- validated provenance DAGs;
- immutable nodes and edges;
- producer-to-artifact-to-consumer lineage;
- content hashes;
- GraphML, DOT, JSON, and tabular exports.

Future:

- interactive DAG viewer;
- provenance query language;
- artifact replay;
- branch-level analysis replay;
- provenance-aware debugging and explanation;
- queries such as “which inputs and parameters produced this figure?” and “what becomes invalid if this parameter changes?”.

### Publication

Implemented:

- generated methods;
- analysis-specific narratives;
- captions and legends;
- software and parameter records;
- bibliography and BibTeX output;
- checksummed publication bundles.

Future:

- complete manuscript generation;
- journal templates;
- journal-specific required statements and reporting checklists;
- reviewer-response generator;
- revision diff reports;
- tracked regeneration of only affected manuscript sections;
- submission-system metadata exports.

### Benchmarking

Implemented foundations include scientific validation suites, release archives, golden outputs, checksums, and regression comparisons.

Future:

- larger public benchmark datasets;
- CI performance dashboards;
- memory-scaling plots;
- algorithm and backend comparisons;
- hardware-normalized performance records;
- long-term trend detection;
- benchmark certification across supported releases and platforms.

### Workflow engine

A dependency-aware workflow engine is one of the largest ideas intentionally postponed while result, project, artifact, and provenance contracts matured.

Planned capabilities include:

- dependency scheduler;
- automatic caching;
- partial reruns;
- analysis replay engine capable of rerunning only one branch of the provenance graph;
- incremental recomputation after parameter or input changes;
- resumable execution;
- local parallel execution;
- distributed execution;
- failure isolation and retry policies;
- deterministic execution plans;
- dry-run and impact-analysis modes.

### Artifact cache

Introduce a cache keyed by immutable content and execution hashes. Equivalent inputs, parameters, software identities, and analysis definitions could reuse validated artifacts across runs or projects.

The cache must include strict validation, provenance preservation, privacy controls, eviction policies, and protections against inappropriate reuse across incompatible environments.

### Plugin SDK

Develop a supported extension contract through which external packages can register:

- analysis definitions;
- parameter schemas;
- canonical result classes;
- validators;
- provenance records;
- serialization rules;
- plots and tables;
- publication narratives;
- workflow dependencies;
- benchmark and golden-output tests.

Plugins should be versioned, discoverable, isolated from core internals, and capable of declaring compatibility requirements.

### Interactive visualization

Potential future work:

- Shiny applications;
- Quarto dashboards;
- browser-based project and provenance explorers;
- plot editing with reproducible edit records;
- brushing and linking across ordinations, metadata, tables, and maps;
- interactive trees;
- interactive ancestry and assignment displays;
- scalable exploration of large provenance graphs.

Interactive features must remain views over canonical objects rather than becoming alternate sources of scientific state.

### Machine learning

Ideas intentionally postponed include:

- automatic outlier detection;
- population assignment;
- anomaly detection;
- QC recommendations;
- model-assisted parameter suggestions;
- automated detection of suspicious sample or population patterns.

These features require carefully defined training data, uncertainty reporting, bias evaluation, interpretability, and safeguards against replacing scientific judgment with opaque recommendations.

### Reporting

Future reporting capabilities may include:

- automatic abstracts based on validated manuscript content;
- graphical abstract generation;
- PowerPoint export;
- reviewer packages;
- executive and non-technical summaries;
- change reports that explain why results changed, not only that their hashes differ;
- cross-project synthesis reports.

Generated summaries must distinguish computed facts from model-generated language and remain traceable to their source objects.

### Analysis comparison and change explanation

Current project comparison can identify changed identities, inputs, and result digests. A future explanation layer should determine the causal path of a change.

Examples include:

- identifying the earliest changed provenance node;
- separating input, parameter, software, stochastic, and formatting changes;
- listing downstream artifacts affected by each change;
- comparing canonical result components with analysis-aware semantics;
- generating human-readable revision reports for collaborators, reviewers, and releases.

### Journal-specific manuscript polish

Beyond formatting, journal profiles should eventually encode requirements such as:

- mandatory declarations;
- data and code availability wording;
- ethics and permits statements;
- author-contribution taxonomies;
- reporting checklists;
- figure and table constraints;
- supplementary naming rules;
- anonymized review requirements;
- repository and accession expectations.

The system should validate completeness without inventing missing scientific or administrative facts.

## 7. Stretch goals

These goals would make `popgenVCF` substantially different from conventional collections of analysis functions.

### Workflow replay

Select any validated node or branch in the provenance graph and reproduce it from its recorded dependencies, parameters, software identity, and random-number state.

### Executable publications

Package manuscript claims, methods, canonical results, figures, tables, provenance, code identity, and replay instructions into a portable research object that can be independently inspected and re-executed.

### FAIR-first analyses

Treat FAIR metadata and stable identifiers as native products of every project rather than optional metadata added at deposition time.

### Benchmark certification

Certify releases against curated scientific datasets, golden outputs, platform matrices, performance envelopes, and reproducibility requirements.

### Continuous reproducibility monitoring

Continuously verify that supported environments can reopen projects, validate artifacts, reproduce selected analysis branches, rebuild publication outputs, and explain any divergence.

### Causal reproducibility reports

Generate reports that connect a changed input, parameter, package version, or algorithm to every affected downstream result, artifact, figure, table, and manuscript statement.

### Cross-project artifact reuse

Safely reuse immutable, validated artifacts across compatible projects through a content-addressed cache while preserving provenance and privacy boundaries.

### Reproducible interactive analysis

Allow interactive exploration, selection, filtering, and presentation edits while recording each action as a deterministic, replayable transformation over canonical objects.

## Roadmap status

This roadmap describes direction rather than a fixed release contract. Priorities may change as scientific validation, user needs, package dependencies, and architectural constraints evolve. Features should advance only when they can preserve the core principles of canonical contracts, deterministic outputs, immutable lineage, reproducibility, and conservative scientific interpretation.
