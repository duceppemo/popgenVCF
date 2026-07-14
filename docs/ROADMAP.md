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