# Development roadmap

## 0.5 validation-contract foundation

- Enforce module contracts, declared outputs, dependencies, references, resource
  classes, and validator functions.
- Record module validation status and metrics in every run.
- Establish deterministic benchmark tooling and benchmark CI artifacts.
- Freeze the registry and analysis-state architecture.

## 0.6 core numerical validation

- Cross-check QC and LD-pruned sets against PLINK 2.
- Cross-check PCA eigenvectors and eigenvalues against PLINK 2 and SNPRelate
  reference fixtures.
- Cross-check FST and diversity statistics against independent implementations.
- Add golden-output tests with explicit numerical tolerances.
- Add resumable stage-level caching and performance-regression thresholds.

## 0.7 population structure

- Harden DAPC cross-validation and convergence reporting.
- Validate ADMIXTURE execution, Q-matrix parsing, and CV selection.
- Add fastStructure and LEA/sNMF registry modules.

## 0.8 spatial and hierarchical genetics

- Extend AMOVA hierarchy and permutation diagnostics.
- Add partial Mantel and spatial autocorrelation analyses.
- Complete chromosome-block confidence intervals and hierarchical F statistics.

## 0.9 publication system

- Stabilize HTML, PDF, and DOCX report schemas.
- Generate manuscript and supplementary tables automatically.
- Add journal-oriented figure presets and accessible palettes.

## 1.0

- Stable scientific API and canonical output schemas.
- Published benchmark datasets and expected numerical results.
- Release containers, Bioconda recipe, complete manual, and reproducible
  end-to-end example workflow.
