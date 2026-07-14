# Development roadmap

## 0.3 foundation

- Complete project rename and migration layer.
- Freeze public API, configuration schema, and analysis-object schema.
- Establish synthetic fixtures, contract tests, CI, containers, and workflow wrappers.
- Add benchmark and numerical-validation framework.

## 0.4 core numerical validation

- Cross-check PCA and LD sets against PLINK 2.
- Cross-check FST and diversity statistics against independent implementations.
- Add deterministic golden-output tests and performance benchmarks.
- Add resumable stage-level caching.

## 0.5 structure and spatial genetics

- Harden DAPC cross-validation.
- Validate ADMIXTURE integration and CV parsing.
- Add sNMF support.
- Expand AMOVA, Mantel, and isolation-by-distance diagnostics.

## 0.6 publication system

- Stable HTML, PDF, and DOCX report schemas.
- Manuscript and supplementary table generators.
- Journal-oriented figure presets and accessible palettes.

## 1.0

- Stable scientific API and file schemas.
- Published benchmark dataset and expected results.
- Bioconda recipe, release containers, complete user manual, and reproducible example workflow.
