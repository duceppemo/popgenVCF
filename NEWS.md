# popgenVCF 0.10.0 development

- Added canonical backend-independent ancestry replicate and result contracts for ADMIXTURE, fastStructure, and sNMF.
- Enforced sample identity, Q-matrix simplex constraints, replicate uniqueness, fit metrics, convergence, runtime, and provenance.
- Added stable replicate-summary and long-form Q-matrix tables for downstream consensus, plotting, and reporting.
- Added Hungarian ancestry-replicate alignment with correlation, cosine similarity, permutation matrices, RMSD, and alignment diagnostics.
- Added consensus ancestry estimates with mean and median Q matrices, empirical confidence intervals, per-cell variance, cluster stability, sample uncertainty, and global replicate stability.
- Added automatic backend-aware K selection with replicate intervals, plateau detection, stability weighting, cross-backend voting, recommendation confidence, and manuscript-ready text.
- Added a unified ancestry backend plugin contract, runtime backend discovery, deterministic task scheduling, canonical parsing, execution records, and `run_ancestry()` adapters for ADMIXTURE, fastStructure, and sNMF.
- Added backend-neutral ancestry publication artifacts with metadata-optional ancestry barplots, uncertainty and stability diagnostics, K-selection curves, PDF/SVG/PNG figures, figure source tables, manuscript text, captions, validation records, and strict artifact manifests.
- Added validated canonical result contracts for PCA, IBS/MDS, neighbour-joining trees, diversity, FST, AMOVA, DAPC, and isolation by distance, including provenance, parameters, metadata, validation, artifact manifests, stable table accessors, legacy adapters, and deterministic RDS serialization.
- Added a unified Quarto report engine with deterministic section discovery and ordering, canonical result tables, reproducibility metadata, report plans, section manifests, artifact registration, source-only operation without Quarto, and optional HTML/PDF rendering.

# popgenVCF 0.9.0 development

- Completed migration of every built-in analysis to `PopgenVCFModuleSpec` descriptors; the default registry is now fully descriptor-driven.
- Added descriptors for diversity, neighbour-joining trees, ADMIXTURE, fastStructure, sNMF, and chromosome-specific analyses while preserving all existing contracts and behavior.
- Migrated FST registration to a self-contained `fst_module_spec()` descriptor while preserving Weir-Cockerham estimates, confidence intervals, output schemas, and population-metadata requirements.
- Migrated IBS/MDS registration to a self-contained `ibs_module_spec()` descriptor while preserving VCF-only execution and downstream tree/IBD dependencies.
- Migrated PCA registration to a self-contained `pca_module_spec()` descriptor that owns execution, validation, references, resources, outputs, and publication artifacts.
- Added first-class VCF-only execution with PCA and IBS/MDS driven directly by VCF sample IDs.
- Made `sample` the only mandatory metadata column and retained arbitrary additional columns.
- Added exact metadata-to-VCF sample identity validation, VCF-order alignment, and a sample matching report.
- Added capability-driven module selection for VCF-only, sample annotation, population, and spatial metadata modes.
- Added `analysis_capabilities.tsv` with explicit availability and skip reasons for every registered module.
- Added transparent support for `.vcf` and `.vcf.gz` inputs.
- Added automatic reuse or creation of Tabix/CSI indexes for BGZF-compressed VCF files.
- Added cached `bcftools sort -Oz` normalization for plain VCF, ordinary gzip, unsorted, and read-only compressed inputs.
- Added `write_pca_publication_artifacts()` as the first publication-output implementation.
- Added canonical PCA coordinate and variance tables, PDF/SVG/PNG scatterplots, manuscript methods text, captions, figure source data, and validation records.
- Added strict sample, eigenvalue, variance, and artifact existence validation for PCA publication outputs.
- Added opt-in registry declarations for required module artifacts.
- Enforced artifact namespaces, required identifiers, optional file existence, and duplicate-free accumulation after module execution.
- Added the combined artifact manifest to `execute_analysis_registry()` output while preserving legacy modules with no declarations.
- Added canonical `PopgenVCFArtifact` and `PopgenVCFArtifactManifest` contracts.
- Added stable artifact identifiers, publication-time file existence checks, duplicate detection, and machine-readable manifest tables.
- Added tests and documentation for the artifact API that will underpin report, supplementary, validation, and provenance outputs.

# popgenVCF 0.8.3

- Replaced the unsatisfiable all-in-one Conda environment with a solvable core environment.
- Install SNPRelate, gdsfmt, and LEA through BiocManager matched to the active R version.
- Isolated fastStructure in an optional Python 3.10 environment and source installer.
- Removed the nonexistent texlive-inconsolata Conda dependency; TeX is now documented as a system/container layer.
- Added resolved-environment export and verification guidance.

# popgenVCF 0.8.3
