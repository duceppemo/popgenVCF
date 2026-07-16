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
- Added an interactive dashboard layer with overview cards, searchable tables, optional Plotly PCA and ancestry views, transparent scientific-quality scoring, JSON/TSV provenance exports, and a compressed reproducibility bundle.
- Added first-class benchmark datasets, specifications, registries, results, and suites with numerical tolerances, runtime/memory/disk budgets, dependency-aware skips, deterministic execution, stable summaries, and serialization.
- Added a versioned benchmark dataset catalogue with embedded CI fixtures, local and remote sources, deterministic cache paths, SHA256 verification, atomic fetch-on-demand, offline reuse, filtering, resource annotations, and planned 1000 Genomes, HGDP, and HapMap reference subsets.
- Added systematic external-reference comparison contracts with exact, numeric, matrix, eigenspace, and label-switching-aware Q-matrix modes, dependency-aware skips, transparent equivalence versus diagnostic roles, and benchmark integration.
- Added a dependency-aware registry of established reference adapters for SNPRelate, PLINK 2, hierfstat, adegenet, poppr, pegas, vegan, ADMIXTURE, fastStructure, and LEA/sNMF, including tool-version discovery and CI-safe precomputed-output contracts.
- Added repeatable performance regression benchmarks with warmups, robust runtime summaries, approximate memory and temporary-disk accounting, thread-scaling diagnostics, machine fingerprints, baseline compatibility checks, configurable regression thresholds, gating and informational modes, stable tables, and baseline serialization.
- Added append-only scientific regression archives with canonical release records, component digests, provenance and environment metadata, release manifests, checksum verification, stable TSV/JSON/RDS exports, duplicate protection, and corruption detection.
- Added release-to-release comparison and Quarto regression reports plus GitHub Actions automation that downloads the latest published archive, appends the current immutable record, publishes workflow artifacts, and attaches checksummed archives and reports to tagged GitHub Releases.
- Added golden-output scientific regression stores with exact, tolerant numeric, matrix, eigenspace, label-switching-aware ancestry, and manifest comparison modes, explicit gating versus diagnostic roles, approved replacement metadata, SHA256 verification, and optional release-certification integration.
- Added canonical reproducible analysis projects with software and RNG identity, checksummed input records, embedded canonical results, portable `.popgenvcf` bundles, integrity verification, reopening without recomputation, and project-to-project change reports.
- Added optional unique metadata sample aliases while retaining immutable VCF IDs in `vcf_sample` provenance fields, and normalized unbounded LD windows to `.Machine$integer.max` before calling SNPRelate.
- Added validated provenance DAGs with deterministic lineage traversal and a canonical sample identity model supporting aliases, individuals, families, replicates, and explicit display ordering while preserving immutable VCF keys.
- Added immutable artifact lineage with checksummed execution and artifact identities, explicit producer/consumer relationships, content verification, project-bundle embedding, and TSV/JSON/GraphML/DOT exports.

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
