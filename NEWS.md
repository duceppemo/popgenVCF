# popgenVCF 0.9.0 development

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

- Fixed label-switching assignment when cluster correlations are negative.
- Added finite-value validation before solving cluster assignments.
- Added regression tests for negative, shifted, and non-finite similarity matrices.

# popgenVCF 0.8.3

- Added a complete Conda/Mamba environment specification for R, Bioconductor, external population-structure engines, reporting tools, and development dependencies.
- Added an environment verification script and reproducibility/export instructions.
- Documented fastStructure Python 3.10 constraints and ADMIXTURE licensing considerations.
- Established the Conda specification as the dependency source for future Docker and Apptainer images.

# popgenVCF 0.8.0

- Added label-switching-aware membership comparison and replicate reproducibility metrics.
- Added validated DAPC membership outputs, multi-seed reproducibility, and K diagnostics.
- Added optional fastStructure and LEA/sNMF integrations with explicit input-order checks.
- Added deterministic population-structure validation fixtures.
- Added unified K-selection helpers and stronger structure-module contracts.

# popgenVCF 0.7.3

- Replaced fragile PCA eigenspace ordering validation with direct eigen-equation residual checks.
- Reclassified hand-calculated and hierfstat FST comparisons as transparent cross-method diagnostics.
- Added a gating FST internal-consistency check without weakening numerical tolerances.
- Added `pca_eigen_residuals()` and regression tests for PCA covariance consistency.

# popgenVCF 0.7.2

- Replaced the invalid assumption that SNPRelate PCA must equal a generic
  standardized-dosage SVD. PCA execution is now validated against an explicit
  eigen-equation residual check using the covariance matrix returned by SNPRelate.
- Reclassified hand-calculated and hierfstat FST comparisons as transparent
  cross-method diagnostics rather than exact-equivalence gates.
- Added a gating FST internal-consistency check without weakening numerical
  tolerances.
