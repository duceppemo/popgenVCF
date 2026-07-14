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
  eigendecomposition of SNPRelate's returned genetic covariance matrix.
- Retained standardized-dosage PCA as a non-gating cross-method diagnostic.
- Added a direct implementation of the Weir-Cockerham 1984 variance-component
  equations for global and pairwise FST validation.
- Retained hierfstat comparisons as diagnostic cross-tool results rather than
  assuming exact equality across multilocus aggregation implementations.
- Removed data.table shallow-copy warnings from PCA and MDS metadata joins.

# popgenVCF 0.7.1

- Ensured LD-pruned SNP identifiers are plain atomic vectors accepted by all
  supported SNPRelate versions.
- Added core validation for IBS, MDS, PCA, diversity, and FST.

# popgenVCF 0.6.3

- Normalized SNPRelate rate/frequency outputs using either `MissingRate` or
  `CallRate`.
- Represented the unbounded LD span with `.Machine$integer.max` at the API
  boundary to avoid integer overflow.

# popgenVCF 0.6.2

- Added version-tolerant optional-argument handling for SNPRelate APIs.

# popgenVCF 0.6.1

- Imported the data.table namespace and completed scientific-validation
  documentation.

# popgenVCF 0.6.0

- Added deterministic scientific-validation fixtures, expected outputs,
  tolerance policies, external-reference runners, and validation CI.

# popgenVCF 0.5.0

- Added enforceable analysis-module contracts with declared outputs, validators,
  references, resource classes, and contract versions.
- Module results are committed only after structural and numerical validation.
- Added validation records to the analysis object.
- Added deterministic synthetic-data performance benchmarks and CI artifacts.
- Added module-contract and numerical-validation policies and tests.

# popgenVCF 0.4.0

- Removed all pre-release backward-compatibility aliases and legacy classes.
- Added a dependency-aware analysis registry with topological execution.
- Replaced the hard-coded analysis orchestrator with registered modules.
- Moved repository foundation documentation out of the package top level.
- Replaced reserved `inst/exec` usage with `inst/scripts`.
- Removed unpublished repository URLs from package metadata.
- Fixed PDF-manual line-width diagnostics.

# popgenVCF 0.3.0

- Renamed the package, CLI, containers, workflow modules, documentation, and public branding from `popgenR` to `popgenVCF`.
- Introduced `PopgenVCFAnalysis` as the primary analysis-state class.
- Added `new_popgen_vcf_analysis()` and `is_popgen_vcf_analysis()` as the preferred state API.
- Added an API stability policy, migration guide, roadmap, contribution policy, code of conduct, citation metadata, installed executable, and tiny parser fixtures.
- Declared the 0.3 configuration schema, output schemas, and fixed QC contract as the foundation for numerical validation.

# popgenVCF 0.2.0

- Added the canonical `PopgenVCFAnalysis` S3 state object.
- Refactored the pipeline to validate and enrich one analysis state across modules.
- Added stage timings and structured pipeline messages to the saved state.
- Preserved standalone module functions and CLI compatibility.
- Updated reports and manifests to consume the new state schema.
- Added regression tests for state invariants and serialization.

# popgenVCF 0.1.2

- Removed Pandoc as a mandatory `R CMD build` dependency.
- Moved the architecture overview to installed static documentation.
- Manuscript report rendering still uses Pandoc when explicitly requested.

# popgenVCF 0.1.1

- Fixed command-line option normalization for all long options, including `--config`, `--force-gds`, and `--no-report`.
- Enforced the fixed LD/QC contract with a standard R warning when configured values are overridden.
- Removed non-ASCII characters from executable R source files.
- Declared data.table and ggplot2 non-standard evaluation symbols for clean static checks.
- Qualified base package functions reported by `R CMD check`.
- Removed unused package imports and corrected regression tests.

# popgenVCF 0.1.0

## Foundation release

- Added a stable YAML-driven command-line interface and R API.
- Added exact SNPRelate LD pruning with MAF from configuration, missing rate
  0.2, correlation threshold `sqrt(0.2)`, unlimited base-pair span, 50-SNP
  window, `start.pos = "first"`, and a four-thread cap.
- Added independently audited sample and variant QC reports.
- Added diversity, PCA, IBS/MDS, neighbour-joining, global and pairwise FST,
  DAPC, AMOVA, Mantel/IBD, ADMIXTURE CV, chromosome analyses, and block
  bootstrap modules.
- Added manuscript-oriented HTML reporting and machine-readable manifests.
- Added package documentation, testthat tests, GitHub Actions, Docker,
  Apptainer, and Nextflow infrastructure.
