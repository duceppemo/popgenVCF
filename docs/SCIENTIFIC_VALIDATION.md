# Scientific Validation

## Purpose

Scientific validation establishes that popgenVCF computes the intended quantity, preserves sample and marker identity, and agrees with an independent reference within a justified tolerance.

Passing package tests alone is not sufficient evidence of numerical correctness.

## Validation hierarchy

Each numerical module should use the strongest feasible combination of:

1. **Analytical fixtures** — tiny datasets with hand-derived expected values.
2. **Independent implementations** — trusted packages or external programs using the same estimator.
3. **Structural invariants** — symmetry, normalization, eigen-equation residuals, boundedness, and conservation properties.
4. **Real reference datasets** — stable public datasets with versioned expected outputs.
5. **Cross-version regression tests** — canonical outputs compared across releases.

A reference is not independent when it calls the same internal implementation or derives expected values from the result under test.

## Dataset tiers

### Tier 1: deterministic synthetic fixtures

Small enough for every CI run. These fixtures exercise missingness, heterozygosity, allele frequencies, LD, IBS, PCA, FST, cluster-label switching, and clearly separated DAPC classes.

### Tier 2: canonical real dataset

A public, redistributable dataset with a representative number of samples and thousands of SNPs. It supplies documentation figures, integration validation, realistic edge cases, and cross-tool comparisons.

### Tier 3: performance benchmark dataset

A larger dataset stored outside the Git repository, identified by immutable checksum and version. It measures runtime, peak memory, parallel scaling, and output stability.

## Required validation record

Each module validation artifact should record:

- module and estimator name;
- fixture or dataset identifier and checksum;
- popgenVCF version and source revision;
- reference implementation and version;
- parameter values;
- comparison metric and tolerance;
- observed difference;
- pass/fail status;
- platform and session information.

## Numerical tolerances

Tolerances must be selected from the mathematical and computational properties of the comparison. They may not be loosened merely to make a test pass.

Exact identity is preferred for discrete selections, sample order, marker IDs, and deterministic labels after alignment. Floating-point comparisons should report both absolute and, where useful, relative error.

Subspace methods require appropriate comparisons. Eigenvectors are sign-indeterminate and repeated eigenvalues may rotate an eigenspace; validation should use canonical correlations, projection matrices, or eigen-equation residuals rather than raw vector equality.

## Current validated contracts

The core suite validates:

- alternate allele frequency, MAF, and missingness;
- observed heterozygosity;
- SNPRelate frequency and missingness compatibility;
- the exact LD-retained marker set;
- IBS against an independent hand calculation;
- MDS eigenspace equivalence;
- PCA eigen-equation residuals;
- population diversity summaries;
- SNPRelate global and pairwise FST consistency.

The population-structure suite validates:

- label-switching-aware Q-matrix alignment;
- replicate membership reproducibility;
- row-normalized membership matrices;
- method-specific K-selection direction;
- DAPC classification on a strongly separated synthetic dataset.

## External-program validation

ADMIXTURE, fastStructure, sNMF, PLINK, and similar engines must be validated with explicit sample-order files and captured command lines. Replicate comparisons must align exchangeable cluster labels before measuring disagreement.

## Failure policy

A failed scientific validation blocks release. The response must diagnose whether the failure originates from implementation, version compatibility, sample/marker order, estimator mismatch, numerical tolerance, or an invalid reference assumption.

Cross-method disagreement may be retained as a transparent diagnostic when methods estimate different quantities. It must not be represented as exact equivalence.

## Adding a new module

A pull request adding a numerical module must include:

- a written estimator definition and references;
- a deterministic fixture;
- an independent expected result;
- tests for invalid and boundary inputs;
- result invariants;
- a machine-readable validation record;
- documentation of known differences from alternative methods.