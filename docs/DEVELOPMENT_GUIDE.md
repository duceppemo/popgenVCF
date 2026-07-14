# Development Guide

## Development workflow

1. Create a focused branch from `main`.
2. Make the smallest coherent scientific or engineering change.
3. Add tests and validation before treating the implementation as complete.
4. Run local package checks and relevant numerical suites.
5. Open a pull request explaining rationale, assumptions, compatibility, and evidence.
6. Merge only after required CI jobs are green.

## Local setup

Use the documented Conda/Mamba environment or the published GHCR image. Do not add automatic dependency installation to analysis functions.

Typical local checks:

```bash
R CMD build .
R CMD check --as-cran popgenVCF_*.tar.gz
```

```r
core <- popgenVCF::run_scientific_validation(
  integration = TRUE,
  threads = 4
)
structure <- popgenVCF::run_population_structure_validation(
  integration = TRUE
)
stopifnot(core$passed, structure$passed)
```

## Code organization

- `R/`: package implementation and module adapters.
- `tests/testthat/`: unit, contract, regression, and deterministic numerical tests.
- `validation/`: independent reference runners and benchmark definitions.
- `inst/extdata/`: tiny redistributable fixtures.
- `inst/conda/`: human-editable environment specifications.
- `inst/scripts/`: installed launchers and environment utilities.
- `docs/`: architecture, governance, validation, roadmap, and style policies.
- `.github/workflows/`: CI, validation, documentation, coverage, container, and release automation.

## Module development checklist

A new or substantially changed module must define:

- stable module identifier;
- prerequisites and registry dependencies;
- configuration fields with validation and defaults;
- state inputs and sample/marker-order requirements;
- scientific result object;
- result validator and invariants;
- unit and integration tests;
- independent numerical validation;
- table, figure, methods, caption, and supplementary outputs;
- documentation and references;
- expected runtime and memory characteristics.

## Testing strategy

### Unit tests

Test pure functions, malformed inputs, boundary cases, sample ordering, missing data, and deterministic seeds.

### Contract tests

Verify registry declarations, dependency resolution, result schemas, output paths, and module validators.

### Numerical tests

Compare against an independent reference with explicit metrics and tolerances. Preserve exact sample and marker identifiers in diagnostics.

### Integration tests

Exercise the installed package and supported external engines in clean environments. Expensive tests may be separated from pull-request tests but must run before release.

### Regression tests

Canonical outputs should be versioned or summarized by stable hashes and numerical metrics. Intentional scientific changes require updated expectations and a NEWS entry.

## Randomness

All stochastic functions accept or derive explicit seeds. Replicates receive deterministic, non-overlapping seeds. Seeds and engine versions are recorded in provenance.

## Performance

Profile before optimizing. Prefer vectorized or compiled package operations when they preserve clarity. Avoid repeated VCF reads, repeated GDS conversion, duplicate dosage matrices, and uncontrolled nested parallelism.

Performance changes require correctness tests and, for important paths, benchmark evidence.

## Documentation

Exported functions require complete R documentation. Statistical modules require mathematical definitions, assumptions, interpretation guidance, references, and examples.

Generated methods text must state the estimator and implementation accurately and must not imply biological conclusions that the analysis does not establish.

## Compatibility

Before 1.0, breaking changes may be accepted to establish coherent interfaces. They must be documented in `NEWS.md` and migration guidance. After 1.0, public configuration, CLI, R API, and canonical output schemas follow semantic-versioning expectations.

## Review expectations

Reviewers should ask:

- Is the scientific quantity defined correctly?
- Is the reference comparison genuinely independent?
- Are sample and marker identities preserved?
- Are tolerances justified?
- Are outputs derived from canonical results?
- Are failures actionable and explicit?
- Does the change align with the project charter?