# Contributing to popgenVCF

Thank you for helping improve popgenVCF. Contributions must align with the [Project Charter](docs/PROJECT_CHARTER.md): scientific correctness, reproducibility, transparency, and publication-ready outputs take priority over feature count.

## Before starting

For substantial scientific modules or public-interface changes, open an issue describing:

- the scientific question and estimator;
- assumptions and expected interpretation;
- proposed independent validation;
- expected inputs, outputs, and dependencies;
- compatibility and computational impact.

Small fixes and documentation improvements may go directly to a focused pull request.

## Development workflow

1. Create a focused branch from `main`.
2. Keep the change coherent and avoid unrelated formatting churn.
3. Add or update tests for every behavioral change.
4. Add independent numerical validation for new or changed estimators.
5. Update documentation, `NEWS.md`, and generated methods text when semantics change.
6. Run package checks and relevant validation suites.
7. Open a pull request using the checklist below.

See the [Development Guide](docs/DEVELOPMENT_GUIDE.md) and [Style Guide](docs/STYLE_GUIDE.md).

## Required local checks

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

Run the GHCR container smoke test or relevant external-engine test when the change affects containers, system dependencies, or external adapters.

## Scientific changes

A statistical change must document:

- exact estimator and methodological references;
- differences from existing implementations;
- sample and marker ordering behavior;
- missing-data behavior;
- random seeds and replicate strategy;
- comparison metric and justified tolerance;
- validation result and reference-tool version.

Do not loosen tolerances solely to make a test pass. Do not describe different estimators as equivalent when they target different quantities.

## Output requirements

Canonical outputs remain machine-readable. Publication tables, figures, reports, methods, and captions are derived products generated from canonical results.

New complete modules should follow the artifact contract described in [Architecture](docs/ARCHITECTURE.md).

## Dependency policy

- Do not install packages inside analysis functions.
- Keep optional dependencies optional unless the selected analysis requires them.
- Record external executable versions and command lines.
- Avoid adding heavy dependencies when a maintained existing dependency or clear base-R implementation is sufficient.
- Update Conda/container definitions when system requirements change.

## Pull-request checklist

- [ ] The scientific or engineering rationale is explained.
- [ ] Tests cover valid, invalid, and boundary behavior.
- [ ] Numerical changes have an independent reference and explicit tolerance.
- [ ] Sample and marker identity/order are preserved and tested.
- [ ] Documentation and `NEWS.md` are updated where needed.
- [ ] Canonical outputs remain machine-readable.
- [ ] Generated prose does not overstate biological interpretation.
- [ ] `R CMD check` and relevant validation suites pass.
- [ ] Performance and compatibility impacts are described.

## Review and conduct

Reviews focus on scientific semantics, reproducibility, maintainability, and user impact. All contributors must follow the project Code of Conduct.