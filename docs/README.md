# popgenVCF Project Documentation

This directory is the living blueprint for popgenVCF. Documents are normative unless explicitly described as informational.

## Governance and direction

- [Project Charter](PROJECT_CHARTER.md) — mission, principles, scientific completion standard, and release standard.
- [Development Roadmap](ROADMAP.md) — current delivery plan from the validated 0.8 foundation through the 0.10 deterministic-execution programme and the 1.0 release criteria.

## Technical design

- [Architecture](ARCHITECTURE.md) — shared analysis state, registry, module contract, artifact model, deterministic execution runtime, and external-engine boundaries.
- [Scientific Validation](SCIENTIFIC_VALIDATION.md) — validation hierarchy, dataset tiers, tolerances, and failure policy.
- [Scientific Review Assignment](SCIENTIFIC_REVIEW_ASSIGNMENT.md) — assigned reviewer, executable review-packet workflow, scientific checklists, decision record, and release-evidence return path.
- [Execution Timeouts](execution-timeouts.md) — elapsed-time policies, timeout safety boundaries, retry interaction, and recorded metadata.

## Contributor guidance

- [Development Guide](DEVELOPMENT_GUIDE.md) — workflow, module checklist, testing strategy, performance, compatibility, and review expectations.
- [Style Guide](STYLE_GUIDE.md) — code, configuration, terminology, tables, figures, reports, and provenance conventions.
- [Contributing](../CONTRIBUTING.md) — contribution process and pull-request checklist.
- [Code of Conduct](../CODE_OF_CONDUCT.md) — expected community conduct.

## Decision hierarchy

When documents appear to conflict, apply them in this order:

1. scientific correctness and the Project Charter;
2. explicit validation contracts and canonical estimator definitions;
3. stable public interfaces and release policy;
4. architecture and artifact contracts;
5. development and style conventions;
6. roadmap scheduling.

A roadmap item never overrides validation or correctness requirements.

## Updating these documents

Substantive changes should be made through a pull request that explains:

- why the policy or architecture must change;
- scientific and compatibility implications;
- migration requirements;
- which tests, validation artifacts, or workflows must change with it.

The documentation should evolve with the implementation. A feature is not complete when its governing documentation remains inaccurate.
