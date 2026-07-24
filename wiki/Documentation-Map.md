# Documentation map

Each documentation surface has a distinct role.

| Surface | Role | Source |
| --- | --- | --- |
| README | Minimal project landing page | `README.md` |
| Wiki | Task-oriented user, validator, developer, and maintainer guides | `wiki/*.md` |
| pkgdown | Generated API reference and rendered vignettes | `man/`, `vignettes/`, `_pkgdown.yml` |
| Normative docs | Scientific, architecture, governance, release, and evidence contracts | `docs/` |
| Changelog | User-visible release and development changes | `NEWS.md` |
| Citation metadata | Machine-readable software identity | `CITATION.cff`, `codemeta.json`, `.zenodo.json` |

## Users

- [Getting Started](Getting-Started)
- [User Guide](User-Guide)
- [Configuration Reference](Configuration-Reference)
- [Results and Interpretation](Results-and-Interpretation)
- [Deployment and Troubleshooting](Deployment-and-Troubleshooting)
- [Rendered vignettes](https://duceppemo.github.io/popgenVCF/articles/)
- [API reference](https://duceppemo.github.io/popgenVCF/reference/)

## Validators

- [Validation and Scientific Review](Validation-and-Scientific-Review)
- [Scientific validation contract](https://github.com/duceppemo/popgenVCF/blob/main/docs/SCIENTIFIC_VALIDATION.md)
- [Scientific concordance](https://github.com/duceppemo/popgenVCF/blob/main/docs/SCIENTIFIC_CONCORDANCE.md)
- [Population structure validation](https://github.com/duceppemo/popgenVCF/blob/main/docs/POPULATION_STRUCTURE_VALIDATION.md)
- [Canonical real-data baselines](https://github.com/duceppemo/popgenVCF/blob/main/docs/CANONICAL_REAL_DATA_BASELINES.md)
- [Reviewer runbook](https://github.com/duceppemo/popgenVCF/blob/main/docs/SCIENTIFIC_REVIEW_ASSIGNMENT.md)

## Developers

- [Developer Guide](Developer-Guide)
- [Architecture](https://github.com/duceppemo/popgenVCF/blob/main/docs/ARCHITECTURE.md)
- [Development guide](https://github.com/duceppemo/popgenVCF/blob/main/docs/DEVELOPMENT_GUIDE.md)
- [Module contract](https://github.com/duceppemo/popgenVCF/blob/main/docs/MODULE_CONTRACT.md)
- [Public API contract](https://github.com/duceppemo/popgenVCF/blob/main/docs/PUBLIC_API_CONTRACT.md)
- [Style guide](https://github.com/duceppemo/popgenVCF/blob/main/docs/STYLE_GUIDE.md)

## Maintainers

- [Release and Governance](Release-and-Governance)
- [Project charter](https://github.com/duceppemo/popgenVCF/blob/main/docs/PROJECT_CHARTER.md)
- [Roadmap](https://github.com/duceppemo/popgenVCF/blob/main/docs/ROADMAP.md)
- [Release-candidate closure](https://github.com/duceppemo/popgenVCF/blob/main/docs/developer/release-candidate-closure.md)
- [Archival readiness](https://github.com/duceppemo/popgenVCF/blob/main/docs/developer/release-archival-readiness.md)
- [Tagged source release](https://github.com/duceppemo/popgenVCF/blob/main/docs/developer/tagged-source-release.md)

## Source-of-truth rule

When a wiki summary conflicts with a normative repository contract, the
normative contract wins. Correct the wiki in the same change. Generated API
documentation follows roxygen source and must not be hand-maintained as a
competing interface definition.
