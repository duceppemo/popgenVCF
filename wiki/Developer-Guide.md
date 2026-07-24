# Developer guide

Read [CONTRIBUTING.md](https://github.com/duceppemo/popgenVCF/blob/main/CONTRIBUTING.md),
the [Development Guide](https://github.com/duceppemo/popgenVCF/blob/main/docs/DEVELOPMENT_GUIDE.md),
and [Architecture](https://github.com/duceppemo/popgenVCF/blob/main/docs/ARCHITECTURE.md)
before changing public or scientific behavior.

## Repository layout

| Path | Purpose |
| --- | --- |
| `R/` | Package implementation |
| `man/` | Generated API documentation |
| `tests/testthat/` | Unit, integration, contract, and regression tests |
| `validation/` | Independent scientific validation runners and fixtures |
| `vignettes/` | Long-form user workflows rendered by pkgdown |
| `docs/` | Normative design, governance, validation, and release documents |
| `wiki/` | Maintained source for the GitHub Wiki |
| `inst/` | Installed scripts, metadata, templates, fixtures, and contracts |
| `scripts/` | Repository command wrappers and release/validation builders |
| `.github/workflows/` | CI, validation, packaging, and release workflows |

## Development environment

```bash
git clone https://github.com/duceppemo/popgenVCF.git
cd popgenVCF
conda config --set channel_priority strict
mamba env create --file inst/conda/environment.yml
conda activate popgenvcf
Rscript inst/scripts/install-bioconductor.R
R CMD INSTALL .
```

## Tests

During development, run focused tests first:

```bash
Rscript -e 'devtools::test(filter = "relevant-context", stop_on_failure = TRUE)'
```

Then run broader checks appropriate to the risk:

```bash
Rscript -e 'devtools::test(stop_on_failure = TRUE)'
R CMD build .
R CMD check popgenVCF_*.tar.gz --as-cran
```

Scientific changes also require:

```bash
Rscript -e 'stopifnot(popgenVCF::run_scientific_validation(integration = TRUE)$passed)'
Rscript -e 'stopifnot(popgenVCF::run_population_structure_validation(integration = TRUE)$passed)'
```

Canonical real-data and external-tool workflows are opt-in production evidence,
not ordinary package checks.

## Scientific module standard

A new or changed numerical module needs:

- a precise estimator definition and references;
- input, output, identity, and failure contracts;
- analytical fixtures where feasible;
- an independent expected result or implementation;
- structural invariants and boundary tests;
- justified comparison metrics and tolerances;
- machine-readable validation evidence;
- user interpretation and limitation guidance;
- public API and generated documentation reconciliation.

Do not generate expected values from the implementation under test and call
that independent validation.

## Public API and documentation

Roxygen source is authoritative for generated `man/` pages and `NAMESPACE`.
After API changes, regenerate and reconcile the public API baseline according to
the repository guides. Do not hand-edit generated documentation unless the
document explicitly identifies a different owner.

Documentation roles:

- README: concise landing page;
- Wiki: task-oriented human guide;
- pkgdown: generated API and rendered vignettes;
- `docs/`: normative contracts, architecture, governance, and evidence policy;
- `NEWS.md`: user-visible changes.

Update all affected surfaces in the same reviewed change.

## Wiki maintenance

Edit Markdown under `wiki/`. Preview links locally, then publish reviewed pages:

```bash
scripts/publish-wiki.sh
scripts/publish-wiki.sh --push
```

The first command is a dry run. Publication updates managed pages without
deleting unrelated historical pages.

## Pull requests

A useful pull request explains:

- the user or scientific problem;
- implementation and contract changes;
- compatibility and migration implications;
- tests and validation evidence;
- documentation changes;
- known limitations and deferred work.

Do not mix unrelated formatting, generated artifacts, scientific approvals, or
baseline changes into a functional patch.

## Security and sensitive data

Never commit private genotype data, credentials, access tokens, institutional
paths, or identifiable sample metadata. Tests use synthetic fixtures. Public
canonical evidence excludes raw genotype files and retains checksums,
descriptors, logs, and aggregate outputs only.

## Useful references

- [Module contract](https://github.com/duceppemo/popgenVCF/blob/main/docs/MODULE_CONTRACT.md)
- [Public API contract](https://github.com/duceppemo/popgenVCF/blob/main/docs/PUBLIC_API_CONTRACT.md)
- [Style guide](https://github.com/duceppemo/popgenVCF/blob/main/docs/STYLE_GUIDE.md)
- [Reproducibility](https://github.com/duceppemo/popgenVCF/blob/main/docs/reproducibility.md)
- [Roadmap](https://github.com/duceppemo/popgenVCF/blob/main/docs/ROADMAP.md)
