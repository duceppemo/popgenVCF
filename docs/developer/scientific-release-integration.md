# End-to-end scientific release integration

Issue #105 adds a dedicated integration contract for the complete popgenVCF scientific release identity chain.

## Purpose

The integration workflow verifies that deterministic scientific records, validation evidence, artifact checksums, and the final `PopgenVCFScientificRelease` can be produced and verified together. It is intentionally separate from the release benchmark archive so failures can be attributed to either release identity integration or longitudinal benchmark publication.

## Integration chain

The builder creates deterministic records for:

1. the installed analysis registry;
2. provenance DAG identity;
3. artifact lineage identity;
4. FAIR bundle identity;
5. manuscript identity;
6. regeneration plan identity;
7. regeneration execution identity;
8. regeneration verification identity;
9. benchmark identity;
10. scientific validation identity.

The scientific validation record is backed by `run_scientific_validation(integration = TRUE)` and `run_population_structure_validation(integration = TRUE)`. Their canonical TSV outputs are included in the artifact manifest. Every downstream record explicitly references its parent identity.

## Determinism checks

`scripts/build_scientific_release_integration.R` builds the complete release twice with identical explicit inputs. It rejects the run unless:

- every validation suite passes;
- every written release checksum validates;
- all ten component SHA256 identities match between executions;
- the final scientific release digest matches between executions;
- modification of a written release file is detected by manifest validation.

Runtime timestamps and temporary paths are excluded from scientific identities. The release date and Git identity are explicit inputs.

## Local execution

Install the package and run:

```bash
R CMD INSTALL .
POPGENVCF_RELEASE_DATE=2026-07-18 \
Rscript scripts/build_scientific_release_integration.R \
  scientific-release-integration local-integration
```

The output contains:

- two independently generated runs;
- canonical component records;
- the final release JSON, Markdown, TSV, and SHA256 manifest;
- `integration-summary.json`;
- `determinism-comparison.tsv`;
- a deliberately modified copy used only to prove tamper detection.

## CI artifact

`.github/workflows/scientific-release-integration.yml` runs on pull requests, pushes to `main`, and manual dispatch. It uploads the verified release directory, component records, deterministic comparison, summary, compressed archive, and archive checksum.

## Scientific boundary

This integration validates reproducibility contracts, deterministic identity linkage, serialization, and checksum integrity. It does not certify biological interpretation, replace external real-data validation, or imply that a passing software contract makes every scientific conclusion correct.
