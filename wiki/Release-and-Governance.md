# Release and governance

popgenVCF separates implementation completion, scientific approval, and release
authorization. A successful test or workflow cannot silently substitute for a
human decision.

## Decision hierarchy

1. scientific correctness and the project charter;
2. validation contracts and canonical estimator definitions;
3. stable public interfaces and release policy;
4. architecture and artifact contracts;
5. development and style conventions;
6. roadmap scheduling.

## Release gates

The 0.10.0 policy defines 15 gates:

1. metadata consistency;
2. public API contract;
3. source-package check;
4. deterministic scientific validation;
5. canonical real-data validation;
6. production baseline approval;
7. external concordance approval;
8. three-backend ancestry approval;
9. benchmark-history approval;
10. source distribution;
11. OCI distribution;
12. Apptainer distribution;
13. archival assets;
14. final scientific approval;
15. release authorization.

Each passed gate references checksum-bound artifacts for one exact candidate
commit. Approval-gated evidence also records state, reviewer, date, and notes.

## Rehearsal versus production

Routine pull requests build a deliberately blocked rehearsal dossier. A
rehearsal tests policy parsing, serialization, checksums, and false-positive
protection but can never authorize a release.

A production dossier consumes retained evidence for the exact candidate and is
ready only when every required gate passes.

## Scientific approval

Scientific approval covers the validity of retained scientific evidence. It is
not inferred from execution and is distinct from release authorization. See
[Validation and Scientific Review](Validation-and-Scientific-Review).

## Release authorization

After a production dossier reports `READY`, the release owner separately
authorizes tagging, publication, artifact upload, archival deposition, and DOI
assignment. That decision must identify the same commit and artifact set.

## Evidence integrity

Evidence uses:

- explicit candidate, version, and commit identity;
- regular files and safe relative paths;
- byte sizes and SHA-256 digests;
- terminal checksum inventories;
- immutable or retained transport identities;
- fail-closed parsing and evaluation.

Measured outputs are regenerated when wrong. Approval metadata never changes a
measurement.

## Archival boundary

Development metadata intentionally omits a release date and DOI. DOI and final
archive claims are added only when the corresponding release artifacts have
passed validation, approval, authorization, and deposition.

## Canonical documents

- [Project charter](https://github.com/duceppemo/popgenVCF/blob/main/docs/PROJECT_CHARTER.md)
- [Roadmap](https://github.com/duceppemo/popgenVCF/blob/main/docs/ROADMAP.md)
- [Release-candidate closure](https://github.com/duceppemo/popgenVCF/blob/main/docs/developer/release-candidate-closure.md)
- [Canonical release gate](https://github.com/duceppemo/popgenVCF/blob/main/docs/CANONICAL_RELEASE_GATE.md)
- [Release archival readiness](https://github.com/duceppemo/popgenVCF/blob/main/docs/developer/release-archival-readiness.md)
- [Reproducibility](https://github.com/duceppemo/popgenVCF/blob/main/docs/reproducibility.md)
- [Scientific review assignment](https://github.com/duceppemo/popgenVCF/blob/main/docs/SCIENTIFIC_REVIEW_ASSIGNMENT.md)
