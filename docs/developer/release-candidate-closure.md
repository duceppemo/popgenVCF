# Phase 0.9.31 release-candidate closure

Phase 0.9.31 combines the separate software, scientific, benchmark, distribution, and archival records into one reviewer-facing release decision. It does not manufacture missing evidence, infer scientific approval from a successful command, or authorize publication merely because ordinary CI is green.

## Canonical policy

The machine-readable policy is installed at:

```text
inst/metadata/release-candidate-policy.json
```

The policy defines 15 required gates:

1. metadata consistency;
2. public API stability;
3. source-package validation;
4. deterministic scientific validation;
5. canonical real-data validation;
6. approved production baseline;
7. approved external-tool concordance;
8. approved ADMIXTURE, fastStructure, and LEA/sNMF evidence;
9. approved historical benchmark evidence;
10. source-distribution verification;
11. OCI-distribution verification;
12. Apptainer-distribution verification;
13. archival asset verification;
14. named scientific approval;
15. named release authorization.

The gate list is policy data rather than evaluator source code. A policy change requires review and a complete validation rerun.

## Evidence bundle

A closure evaluation consumes a directory with this structure:

```text
release-candidate-evidence/
|-- release-candidate-evidence-index.json
`-- <every artifact referenced by the index>
```

The evidence index uses schema version `1.0` and records:

- evaluation mode: `rehearsal` or `production`;
- candidate identifier;
- target release and package version;
- exact 40-character Git commit;
- ISO-8601 evaluation timestamp;
- exactly one record for every policy gate.

Each gate record contains:

- `gate_id`;
- one status: `passed`, `failed`, `blocked`, or `not_run`;
- a non-empty summary;
- zero or more retained artifact records;
- approval metadata when the policy requires approval.

Every passed gate must reference at least one regular file. Each artifact record contains a relative path, exact byte count, and SHA-256 digest. Absolute paths, directory traversal, duplicate paths, missing files, size mismatches, and checksum mismatches fail closed.

## Approval records

Approval-required gates can pass only with:

- `state: approved`;
- a non-empty reviewer identity;
- an ISO-8601 review date;
- optional review notes.

Execution is not approval. A generated baseline, comparison table, benchmark, or ancestry result remains non-authoritative until a qualified reviewer evaluates the retained evidence and records an approval decision.

## Rehearsal mode

Pull requests and ordinary pushes run a deliberately blocked rehearsal. Rehearsal mode validates:

- policy parsing;
- complete gate inventory;
- deterministic ordering;
- dossier serialization;
- checksum generation and verification;
- false-positive protections.

A rehearsal cannot become release-ready, even when every supplied record otherwise passes. It never authorizes tagging, publication, Zenodo deposition, or DOI assignment.

Run a local rehearsal with:

```bash
Rscript scripts/build_release_candidate_rehearsal.R \
  inst/metadata/release-candidate-policy.json \
  release-candidate-evidence/release-candidate-evidence-index.json \
  0.10.0-rc1 \
  "$(git rev-parse HEAD)" \
  1970-01-01T00:00:00Z

Rscript scripts/build_release_candidate_dossier.R \
  inst/metadata/release-candidate-policy.json \
  release-candidate-evidence/release-candidate-evidence-index.json \
  release-candidate-evidence \
  release-candidate-dossier
```

The expected result is `BLOCKED`.

## Production evaluation

The manual `Release candidate closure` workflow evaluates production evidence only when `evidence_release_tag` identifies a GitHub Release containing the evidence index and every referenced artifact. The workflow checks out the exact candidate revision, verifies the index commit and package version, downloads the evidence assets, builds the dossier, and optionally fails unless the dossier is ready.

A production dossier becomes ready only when:

- every required gate has status `passed`;
- every passed gate retains checksum-verifiable evidence;
- every approval-required gate has a named approved review record;
- the evidence index identifies the exact evaluated commit;
- the evaluation mode is `production`.

## Dossier outputs

The evaluator writes:

```text
release-candidate-dossier/
|-- release-candidate-gates.tsv
|-- release-candidate-blockers.tsv
|-- release-candidate-artifacts.tsv
|-- release-candidate-dossier.json
|-- release-candidate-readiness.md
`-- release-candidate-SHA256SUMS.txt
```

Verify the terminal checksum record with:

```bash
cd release-candidate-dossier
sha256sum --check release-candidate-SHA256SUMS.txt
```

The JSON dossier is the machine-readable decision record. The TSV files support review and audit. The Markdown report summarizes readiness and blockers. The checksum file binds all generated dossier records.

## Review sequence

Before authorizing 0.10.0:

1. confirm the candidate commit and package version;
2. verify the evidence index inventory;
3. recompute all retained artifact checksums;
4. inspect each failed or blocked gate;
5. confirm reviewer identities and review dates;
6. validate source, OCI, and Apptainer distributions from clean environments;
7. review the complete archival asset inventory;
8. require a ready production dossier for the exact commit;
9. only then tag, publish, deposit, assign the DOI, and archive the release.

## First real evidence-index assembly (2026-08-01)

`scripts/build_release_candidate_evidence_index.R` is the production counterpart to the rehearsal script above: instead of deriving every gate from the policy alone (always `blocked`), it takes real, retained gate evidence from an evidence directory and assembles a checksum-verified, schema-valid index in `mode: production`. It does not fabricate evidence for gates it has no real source for -- those are honestly recorded as `not_run`, and a gate with only partial approval is recorded as `blocked` rather than `passed`.

Run against the real, formally approved `production_baseline`, `external_concordance`, and `ancestry_three_backend` evidence from this release cycle (see each gate's own "First execution and review" notes), plus the retained `benchmark_history` evidence:

- `production_baseline`, `external_concordance`, and `ancestry_three_backend` (gates 6-8) are `passed`, each with real checksum-verified artifacts and a named `approved` review record (Marc-Olivier Duceppe).
- `benchmark_history` (gate 9) is honestly `blocked`, not `passed`: `benchmark_identity` and `repetition_count` are approved, but `budget_checks`/`trend_interpretation` remain `insufficient-evidence` because this repository has zero published GitHub Releases for `release-benchmark-archive.yml`'s baseline-discovery step to compare against.
- The remaining 11 gates (1-5, 10-15) are `not_run`: this script has no real evidence source wired in for them yet. Each per-gate CI workflow that eventually supplies real evidence for one of them should extend this script (or a shared collector), rather than have a human hand-author a monolithic config for all 15 gates at once.

The resulting index was fed through the real `build_release_candidate_dossier.R` end to end: all checksums verified, `release_ready: false`, `3 / 15` required gates passed, `12` blocking gates -- exactly the state the underlying evidence actually supports, not a rehearsal placeholder. This closes the durable-retention gap for the three approved scientific gates (their evidence is now checksum-bound into a real index artifact, not only prose in `docs/`), but a production dossier still cannot become release-ready until the remaining gates are wired up and, separately, this index and its artifacts are published as assets on a real GitHub Release for `evidence_release_tag` to reference -- both still open.

## Current scientific boundary

The closure mechanism does not complete the production work tracked by #22, #24, #43, and #1. Until those real-data, external-tool, ancestry, benchmark, distribution, and approval records exist and are reviewed, the 0.10.0 release candidate must remain blocked.
