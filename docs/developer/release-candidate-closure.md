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

## Gate-record fragments and the evidence collector (2026-08-01, continued)

The hand-authored, per-release-specific version of `scripts/build_release_candidate_evidence_index.R` described above (with each gate's prose and artifact list written directly in R source) was used to close out the 0.10.0 release, then replaced with a generic version once 0.10.0 shipped. It no longer hardcodes anything about a specific release.

Every gate's evidence is now a small, self-describing JSON fragment named `<gate_id>-gate-record.json`:

```json
{
  "gate_id": "<one of the release-candidate-policy.json gate ids>",
  "status": "passed" | "failed" | "blocked" | "not_run",
  "summary": "<one paragraph: what was checked and why the status holds>",
  "artifacts": [
    {"path": "<relative to this fragment's own directory>", "size_bytes": 123, "sha256": "<hex>"}
  ],
  "approval": null
}
```

`approval` is `null` for gates that do not require named sign-off, or `{"state": "approved"|"rejected"|"pending", "reviewer": "<name>", "reviewed_at": "YYYY-MM-DD", "notes": "<optional>"}` for approval-required gates. `inst/scripts/release_candidate_gate_record.R` provides `write_release_candidate_gate_record()`, a small R helper any CI step or reviewer script can call to produce one correctly.

Eight gates can be determined by CI alone, because they are objective technical checks rather than scientific judgment calls: `metadata_consistency`, `public_api_contract`, `source_package_check`, `scientific_validation`, `canonical_validation`, `source_distribution`, `apptainer_distribution`, and `oci_distribution` (the last one only when a real registry push happens). Each gate's own producing workflow now writes its fragment as a normal part of its run:

| Gate | Workflow | Fragment location |
| --- | --- | --- |
| `metadata_consistency` | `release-metadata.yml` | `artifacts/metadata_consistency-gate-record.json` |
| `public_api_contract` | `public-api-contract.yml` | `artifacts/public-api-contract/public_api_contract-gate-record.json` |
| `source_package_check`, `scientific_validation`, `source_distribution` | `tagged-source-release.yml` | `release-assets/<gate_id>-gate-record.json` |
| `canonical_validation` | `canonical-real-data.yml` | `canonical-production-evidence/canonical-validation-gate-record.json` |
| `apptainer_distribution` | `apptainer.yml` | `apptainer-metadata/apptainer_distribution-gate-record.json` |
| `oci_distribution` | `container.yml` | `release-container/oci_distribution-gate-record.json` (only when `publish=true`, i.e. a real registry push happened) |

`scripts/build_release_candidate_evidence_index.R` now takes `<policy.json> <evidence-sources-dir> <output-index.json> <output-evidence-dir> <candidate-id> <git-commit> <evaluated-at>`. It recursively finds every `*-gate-record.json` under `<evidence-sources-dir>`, re-verifies each artifact's recorded checksum against the real file, flattens nested artifact paths into `<gate_id>--<flattened-path>` filenames in `<output-evidence-dir>` (GitHub Release assets cannot contain directory structure), and errors closed on a duplicate `gate_id`, an unknown `gate_id`, or a checksum mismatch. Gates with no fragment are honestly `not_run`; it never fabricates evidence.

`.github/workflows/release-candidate-collector.yml` automates the CI-producible half of this: given a `candidate_ref` (a branch or tag -- the `workflow_dispatch` API cannot target an arbitrary commit SHA), a `candidate_id`, a `dataset`, and whether to actually publish the container image, it dispatches all six producing workflows against that ref, verifies each landed on the exact resolved commit (not a stale prior run), downloads their fragments, and runs the collector script -- uploading a `release-candidate-collected-evidence-<candidate_id>` artifact with whatever it could determine. A single gate's CI failure does not abort collecting the others.

The remaining 7 gates -- `production_baseline`, `external_concordance`, `ancestry_three_backend`, `benchmark_history`, `scientific_approval`, `release_authorization`, and `archival_assets` -- require real scientific review, a named reviewer's sign-off, or (for `archival_assets`) an actual Zenodo deposition that can only happen after the release is published. No workflow produces these automatically, by design. To close a release candidate: download the collector's artifact, add each of those 7 gates' own `<gate_id>-gate-record.json` fragments (and their referenced evidence files) into the same source tree, and re-run `build_release_candidate_evidence_index.R` locally against the combined directory to get the final, complete index.

## Human-gated fragments for the scientific-judgment gates (2026-08-01)

`production_baseline`, `external_concordance`, `ancestry_three_backend`, and `benchmark_history` are the 4 gates on the named reviewer's standing assignment (`inst/metadata/scientific-review-assignment.json`) that recur every release -- unlike `scientific_approval`, `release_authorization`, and `archival_assets`, which are one-off, release-specific sign-offs rather than reusable scientific-review records. `scripts/write_scientific_review_gate_record.R` turns a reviewer's determination into a correctly-shaped, checksum-verified `<gate_id>-gate-record.json` fragment without anyone hand-computing checksums or hand-writing JSON:

```text
Rscript scripts/write_scientific_review_gate_record.R <review-spec.json> <evidence-dir> <output-path>
```

The review-spec is a small declarative JSON the reviewer (or whoever is recording their determination) writes:

```json
{
  "gate_id": "production_baseline",
  "status": "passed",
  "summary": "One paragraph: what was reviewed and why the status holds.",
  "artifacts": ["autosomal-baseline-proposal.json", "autosomal-baseline-observations.tsv"],
  "approval": {
    "state": "approved", "reviewer": "Full Name", "reviewed_at": "YYYY-MM-DD", "notes": "Optional."
  }
}
```

`artifacts` are paths relative to `<evidence-dir>`; the script never invents evidence -- every artifact must already exist there, and it fails closed (before writing anything) if the spec references a missing artifact, a missing required field (`gate_id`, `status`, `summary`, `artifacts`), or an empty artifact list. `approval` may be omitted or `null` for gates that do not need a named sign-off. Checksums and sizes are always computed from the real files via the shared `write_release_candidate_gate_record()` helper, never copied from the spec.

This was run against the real, retained `v0.10.0` Release evidence for all 4 gates (downloaded via `gh release download v0.10.0`), using the exact review determinations already recorded in `docs/developer/canonical-autosomal-baseline-proposal.md`, `docs/SCIENTIFIC_CONCORDANCE.md`, `docs/user/ancestry-backends.md`, and `docs/CONTINUOUS_RELEASE_BENCHMARKING.md`. Merging the 4 resulting fragments with the 8 CI-auto-collected fragments from a real `release-candidate-collector.yml` run through `build_release_candidate_evidence_index.R` reproduced `12 / 15` gates `passed`, with the 3 blocking gates correctly limited to `scientific_approval`, `release_authorization`, and `archival_assets` -- matching what was independently proven true for the real `v0.10.0` release, now demonstrated end to end with the generic fragment tooling instead of a hand-authored, release-specific script.

## Current scientific boundary

The closure mechanism does not complete the production work tracked by #22, #24, #43, and #1. Until those real-data, external-tool, ancestry, benchmark, distribution, and approval records exist and are reviewed, the 0.10.0 release candidate must remain blocked.
