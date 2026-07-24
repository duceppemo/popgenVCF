# Scientific review assignment

Marc-Olivier Duceppe is the designated reviewer for the scientific and
quantitative evidence that requires named approval for popgenVCF 0.10.0.

- Email: `marc-olivier.duceppe@inspection.gc.ca`
- ORCID: [0000-0003-2130-0427](https://orcid.org/0000-0003-2130-0427)
- Assigned: 2026-07-24

The assignment covers the production quantitative baseline, external-tool
concordance, the real-data three-backend ancestry case, release benchmark
history, and final review of the complete scientific evidence set. It does not
cover the separate release-authorization decision.

Assignment is not approval. Each result remains proposed, blocked, or not run
until its retained evidence has been inspected and an explicit dated decision
has been recorded. In particular, the promotion-eligible autosomal baseline
proposal remains `approval: proposed`.

The machine-readable assignment is
`inst/metadata/scientific-review-assignment.json`. Approval records should use
the reviewer identity `Marc-Olivier Duceppe
(ORCID: 0000-0003-2130-0427)` and must preserve the actual review date and
artifact-specific notes.

## How the validation process works

Scientific validation has four distinct stages:

1. A production workflow executes an analysis for one exact Git commit and
   writes immutable evidence, logs, tables, commands, environment details, and
   checksums. Workflow success creates a proposal; it is not approval.
2. The reviewer downloads the retained evidence without modifying it and runs
   the review-packet builder. The builder verifies mechanical integrity and
   summarizes machine-readable comparisons.
3. The reviewer independently inspects the scientific evidence, completes the
   checklist, and records an approved or rejected decision with the actual
   review date and evidence-specific notes.
4. A release integrator incorporates that decision into the component approval
   records and the production evidence index. The release-candidate evaluator
   rechecks every indexed file and gate. It reports `READY` only when all 15
   release gates pass; release authorization remains a separate decision.

The reviewer does not need to email results to a service, and the packet builder
does not upload, email, approve, or modify evidence. The completed review must
be returned as a reviewed pull request or attached to the candidate's retained
release evidence. This creates an auditable record. Merely sending an informal
email or saying "looks good" does not satisfy the gate contract.

## What is automatic and what is manual

The packet builder automatically:

- verifies every discovered `*SHA256SUMS.txt` entry;
- checks the evidence index's file sizes and SHA-256 digests;
- summarizes assigned gate status and existing approval metadata;
- compares the baseline snapshot values with its observation table;
- extracts external-concordance pass, role, and approval states;
- extracts continuous-benchmark comparison states;
- inventories likely ancestry evidence files;
- creates a checksummed report, editable checklist, and unsigned decision
  template.

These automated comparisons detect corruption, missing files, inconsistent
serialization, and declared numerical failures. They do not determine whether
the estimator is appropriate, the tolerance is justified, population structure
is biologically credible, a diagnostic difference is acceptable, or the
scientific claims are supported. Those are reviewer decisions.

In particular, baseline snapshot-to-observation agreement is an internal
consistency check. Both files originate from the same execution. The reviewer
must still trace the values to the retained QC and PCA source tables and assess
the analysis independently.

## Reviewer prerequisites

Use a clean checkout of the exact candidate commit and a separate directory for
downloaded evidence. Required local commands are Git, R 4.3 or newer, and
`sha256sum`. The R packages `data.table`, `digest`, and `jsonlite` must be
installed. `gh` is convenient for downloading GitHub Actions artifacts but is
not required if the evidence is supplied through another checksum-preserving
channel.

Do not review a moving branch name. Record the full 40-character commit. Do not
edit measured outputs, checksum manifests, logs, or evidence tables. If evidence
is wrong or incomplete, reject it or leave it pending and request regeneration.

The canonical reviewer identity is:

```text
Marc-Olivier Duceppe (ORCID: 0000-0003-2130-0427)
```

## Obtain the evidence

For the current promotion-eligible autosomal proposal, download workflow run
`30065732603`:

```bash
mkdir -p review-input
gh run download 30065732603 \
  --name canonical-production-evidence \
  --dir review-input
```

The GitHub Actions copy is scheduled to expire on 2026-10-22. If it is no
longer available, a new candidate-bound run must reproduce and retain equivalent
evidence. Do not approve the older proposal 3 snapshot: it lacks filename-bound
source checksum serialization and is not promotion eligible.

A final review input should contain the complete production evidence set and
`release-candidate-evidence-index.json`. A component-only download can still be
reviewed, but the packet will correctly report `EVIDENCE INCOMPLETE` until all
assigned gates are present.

Before running any R code, preserve the downloaded archive or directory and its
transport digest in read-only institutional storage when possible. Work on a
copy.

## Build the reviewer packet

From the exact repository checkout, run:

```bash
Rscript scripts/build_scientific_review_packet.R \
  review-input \
  scientific-review-packet
```

Use strict mode only for a complete candidate evidence set:

```bash
Rscript scripts/build_scientific_review_packet.R \
  review-input \
  scientific-review-packet \
  --strict
```

Strict mode returns a non-zero status if an integrity check fails or an assigned
gate lacks complete indexed evidence. Non-strict mode always attempts to write
the report so incomplete component evidence can still be examined. Integrity
failures are never downgraded; they appear as `INTEGRITY FAILED`.

The output directory contains:

```text
scientific-review-packet/
|-- scientific-review-report.md
|-- automated-checks.tsv
|-- assigned-gates.tsv
|-- baseline-summary.tsv
|-- concordance-summary.tsv              # when present
|-- benchmark-summary.tsv                # when present
|-- manual-review-checklist.tsv
|-- scientific-review-decision-template.json
`-- scientific-review-packet-SHA256SUMS.txt
```

Verify the generated packet:

```bash
cd scientific-review-packet
sha256sum --check scientific-review-packet-SHA256SUMS.txt
cd ..
```

Do not sign or return a packet with `INTEGRITY FAILED`. Resolve its failed rows
in `automated-checks.tsv` by obtaining or regenerating correct evidence.

## Optional independent re-execution

Re-execution is strongly recommended when the required tools and source data
are available. It complements, but does not replace, review of the retained
candidate evidence.

Run the deterministic and integration suites from a clean installation:

```bash
Rscript -e 'print(popgenVCF::run_scientific_validation())'
Rscript -e 'print(popgenVCF::run_scientific_validation(integration = TRUE))'
Rscript -e 'print(popgenVCF::run_population_structure_validation())'
Rscript -e 'print(popgenVCF::run_population_structure_validation(integration = TRUE))'
validation/run-validation.sh
```

For an independent canonical-data execution, follow
`docs/developer/canonical-production-execution.md`. Keep raw canonical data
outside both the repository and evidence directories. Record source checksums,
the exact commit, commands, versions, environment, and the newly produced
terminal checksum inventory. Compare a reproduction with the candidate; never
replace candidate evidence silently.

## Review the production baseline

Confirm all of the following:

1. The candidate is proposal 4 or a later valid reproduction from commit
   `4dcf69ff488f659cbca7d3a7cae5a8db3f8ddf18` or its explicitly reviewed
   successor.
2. Dataset ID `1000g_phase3_chr22_v5a`, version `20130502-v5a`, source VCF,
   index, and panel filenames are bound to their approved checksums.
3. All 2,504 samples are unique and have complete population,
   superpopulation, and sex metadata; the VCF and panel sample sets agree.
4. The fixed contract is region `22:20000000-21000000`, biallelic SNPs,
   maximum sample and variant missingness 0.20, MAF 0.05, LD r2 0.20, seed 42,
   ten requested PCs, four threads, and PCA only.
5. Inspect the sample-QC, metadata-match, independent/sequential QC, PCA-score,
   PCA-variance, execution-ledger, validation, and log files—not just the
   summary JSON.
6. Independently confirm 21,418 subset variants, 2,504 retained samples, 2,028
   QC variants, 350 LD-pruned variants, PC1 proportion
   0.26553988138366075, and PC2 proportion 0.17740063253018323.
7. Exact comparison is appropriate for counts. The relative tolerance `1e-6`
   for PCA proportions is scientifically and numerically justified. Never
   loosen a tolerance simply to pass.
8. PCA behavior, sample order, outliers, missingness, population patterns, and
   retained-marker evidence do not indicate a silent data or implementation
   error.

After approval, an integrator can create the approved snapshot through the
public API rather than hand-editing JSON:

```r
proposal <- popgenVCF::read_canonical_real_data_baseline_snapshot(
  "review-input/autosomal-baseline-proposal/autosomal-baseline-proposal.json"
)
approved <- popgenVCF::approve_canonical_real_data_baseline_snapshot(
  proposal,
  approved_by = "Marc-Olivier Duceppe (ORCID: 0000-0003-2130-0427)",
  approved_at = "YYYY-MM-DD",
  notes = "Artifact-specific scientific review rationale and limitations."
)
popgenVCF::write_canonical_real_data_baseline_snapshot(
  approved,
  "approved-autosomal-baseline.json",
  require_approved = TRUE
)
```

Replace `YYYY-MM-DD` with the actual review date. Commit the approved snapshot
and review records in a separate reviewed change; do not overwrite the original
proposal evidence.

## Review external-tool concordance

For every required tool-analysis pair, inspect the long-form comparison table,
not only its pass flag. Confirm the external tool/version, exact command,
container or executable identity, input checksum, sample and marker order,
tolerance profile, environment, interpretation, and citations.

An `equivalence` comparison may pass only when both implementations estimate
the same quantity and every required row is within a predeclared justified
tolerance. Failed, skipped, errored, missing, or unapproved equivalence evidence
blocks approval. A `diagnostic` comparison may preserve a meaningful
cross-method difference, but the difference and estimator mismatch must be
explained and must not be called equivalence.

Approved concordance records must be rebuilt with
`new_scientific_concordance_record(..., approval = "approved",
approved_by = ..., approved_at = ...)`, assembled into a complete suite, and
written with `write_scientific_concordance_evidence(...,
require_release_ready = TRUE)`. That final writer fails closed if the suite is
not complete and approved.

## Review the three-backend ancestry case

Confirm ADMIXTURE, fastStructure, and LEA/sNMF used the same checksum-pinned
biological samples. Check backend-specific input conversions and sample-order
files against every Q-matrix row. Inspect commands, versions, seeds, K range,
replicate schedule, convergence/fit statistics, cross-validation or entropy
criteria, raw Q matrices, logs, label permutations, aligned matrices, replicate
RMSE/correlations, consensus evidence, and tolerances.

Agreement demonstrates computational and structural consistency. It does not
prove that the selected K or inferred components are biologically true. The
review notes must state this limitation and distinguish backend agreement from
biological interpretation.

## Review benchmark history

Verify the archive and transport checksums. Confirm benchmark identity, module,
dataset tier, threads, release, commit, observations, repetitions, hardware and
software fingerprints, baseline selection, and predeclared budget. A comparison
is gating only when evidence is complete, environments are compatible, metrics
are comparable, minimum repetitions are met, and every budget check passes.

Treat GitHub-hosted-runner timing as noisy informational evidence unless the
record explicitly establishes a stable comparable runner. Explain historical
changes; do not classify a changed digest or normal runner variation as a
scientific regression without supporting evidence.

## Complete and return the decision

Open `manual-review-checklist.tsv` and set every `reviewer_status` to an
explicit value such as `passed`, `failed`, or `not_applicable`, with
artifact-specific notes. Required items cannot remain `pending` for approval.

Copy `scientific-review-decision-template.json` to a new decision file. Fill in:

- the actual ISO review date;
- overall `approved` or `rejected` decision;
- one decision and rationale for every assigned gate;
- limitations, deviations, unresolved concerns, and exact artifact identities.

The generated template is deliberately `pending` and is not directly consumed
as approval by the release evaluator. Return the completed checklist, decision
file, report, and packet checksum through a pull request or the controlled
release-evidence channel. The integrator then:

1. records your decision in the component-specific approved artifacts;
2. sets the corresponding evidence-index approval object to `approved` or
   `rejected`, with your canonical identity, actual date, and notes;
3. retains every approved artifact's size and SHA-256;
4. rebuilds the release-candidate dossier;
5. returns any failed comparison or changed evidence to you for renewed review.

The evidence-index approval shape is:

```json
{
  "state": "approved",
  "reviewer": "Marc-Olivier Duceppe (ORCID: 0000-0003-2130-0427)",
  "reviewed_at": "YYYY-MM-DD",
  "notes": "Artifact-specific rationale and limitations."
}
```

No result is automatically sent to you, and no completed decision is
automatically sent back. Artifact production and numerical comparison are
automated; reviewer notification, scientific judgment, and return of the signed
decision are explicit human workflow steps. Final `READY` calculation is then
automatic and fail closed. Tagging, publication, deposition, DOI assignment,
and release authorization occur only afterward and are outside this scientific
review assignment.
