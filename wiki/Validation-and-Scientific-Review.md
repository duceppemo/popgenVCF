# Validation and scientific review

This page is for validators and the named scientific assignee. It distinguishes
automated evidence checks from scientific judgment and explains exactly how a
decision returns to the release process.

The normative source is the
[scientific reviewer runbook](https://github.com/duceppemo/popgenVCF/blob/main/docs/SCIENTIFIC_REVIEW_ASSIGNMENT.md).

## Validation hierarchy

popgenVCF uses complementary evidence:

1. analytical fixtures with hand-derived expectations;
2. independent implementations and external tools;
3. structural invariants;
4. checksum-pinned canonical real data;
5. cross-version and benchmark comparisons;
6. explicit named scientific review.

Routine package tests do not replace production real-data review.

## Assigned reviewer

For 0.10.0, scientific review is assigned to:

```text
Marc-Olivier Duceppe
marc-olivier.duceppe@inspection.gc.ca
ORCID: 0000-0003-2130-0427
```

The assignment covers the production baseline, external concordance,
three-backend ancestry case, benchmark history, and final scientific evidence
set. Assignment does not confer approval and does not include release
authorization.

## End-to-end process

1. A production workflow executes one exact candidate commit and writes
   immutable evidence, logs, commands, versions, tables, and checksums.
2. The reviewer obtains the retained artifact through GitHub Actions, a release,
   or a controlled institutional channel.
3. The reviewer runs the packet builder against an unmodified copy.
4. Automated checks verify integrity and summarize declared comparisons.
5. The reviewer completes the manual checklist and records approval or
   rejection with the actual date and artifact-specific notes.
6. The completed packet returns through a reviewed pull request or controlled
   release-evidence channel.
7. An integrator records the decision in component evidence and the production
   evidence index.
8. The release-candidate evaluator recomputes every digest and gate, then
   reports `READY` or `BLOCKED`.

Nothing is automatically emailed or uploaded. Results are not automatically
sent to the reviewer, and a completed decision is not automatically sent back.
Artifact production, mechanical comparison, and final readiness calculation are
automated; delivery, judgment, and return are explicit human steps.

## Obtain the current baseline proposal

The first promotion-eligible proposal is workflow run `30065732603`:

```bash
mkdir -p review-input
gh run download 30065732603 \
  --name canonical-production-evidence \
  --dir review-input
```

Do not approve proposal 3: its source checksums lost filename bindings during
serialization. If the retained Actions artifact expires, regenerate a
candidate-bound proposal and preserve the new transport identity.

## Build the review packet

```bash
Rscript scripts/build_scientific_review_packet.R \
  review-input \
  scientific-review-packet
```

For a complete candidate evidence set:

```bash
Rscript scripts/build_scientific_review_packet.R \
  review-input \
  scientific-review-packet \
  --strict
```

The packet contains automated checks, assigned gate status, baseline,
concordance and benchmark summaries, a 33-item manual checklist, an unsigned
decision template, a Markdown report, and terminal checksums.

Verify it:

```bash
cd scientific-review-packet
sha256sum --check scientific-review-packet-SHA256SUMS.txt
```

Possible packet states:

- `INTEGRITY FAILED` — do not review or sign; replace or regenerate evidence;
- `EVIDENCE INCOMPLETE` — component review can continue, but final approval
  cannot;
- `READY FOR MANUAL SCIENTIFIC REVIEW` — mechanical checks passed and assigned
  gates have indexed evidence; scientific approval is still pending.

## What the script checks

- every discovered terminal checksum entry;
- evidence-index size and SHA-256 records;
- candidate and gate identities;
- baseline snapshot/observation serialization consistency;
- concordance inventory, role, status, and approval declarations;
- continuous benchmark comparison status;
- likely ancestry evidence inventory;
- absence of symbolic links in retained evidence.

The baseline comparison is not independent: the snapshot and observation table
come from the same run. Trace all six values to the retained QC and PCA tables.

## What the reviewer must judge

### Production baseline

Confirm dataset identity/checksums, 2,504 unique complete samples, fixed region
and QC/PCA contract, sample and marker order, all source tables, counts, PCA
behavior, exact count comparisons, and the rationale for relative `1e-6` PCA
variance tolerance.

### External concordance

For each required tool-analysis pair, confirm command, version, executable or
container identity, input/sample order, estimator compatibility, citations,
tolerances, every long-form comparison row, and the distinction between
equivalence and diagnostic evidence.

Failed, skipped, errored, missing, or unapproved equivalence evidence blocks
approval.

### Three-backend ancestry

Confirm ADMIXTURE, fastStructure, and LEA/sNMF used the same biological samples;
verify backend conversions and sample-order files; inspect K ranges, seeds,
replicates, fit statistics, raw Q matrices, label alignment, RMSE/correlations,
consensus evidence, and limitations. Agreement is not proof that K or ancestry
components are biologically true.

### Benchmark history

Confirm benchmark and baseline identity, repetitions, runner comparability,
runtime/memory/throughput/scaling checks, declared budget, and historical trend
interpretation. Hosted-runner noise must not be represented as a scientific
regression without evidence.

### Final scientific evidence

Confirm every assigned gate is complete for the same candidate, failures are
resolved, claims match methods/results/limitations, and no approval metadata
was inferred from successful execution.

## Complete and return the decision

1. Fill every required row in `manual-review-checklist.tsv` with a status and
   artifact-specific notes.
2. Copy `scientific-review-decision-template.json`; fill the actual ISO date,
   overall decision, per-gate decisions, rationale, limitations, and exact
   artifact identities.
3. Verify and retain the completed packet checksum.
4. Return the packet in a reviewed pull request or controlled release-evidence
   channel.

The decision template is deliberately pending and is not directly consumed as
approval. An integrator must create component-specific approved records and add
this approval object to each applicable evidence-index gate:

```json
{
  "state": "approved",
  "reviewer": "Marc-Olivier Duceppe (ORCID: 0000-0003-2130-0427)",
  "reviewed_at": "YYYY-MM-DD",
  "notes": "Artifact-specific rationale and limitations."
}
```

The dossier builder then checks the exact artifacts and decision automatically.
Release tagging, publication, DOI deposition, and release authorization remain
separate later actions.

## Independent rerun

Where practical, rerun deterministic and integration suites from a clean
installation:

```bash
Rscript -e 'print(popgenVCF::run_scientific_validation(integration = TRUE))'
Rscript -e 'print(popgenVCF::run_population_structure_validation(integration = TRUE))'
validation/run-validation.sh
```

Retain the reproduction separately and compare it with the candidate. Never
silently replace the candidate evidence.
