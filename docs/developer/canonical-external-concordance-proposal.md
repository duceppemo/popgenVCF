# External-tool concordance proposal (chr22)

This covers the `external_concordance` release gate (issue #22). See `docs/SCIENTIFIC_CONCORDANCE.md` for the gate's scope and schema (`equivalence` vs `diagnostic` roles, evidence fields, `require_release_ready`). This document tracks concrete executions of `scripts/run-external-concordance-chr22.R` against the approved chromosome 22 canonical source and their review status. The execution creates a scientific-review proposal; it does not approve the gate or authorize a release.

## Fixed analysis contract

Reuses the same bounded biallelic-SNP interval and QC contract as `production_baseline` (`22:20000000-21000000`, MAF 0.05, missingness 0.20, LD r2 0.20, seed 42), so results are anchored to the already-reviewed `production_baseline` evidence rather than an independently-scoped subset. Runs the full default `popgenVCF::run_pipeline()` analysis set, then independently recomputes PCA, IBS, FST, diversity, DAPC, and AMOVA via PLINK 2, SNPRelate (direct), hierfstat, adegenet, poppr, and pegas.

## Execution and evidence

```text
run-external-concordance-chr22.R <output-dir> <work-dir> <source-dir>
```

`<source-dir>` must already contain the approved, verified chromosome 22 source files. Requires `bcftools` and `plink2` on `PATH`, plus SNPRelate, adegenet, hierfstat, poppr, pegas, vegan, and ade4 installed. Not routine CI (matching `production_baseline`) -- normally run in a dedicated environment (this repository's `popgenvcf` conda environment, `inst/conda/environment.yml`).

Writes `scientific_concordance.json`/`.tsv`/`_methods.md` via `write_scientific_concordance_evidence()`. Each record carries the exact command, reference tool/version, tolerance profile, per-comparison numerical results (condensed for large matrices), and a fixed interpretation string distinguishing equivalence claims from diagnostic ones.

## First execution and review (2026-07-30)

Result: `inventory_complete: TRUE` (PLINK 2, SNPRelate, hierfstat, adegenet, pegas all represented); `release_ready: FALSE` pending named approval. All three applicable `equivalence` comparisons passed at machine precision: SNPRelate PCA (10/10 canonical correlations), SNPRelate IBS (6,270,016/6,270,016 cells), adegenet DAPC (5,008/5,008 cells). `vegan_mantel` was correctly excluded -- the real 1000 Genomes panel carries no latitude/longitude metadata.

Diagnostic comparisons: PLINK 2 KING vs IBS disagreed on essentially every pair, exactly as expected for non-equivalent estimands. hierfstat FST (0.081957) and SNPRelate FST (0.081714) agreed closely. adegenet diversity showed the expected explainable split: Hobs matched exactly, Hexp differed by the small-sample unbiased-correction factor. popgenVCF's own AMOVA value (poppr-based, 0.081714) matched hierfstat's/SNPRelate's FST closely, but pegas's independent AMOVA gave 0.144868 -- a ~77% relative difference, attributed to pegas's explicit Euclidean distance on raw genotype dosage vs poppr's different default AMOVA distance convention.

**2026-07-30 -- Marc-Olivier Duceppe:** accepted both open items -- see `docs/SCIENTIFIC_CONCORDANCE.md` for the full note. Concordance records themselves remain `approval: proposed`.

## Second execution: candidate `1.0.0-rc2` at commit `cad9619` (2026-08-20)

Rerun by Claude, on request, in the `popgenvcf` conda environment against the same MD5-verified chr22 source (reused from local cache, re-verified against the registry's upstream MD5s before use). Confirmed no relevant code diff between the 2026-07-30 run and this commit before treating a byte-for-byte reproduction as expected rather than coincidental.

**Every numeric result reproduces the July run exactly**, now bound to the current candidate:

| analysis | reference | role | result |
| --- | --- | --- | --- |
| pca | SNPRelate | equivalence | 10/10 canonical correlations within tolerance |
| ibs | SNPRelate | equivalence | 6,270,016/6,270,016 cells, mean\|abs error\| = 2.52e-16 |
| dapc | adegenet | equivalence | 5,008/5,008 pairwise rows, mean\|abs error\| = 1.59e-17 |
| fst | hierfstat | diagnostic | observed 0.081957 (hierfstat) vs 0.081714 (popgenVCF/SNPRelate), 0.30% relative difference |
| amova | pegas | diagnostic | observed (popgenVCF) 0.081714 vs pegas 0.144868, 77.3% relative difference -- same explained divergence as before |
| amova | poppr | diagnostic | 0.081714 vs 0.081714, exact -- see estimator-compatibility note below |
| ibs | PLINK 2 (KING) | diagnostic | 0/6,270,016 cells within tolerance, mean\|abs error\| = 0.924 -- expected, non-equivalent estimands |
| diversity | adegenet | diagnostic | 26/52 cells within tolerance -- Hobs exact, Hexp differs by the unbiased-correction factor, as before |
| pca | PLINK 2 | diagnostic | within tolerance (eigenspace diagnostic) |

This is a real end-to-end determinism check, not an assumption: same source bytes (MD5-verified), same seed/region/thresholds, same package version, and the actual observed values match to the digits shown in the July run's own writeup.

**A minor scope nuance found while reviewing, not a defect**: the `amova`/`poppr` record compares popgenVCF's own computed value against itself (`poppr::poppr.amova()` *is* the implementation `run_amova()` calls internally), so `observed == reference` by construction -- it verifies poppr's own version/reproducibility, not independent cross-tool agreement. The genuinely independent AMOVA cross-check is the `pegas` record. Worth deciding whether to relabel the poppr record's role/wording so a future reviewer doesn't mistake it for a second independent concordance check.

`inventory_complete: TRUE`, `release_ready: FALSE`, `approval: proposed` on every record -- unchanged pending review.

## Review boundary

Concordance records remain `approval: proposed` until the named reviewer independently verifies tool inventory, exact commands/versions/environment, estimator compatibility for each equivalence claim, every numerical comparison (not just the record-level `passed` flag), and that diagnostic disagreements are explained rather than hidden, then approves via a formal, signed review packet (see `inst/scripts/scientific_review_packet.R`). A successful execution alone cannot promote this proposal.

Marc-Olivier Duceppe
([ORCID 0000-0003-2130-0427](https://orcid.org/0000-0003-2130-0427))
is assigned to perform this review.

### Draft review-checklist walkthrough (prepared by Claude, 2026-08-20, pending reviewer confirmation)

Not a completed human review -- an assistant-prepared cross-check against the raw evidence JSON (full per-record commands, tolerances, interpretations, and numeric comparisons, not just the summary TSV), offered as a starting point for the named reviewer's own review. No item below is actually approved until the reviewer confirms it.

| item_id | draft status | notes |
| --- | --- | --- |
| `tool_inventory` | proposed approved | `required_tools`=[adegenet, hierfstat, pegas, PLINK 2, SNPRelate], `required_analyses`=[amova, dapc, diversity, fst, ibs, pca], `missing_tools`/`missing_analyses` both empty. Equivalence vs diagnostic role assignment matches `docs/SCIENTIFIC_CONCORDANCE.md`'s stated scope: only comparisons between implementations of the literal same estimator (SNPRelate PCA/IBS, adegenet DAPC classification) are `equivalence`; everything estimating a genuinely different quantity or using a different convention (FST estimator variants, AMOVA distance conventions, KING vs IBS, Hexp bias correction) is correctly `diagnostic`. |
| `command_and_version` | proposed approved | Every record's exact command string, reference tool version (hierfstat 0.5.11, adegenet 2.1.11, pegas 1.4, poppr 2.9.8, SNPRelate 1.44.0, PLINK v2.0.0-a.6.9LM), and environment (region `22:20000000-21000000`, seed 42, platform `x86_64-conda-linux-gnu`, R 4.5.3) are recorded per-record and match the contract. No explicit sample-order checksum field in this schema (unlike `ancestry_three_backend`'s), but sample order is implicitly bound by the deterministic pipeline (fixed seed, same source bytes, same QC/LD contract already reviewed for `production_baseline`) -- worth deciding whether this gate's schema should gain an explicit sample-order binding to match the ancestry gate's stronger guarantee. |
| `estimator_compatibility` | proposed approved, with one flagged nuance | Each `equivalence` record's interpretation states the two implementations should be numerically equivalent for a stated reason (same eigenspace, same pairwise coefficient definition, same classification workflow) -- all three passed at machine precision, consistent with that claim. **Nuance**: the `amova`/`poppr` diagnostic record compares popgenVCF's own value against itself (see note above) -- not a defect, but its role/wording may need clarifying so it isn't mistaken for an independent check. |
| `numerical_comparisons` | proposed approved | Inspected every record's actual observed/reference/error values (not just the `passed` boolean): three equivalence records at machine precision (2.5e-16, 1.6e-17, and full 10/10 canonical correlations); the six diagnostic records' non-passing or partially-passing cells (KING 0/6.27M, FST 0.30% relative, AMOVA 77.3% relative, diversity 26/52) all correspond to already-documented, expected mechanisms, not unexplained failures. No comparison errored or was silently skipped. |
| `diagnostic_interpretation` | proposed approved | Every diagnostic record carries a specific, non-generic `interpretation` string naming *why* it's diagnostic (e.g. "KING kinship and IBS-derived similarity are related but not identical estimands"), and every message is explicitly `"diagnostic difference recorded; comparison is non-gating"` -- no diagnostic result is presented as if it were an equivalence pass. |

**Recommendation for the real review note**: the evidence is genuine, reproducible, and self-consistent with the July review. The one substantive open item is the `amova`/`poppr` self-comparison wording/role nuance above -- a documentation clarity fix, not a scientific concern, and not blocking approval of the other items.

## Rebound to commit `50fb45a` (2026-08-22)

Rerun purely to bind evidence to the current candidate commit after the `benchmark_history` performance-regression fixes -- none of which touch this module's code path. Every numeric value reproduced exactly (same FST/AMOVA divergences, same three machine-precision equivalence passes), as expected. No change to the draft checklist above.
