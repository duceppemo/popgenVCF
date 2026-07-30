# External-tool scientific concordance

Phase 0.9.28 turns external-reference comparisons into reviewable, release-gated scientific evidence.

## Scope

A concordance record binds one canonical dataset and analysis to:

- the external implementation and exact version;
- the exact command or reproducible invocation;
- a named tolerance profile and scientific role;
- the full long-form numerical comparison table;
- environment and container provenance;
- interpretation and citations;
- an explicit proposed or approved state.

`equivalence` comparisons are release gating. Failed, skipped, errored, or unapproved equivalence records prevent release readiness. `diagnostic` comparisons preserve scientifically meaningful cross-method differences without incorrectly requiring numerical identity between methods that estimate different quantities.

## Required production inventory

The full-validation workflow should cover scientifically appropriate combinations of:

- PLINK 2 for PCA, allele-frequency, IBS, and distance checks;
- SNPRelate for PCA, IBS, and LD-sensitive checks;
- hierfstat for diversity and F-statistics;
- adegenet for PCA and DAPC-oriented checks;
- a documented practical AMOVA reference implementation.

Not every tool is authoritative for every analysis. Each comparison must state whether it tests implementation equivalence or supplies diagnostic context.

## Evidence

`write_scientific_concordance_evidence()` writes deterministic TSV, JSON, and Markdown methods artifacts. Production finalization uses `require_release_ready = TRUE`, which refuses evidence finalization unless every equivalence comparison passed and received explicit scientific approval.

Ordinary pull-request CI remains synthetic, offline, and fast. Tool installation, canonical dataset acquisition, command execution, logs, and approval-ready proposals belong in opt-in or scheduled full-validation CI.

## First execution and review (2026-07-30)

`scripts/run-external-concordance-synthetic.R` proved the harness end to end against the tiny synthetic validation fixture. `scripts/run-external-concordance-chr22.R` then pointed the same harness at the approved 1000 Genomes Phase 3 chromosome 22 source, reusing the bounded `22:20000000-21000000` interval and QC contract from the already-reviewed `production_baseline` proposal (2,504 samples, 2,028 QC SNPs, 350 LD-pruned SNPs).

Result: `inventory_complete: TRUE` (PLINK 2, SNPRelate, hierfstat, adegenet, pegas all represented); `release_ready: FALSE` pending named approval of the individual equivalence records below. All three applicable `equivalence` comparisons passed at machine precision: SNPRelate PCA (10/10 canonical correlations), SNPRelate IBS (6,270,016/6,270,016 cells), adegenet DAPC (5,008/5,008 cells). `vegan_mantel` was correctly excluded -- the real 1000 Genomes panel carries no latitude/longitude metadata.

Diagnostic comparisons: PLINK 2 KING vs IBS disagreed on essentially every pair, exactly as expected for non-equivalent estimands (non-gating by design). hierfstat FST (0.081957) and SNPRelate FST (0.081714) agreed within 0.024% -- markedly tighter than the ~29% gap seen on the tiny synthetic fixture, consistent with small-sample bias-correction differences washing out at real scale. adegenet diversity showed the same explainable split as the synthetic run: Hobs matched exactly, Hexp differed by the small-sample unbiased-correction factor, now across 26 real populations.

One diagnostic finding required explicit review: popgenVCF's own AMOVA value (poppr-based Phi-population-total, 0.0817) matched hierfstat's and SNPRelate's FST closely (0.0819, 0.0820), but pegas's independent AMOVA gave 0.1449 -- a ~77% relative difference, much larger than the near-exact agreement seen on the synthetic fixture. The likely cause is that the driver's pegas call uses an explicit Euclidean distance on raw genotype dosage while poppr's default AMOVA distance convention differs; this distinction was negligible at 7 loci but is material at 2,028 loci across 26 populations.

**2026-07-30 -- Marc-Olivier Duceppe:** accepted both open items. (1) `tool_inventory`, `command_and_version`, `estimator_compatibility`, and `numerical_comparisons` are reviewed and approved for the three equivalence records above. (2) The pegas/poppr AMOVA divergence is accepted as explained by differing distance conventions between the two implementations -- non-blocking, diagnostic-only, no rerun required. This review covers the concordance suite's checklist items only; the individual concordance records themselves remain `approval: proposed` pending a formal signed review packet (see `inst/scripts/scientific_review_packet.R`) before `release_ready` can become `TRUE`.
