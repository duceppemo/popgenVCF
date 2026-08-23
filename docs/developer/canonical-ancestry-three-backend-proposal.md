# Ancestry three-backend concordance proposal

This covers the `ancestry_three_backend` release gate (issue #24): ADMIXTURE, fastStructure, and LEA/sNMF are run independently across a declared K range and replicate schedule against the approved 1000 Genomes Phase 3 chromosome 22 canonical source, then assembled into a `canonical_ancestry_three_backend` evidence proposal (`R/canonical_ancestry_three_backend.R`). The execution creates a scientific-review proposal; it does not approve the gate or authorize a release.

## Fixed analysis contract

`scripts/run-ancestry-three-backend-proposal.R` uses a wider genomic interval than the 1Mb `production_baseline` window: `22:15000000-25000000` (~3,500 SNPs after LD pruning), not `22:20000000-21000000` (~350 SNPs). This was a deliberate, tested choice, not the same window reused by default: the narrower window gives only moderate cross-backend Q-matrix agreement (~0.83-0.89 alignment), while the wider one gives near-complete convergence (~0.97-0.99) across all three backends.

Default sweep: K=2:10, 5 replicates per backend per K, seed 42 (135 total backend runs). This takes several hours at full scale, so — matching `production_baseline` and `external_concordance` — it is a manual/scheduled production execution, not routine CI.

## Execution and evidence

Usage:

```text
run-ancestry-three-backend-proposal.R <output-dir> <work-dir> <source-dir>
    <candidate-id> <git-commit> <generated-at>
    [--k-range=2:10] [--replicates=5] [--region=22:15000000-25000000]
```

`<source-dir>` must already contain the approved, verified chromosome 22 source files. Requires `bcftools`, `plink2`, ADMIXTURE, fastStructure, and the `LEA` R package (sNMF) on `PATH`/installed — none of which is guaranteed on a bare CI runner, so this is normally run in a dedicated environment (e.g. this repository's `popgenvcf` conda environment, matching `inst/conda/environment.yml`).

The evidence proposal (`ancestry-three-backend-proposal.json`, checksum-covered by `ancestry-three-backend-SHA256SUMS.txt`) records:

- `dataset_id`/`dataset_version`/`region`/`sample_count`;
- `sample_order_sha256` — `sha256(paste(sample_ids, collapse = "\n"))`, binding every backend's Q-matrix rows to one exact sample order (raw per-replicate Q-matrices themselves are not persisted in the proposal JSON, only this checksum — the underlying `.Q`/`.meanQ` files stay in the work directory);
- `backend_evidence` — per-backend, per-K, per-replicate fit statistics (ADMIXTURE: CV error; fastStructure: marginal likelihood; sNMF: cross-entropy) plus stability/alignment summaries;
- `cross_backend_comparisons` — exactly one record per unordered backend pair, all evaluated at `selected_k`, each with alignment/correlation/cosine scores and RMSD;
- `k_selection` — per-backend recommended K (optimum + stability plateau) and the overall consensus K with an explicit agreement fraction and confidence level;
- `approval` (`proposed`/`approved`/`rejected`), `approved_by`, `approved_at`.

Every cross-backend comparison's `interpretation` field is a fixed, honest caveat: agreement is evidence of numerical/structural consistency between independent implementations, not proof that the inferred K or ancestry components are biologically correct.

## First execution (2026-08-19)

Generated locally (`run-ancestry-three-backend-proposal.R`) at 2026-08-19T14:25:15Z from commit `130d5dae2620b3cc7bd4464679bb44e12541a42d`, against the approved `1000g_phase3_chr22_v5a` (`20130502-v5a`) source (MD5-verified against the registry). 2,504 samples, region `22:15000000-25000000`, K=2:10, 5 replicates/backend/K, seed 42.

Confirmed unchanged (no relevant code diff) through commit `cad9619` — the current candidate — so this evidence remains valid without rerunning: `git diff --stat 130d5da..cad9619 -- R/canonical_ancestry_three_backend.R scripts/run-ancestry-three-backend-proposal.R` is empty.

**Backend-specific K recommendations disagree.** ADMIXTURE's CV error is minimized at K=10; fastStructure's marginal likelihood is maximized at K=5; sNMF's cross-entropy plateaus at K=6 (optimum nominally at K=10, but the plateau-detection logic selects the simpler K=6 model for negligible additional fit). The consensus procedure selects overall K=5 with only 1-of-3 backend agreement (33%, "moderate confidence"). This is a real, expected outcome of the checklist's own warning against inferring a single "true" K from a minimum/maximum statistic — not a defect.

**At the consensus K=5, all three backends agree well with each other structurally**, even though they individually preferred different K values: ADMIXTURE-fastStructure alignment 0.995, ADMIXTURE-sNMF 0.993, fastStructure-sNMF 0.983 (all above the 0.8 minimum threshold), each corroborated by independent correlation and cosine scores and consistent RMSD (0.05-0.10).

Real convergence, spot-checked directly against raw tool logs (not just summary statistics): `admixture_K5.log` — 19 iterations, 56.7s, CV error 0.42386; `faststructure.5.log` — 210 iterations, 224.4s, marginal likelihood -0.782364. Both match the JSON's recorded K=5 replicate means.

### `sample_order` verification (independent, not just trusted)

`sample_order_sha256` was independently recomputed in R directly from the retained raw PLINK `.fam` file (not from the JSON) using the exact algorithm in `R/canonical_ancestry_three_backend.R` (`digest::digest(paste(sample_ids, collapse = "\n"), algo = "sha256", serialize = FALSE)`), and matched the recorded value exactly (`9d33427b781b51aaa22c1c30eeccb1fcc357787a0219cd002e2703f7466530da`). A first attempt using a shell `awk | sha256sum` pipeline did not match, because `sha256sum` hashes the trailing newline `awk` emits after the last sample while the R `paste(..., collapse = "\n")` convention has none — caught and corrected before treating it as a real discrepancy.

### A real evidence-retention gap, found and not hidden

LEA/sNMF's on-disk cache (`ancestry-work/cache/ancestry/popgenVCF_snmf.snmf/`) retains only K=10's raw per-replicate output files locally; K=2-9's raw `.Q`/log files are not individually retained, only their summarized cross-entropy statistics in the proposal JSON. ADMIXTURE and fastStructure's raw per-K, per-replicate outputs are all retained. Whether this asymmetric retention is acceptable for this gate, or whether a rerun with full sNMF retention is needed, is a decision for the named reviewer.

## Review boundary

The proposal states `approval: proposed`; promotion requires the named reviewer to independently verify the exact candidate commit, source checksums, sample-order binding, replicate design and convergence, K-selection methodology, and cross-backend label-alignment evidence, then approve via a formal, signed review packet (see `inst/scripts/scientific_review_packet.R`). A successful execution alone cannot promote this proposal.

Marc-Olivier Duceppe
([ORCID 0000-0003-2130-0427](https://orcid.org/0000-0003-2130-0427))
is assigned to perform this review.

### Draft review-checklist walkthrough (prepared by Claude, 2026-08-20, pending reviewer confirmation)

Not a completed human review — an assistant-prepared cross-check against the raw evidence (proposal JSON, raw `.fam`/`.Q`/`.meanQ` files, and raw tool logs), offered as a starting point for the named reviewer's own review. No item below is actually approved until the reviewer confirms it.

| item_id | draft status | notes |
| --- | --- | --- |
| `same_biological_input` | proposed approved | All three backends run against `1000g_phase3_chr22_v5a`/`20130502-v5a`, region `22:15000000-25000000`, same approved MD5-verified source. PLINK `.bed`/`.bim`/`.fam` (2,504 samples x 3,586 SNPs) is the shared input feeding ADMIXTURE and fastStructure directly; LEA/sNMF's `.geno` cache is derived from the same retained sample/SNP set. |
| `sample_order` | proposed approved | Independently recomputed in R directly from the raw `.fam` file, matches the recorded `sample_order_sha256` exactly (see above). 2,504 unique, non-missing IDs, zero duplicates. |
| `replicate_design` | proposed approved, with one flagged gap | K=2-10, 5 replicates each, seed 42, for all three backends (135 total runs). Raw logs for K=5 spot-checked directly and match the JSON's summary statistics for both ADMIXTURE and fastStructure. **Gap**: LEA/sNMF's raw per-K files (K=2-9) are not individually retained on disk, only K=10's -- see the retention-gap note above. |
| `k_selection` | proposed approved, with the finding restated plainly | Backend-specific optima genuinely disagree (ADMIXTURE K=10, fastStructure K=5, sNMF plateau K=6); consensus lands on K=5 with only 33% backend agreement ("moderate confidence"). This is exactly what the checklist item warns against over-reading -- not evidence that K=5 is the biologically "true" number of ancestral populations, only the plateau-selected compromise. |
| `label_alignment` | proposed approved | At the shared K=5, all three pairwise alignments pass the 0.8 minimum threshold (0.995 / 0.993 / 0.983), each corroborated by independent correlation and cosine scores plus consistent RMSD. The Hungarian-algorithm label-matching itself was not reimplemented independently; verification relied on the retained per-replicate `.Q`/`.meanQ` files being present and structurally valid. |
| `biological_limits` | proposed approved | The record itself is already explicit and correctly hedged: every cross-backend comparison's `interpretation` field states that agreement is evidence of numerical/structural consistency, not proof of biological correctness of K or the inferred ancestry components. |

**Recommendation for the real review note**: five of six items are straightforward given the evidence is genuine and independently reproducible. The two decisions that actually need the reviewer's judgment: (1) whether the sNMF raw-file retention gap (K10-only) is acceptable as-is or needs a rerun with full retention, and (2) how to phrase the K-selection disagreement in eventual release-facing documentation so it reads as an honest limitation rather than a settled result.

## Rebound to commit `50fb45a` (2026-08-22)

Full sweep rerun (all 135 backend runs) purely to bind evidence to the current candidate commit after the `benchmark_history` performance-regression fixes -- none of which touch ancestry-backend code. Reproduced exactly: same per-backend K recommendations (ADMIXTURE=10, fastStructure=5, sNMF=6), same consensus K=5, same cross-backend alignment scores (0.995/0.993/0.983), and the `sample_order_sha256` independently recomputed and matched again. No change to the draft checklist above.

**2026-08-22 -- Marc-Olivier Duceppe:** reviewed and approved all 6 manual checklist items against this commit-`50fb45a` evidence. `same_biological_input`, `sample_order`, `label_alignment`, and `biological_limits` approved as verified. `replicate_design` approved as-is despite the sNMF K10-only raw-file retention gap -- accepted, no rerun required. `k_selection` approved with the backend disagreement stated plainly as an honest limitation: K=5 is the consensus procedure's output, not evidence of a biologically true ancestral population count; the real, defensible finding is the cross-backend structural agreement at K=5 (0.98-0.995 alignment). `ancestry_three_backend-gate-record.json` materialized (`status: passed`, checksum-bound to the retained evidence, `approval.state: approved`) via `scripts/write_scientific_review_gate_record.R`, ready to fold into a future release-candidate evidence index rebuild.
