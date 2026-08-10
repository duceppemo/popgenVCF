# Development Roadmap

The roadmap is governed by the [Project Charter](PROJECT_CHARTER.md). Scientific correctness and validation take priority over schedule or feature count.

## Completed foundation: 0.4–0.8

The completed foundation includes registry-managed analysis modules, serializable project state, deterministic QC and LD pruning, numerical and population-structure validation, reproducible environments, package CI, scientific-validation CI, and validated container publishing.

## Phase 8: deterministic execution and operational reliability

Phase 8 established the deterministic, recoverable, and scientifically auditable execution runtime.

### Completed milestones

- [x] **8.1 — Unified analysis execution engine**
- [x] **8.2 — Dependency-aware failure propagation and execution records**
- [x] **8.3 — Deterministic checkpoints and resume support**
- [x] **8.4 — Deterministic retry and recovery orchestration**
- [x] **8.5 — Deterministic execution timeouts**
- [x] **8.6 — Deterministic cancellation and graceful shutdown**
- [x] **8.7 — Resource policies and execution admission**
- [x] **8.8 — Supervised external-process execution**
- [x] **8.9 — Portable deterministic concurrent scheduling**
- [x] **8.10 — Execution hardening and stable runtime API**

## Phase 9: unified publication-quality module runtime

Phase 9 connected stable plugin, scientific-object, schema, cache, checkpoint, migration, validation, provenance, and publication contracts to the Phase 8 runtime.

### Completed implementation milestones

- [x] **9.1 — Finalize the module plugin contract**
- [x] **9.2 — Canonical result and publication-artifact contracts**
- [x] **9.3 — Canonical reusable scientific data objects**
- [x] **9.4 — Centralized schema validation and compatibility enforcement**
- [x] **9.5 — Deterministic scientific object caching**
- [x] **9.6 — Deterministic module execution planning and orchestration**
- [x] **9.7 — Deterministic execution checkpoints and recovery**
- [x] **9.8 — Unified module execution integration**
- [x] **9.9 — Executable vertical slice and Phase 8 runtime adapter**
- [x] **9.10 — First built-in module runtime integration**
- [x] **9.11 — First production analysis module migration**
- [x] **9.12 — Production module migration registry and staged cutover**
- [x] **9.13 — Legacy runtime deprecation and unified release readiness**
- [x] **9.14 — Final closure-review and roadmap-handoff contracts**
- [x] **9.14.1 — Closure evidence assembly and roadmap synchronization**

See [Phase 9 closure evidence and roadmap handoff](PHASE9_CLOSURE.md).

## Phase 10: stable public scientific interface

- [x] **10.1 — Canonical public analysis and artifact API**
- [x] **10.2 — Public API compatibility and release conformance**

## 0.10: publication-quality release candidate

The authoritative development package version is **0.10.0**.

### Completed implementation milestones

- [x] **0.9.1 — Deterministic publication report rendering**
- [x] **0.9.2 — Journal presets and deterministic publication layouts**
- [x] **0.9.3 — Accessible and grayscale-safe figure modes**
- [x] **0.9.4 — Deterministic submission packages and supplementary indexes**
- [x] **0.9.5 — Publication PCA and ordination outputs**
- [x] **0.9.6 — Publication DAPC outputs**
- [x] **0.9.7 — Publication FST outputs**
- [x] **0.9.8 — Publication diversity outputs**
- [x] **0.9.9 — Publication ancestry outputs**
- [x] **0.9.10 — Publication AMOVA outputs**
- [x] **0.9.11 — Publication isolation-by-distance outputs**
- [x] **0.9.12 — Publication spatial-genetics outputs**
- [x] **0.9.13 — Canonical real-data validation contracts**
- [x] **0.9.14 — Release-state and public-API reconciliation**
- [x] **0.9.15 — Public API contract hardening**
- [x] **0.9.16 — Canonical API baseline and CI enforcement**
- [x] **0.9.17 — Publication IBS and MDS outputs**
- [x] **0.9.18 — Canonical real-data materialization and external-comparison contracts**
- [x] **0.9.19 — Canonical dataset registry and approval gate**
- [x] **0.9.20 — First approved canonical dataset integration**
- [x] **0.9.21 — Canonical validation suites**
- [x] **0.9.22 — Quantitative canonical baseline metrics**
- [x] **0.9.23 — Longitudinal canonical drift detection**
- [x] **0.9.24 — Canonical scientific change approval and reconciliation**
- [x] **0.9.25 — Canonical release-readiness gate**
- [x] **0.9.26 — Canonical release bundle integration**
- [x] **0.9.27 — Canonical real-data baseline adoption contract**
- [x] **0.9.28 — External-tool scientific concordance contract**
- [x] **0.9.29 — Continuous release benchmarking contract**
- [x] **0.9.30a — Source-package and R CMD check hygiene** (#285)
- [x] **0.9.30b — Generated API documentation and canonical interface reconciliation** (#284)
- [x] **0.9.30.1 — User tutorials, scientific interpretation, deployment, and troubleshooting** (#288)
- [x] **0.9.30.2 — Publication narrative completeness and maintained gallery** (#68)
- [x] **0.9.30.3 — Citation, software, and reproducibility metadata reconciliation** (#291)
- [x] **0.9.30.4 — DOI-ready archival metadata, SBOMs, checksums, and provenance** (#297)
- [x] **0.9.31 — Release-candidate closure policy, reviewer dossier, and workflow** (#299)
- [x] **0.9.32 — Autosomal production validation execution and evidence** (#22)

### Phase 0.9.32 execution status

- [x] approve and checksum-pin the 1000 Genomes Phase 3 chromosome 22 source;
- [x] execute and retain candidate-bound structural validation for the complete chromosome 22 source;
- [x] define a bounded chromosome 22 QC, LD-pruning, and PCA proposal workflow;
- [x] execute the quantitative proposal workflow from the reviewed implementation and retain an importable, filename-bound proposal snapshot;
- [x] scientifically review and approve or revise the proposed metric values and tolerances;
- [x] complete external-tool concordance and remaining full-validation evidence.

These milestones complete the software, documentation, metadata, archival-readiness, and release-candidate decision contracts, and -- as of the 2026-08-01/2026-08-02 closure below -- the production real-data baseline, external-tool concordance suite, cross-backend ancestry evidence, release benchmark history, exact distribution evidence, and final 0.10.0 release authorization have all been executed, reviewed, approved, deposited, and published.

**2026-08-01 progress note:** Marc-Olivier Duceppe scientifically reviewed and approved all six registered chr22 autosomal baseline metrics (see `docs/developer/canonical-autosomal-baseline-proposal.md`); that evidence was already durably retained as a checksum-verified GitHub Actions artifact. In the same session, `external_concordance` and `ancestry_three_backend` were each executed for the first time against real chr22 data and reviewed (see `docs/SCIENTIFIC_CONCORDANCE.md` and `docs/user/ancestry-backends.md`), and `benchmark_history`'s evidence-generation pipeline was reconciled, executed through real CI, and reviewed (see `docs/CONTINUOUS_RELEASE_BENCHMARKING.md`). At that point none of the last three were checked off above: their evidence had not yet been produced through a durable, retained production path, their underlying evidence objects had not been formally transitioned from `approval: proposed` to `approved`, and `benchmark_history` was further blocked on this project's first published GitHub Release.

**2026-08-01 continued:** the missing `read_`/`approve_` functions for the ancestry and concordance evidence contracts were added (`read_canonical_ancestry_three_backend_evidence()`, `approve_scientific_concordance_record()`, `read_scientific_concordance_suite()`), and used to formally transition `production_baseline`, `external_concordance` (its three equivalence records), and `ancestry_three_backend` from `proposed` to `approved` against their already-recorded reviewer determinations. A new production evidence-index assembly script (`scripts/build_release_candidate_evidence_index.R`, the real-evidence counterpart to the synthetic-only `build_release_candidate_rehearsal.R`) checksum-bound that approved evidence into a real `release-candidate-evidence-index.json`, evaluated end to end through `build_release_candidate_dossier.R`: `release_ready: false`, `3 / 15` required gates passed. That index, its four gate-evidence artifacts, the dossier, and a freshly rehearsed source-release bundle were published as assets on this project's first GitHub Release, `0.10.0-rc1` (explicitly a pre-release, not v0.10.0) -- durably retaining the three approved gates publicly for the first time, and giving `release-benchmark-archive.yml` a real prior release to compare future benchmark runs against. `container.yml` was hardened to skip its GHCR publish step for GitHub pre-releases (it previously fired on every published release regardless of tag), so no container image was pushed for this checkpoint. `benchmark_history` remains `blocked`, not `passed` -- a real trend-comparison run against `0.10.0-rc1` has not yet been executed -- and the remaining 11 gates (software, distribution, archival, and the two final approval gates) remain `not_run`: most already have real CI evidence produced elsewhere in this repository, but none of it is wired into the checksum-bound evidence-index format yet.

**2026-08-01, release closure:** the remaining gates were wired in one at a time against real CI evidence -- `metadata_consistency`, `public_api_contract`, `source_package_check`, `scientific_validation`, `source_distribution`, `apptainer_distribution`, `canonical_validation`, `benchmark_history` (after fixing a missing `GH_TOKEN` bug that had silently broken every baseline-discovery attempt), and `oci_distribution` (a real registry push to GHCR) -- each verified end to end through `build_release_candidate_dossier.R`. Marc-Olivier Duceppe, as named scientific reviewer, then approved the complete scientific evidence set and, as release owner, authorized tagging, publication, container-image publication, and Zenodo deposition. The real GitHub Release `v0.10.0` was tagged and published (superseding the `0.10.0-rc1` pre-release, which was deleted once the real release existed), Zenodo deposition completed, and `archival_assets` closed out the dossier: **`READY`, 15 / 15 required gates passed**, DOI [10.5281/zenodo.21747548](https://doi.org/10.5281/zenodo.21747548) (concept DOI [10.5281/zenodo.21747067](https://doi.org/10.5281/zenodo.21747067)), 2026-08-01.

**2026-08-01/2026-08-02, generic tooling and issue closure:** the hand-authored, 0.10.0-specific evidence-index script was replaced with a generic, self-describing `<gate_id>-gate-record.json` fragment contract (`inst/scripts/release_candidate_gate_record.R`) that any CI workflow or reviewer script can write. Six CI workflows now write their own fragments as a normal part of their run; `.github/workflows/release-candidate-collector.yml` dispatches and collects all six in parallel for a future candidate ref; `scripts/write_scientific_review_gate_record.R` lets the named reviewer materialize a determination for the 4 gates on their standing assignment (`production_baseline`, `external_concordance`, `ancestry_three_backend`, `benchmark_history`) without hand-computing checksums. Re-run against the real `v0.10.0` evidence, the new tooling independently reproduced the same `12 / 15` result the hand-authored path had proven, with the 3 one-off, release-specific gates (`scientific_approval`, `release_authorization`, `archival_assets`) correctly left `not_run` for a not-yet-cut future candidate. With the release itself closed out, issues #1, #22, #24, and #43 were closed against this real evidence; #43 was closed with an explicit note that the `canonical`/`medium`/`large` benchmark-dataset tiers and a trend dashboard remain unimplemented, deferred rather than dropped (see "Validation datasets and benchmarking" below). #4 remains open as the umbrella tracker for the still-open 1.0 decision (see "1.0: stable scientific release").

### Completed stabilization gate

Repository health and release-candidate infrastructure are reconciled:

- [x] eliminate false release-readiness paths in concordance and benchmark evidence;
- [x] replace runtime namespace mutation with explicit public exports;
- [x] report roxygen, NAMESPACE, Rd, API-baseline, release-metadata, LICENSE, Zenodo, and action-pin drift deterministically;
- [x] synchronize README, NEWS, roadmaps, and issue trackers;
- [x] retire obsolete competing roadmap material;
- [x] eliminate avoidable package-check notes and source-package hygiene defects;
- [x] establish deterministic roxygen generation and a 613-entry canonical installed API baseline;
- [x] pin external GitHub Actions to immutable commits with controlled update automation;
- [x] generate source and OCI SBOM/provenance evidence and checksum-linked archival manifests;
- [x] define a checksum-verified 15-gate production evidence contract and deterministic reviewer dossier;
- [x] run the complete relevant CI matrix on the reconciled state.

### Completed Phase 0.9.30 sequence

1. **Opening maintenance**
   - [x] eliminate avoidable `R CMD check` notes and source-package hygiene defects (#285);
   - [x] reconcile roxygen ownership, generated `NAMESPACE`/Rd files, S3 registrations, and the canonical API baseline (#284).
2. **Documentation, metadata, and archival readiness**
   - [x] complete the first-analysis tutorial, scientific interpretation guide, troubleshooting guide, reproducibility guide, and GHCR/Apptainer/HPC deployment guidance (#288);
   - [x] complete the maintained publication gallery and end-to-end narrative integration (#68);
   - [x] reconcile citation, installed-package, CodeMeta, FAIR software, and reproducibility metadata with development-safe release boundaries (#291);
   - [x] add DOI-ready Zenodo metadata, source and OCI SBOMs, provenance evidence, release checksums, manifests, and archival instructions (#297).

### Completed Phase 0.9.31 closure mechanism

- [x] define a canonical 15-gate release-candidate policy;
- [x] require exactly one checksum-verifiable evidence record per gate;
- [x] distinguish deliberately blocked rehearsal evaluation from production evaluation;
- [x] require named reviewer identity and review dates for approval-gated evidence;
- [x] reject missing, duplicate, malformed, traversing, resized, or checksum-mismatched evidence;
- [x] generate deterministic gate, blocker, artifact, JSON, Markdown, and terminal checksum records;
- [x] add a pull-request rehearsal and manual production-evaluation workflow;
- [x] document backend-specific ADMIXTURE, fastStructure, and LEA/sNMF installation and provenance;
- [x] preserve the 613-entry public API and all publication boundaries.

### Completed 0.10.0 production evidence and publication

- [x] execute full canonical validation and external-tool concordance (#22);
- [x] approve the production quantitative baseline and real-data cross-backend ancestry evidence (#22, #24);
- [x] publish approved release benchmark history and supporting trend evidence (#43);
- [x] validate the exact source, OCI, and Apptainer distribution artifacts from clean environments;
- [x] assemble and review the complete archival evidence release;
- [x] evaluate a production dossier for the exact candidate commit and obtain `READY` status;
- [x] obtain named scientific approval and final release authorization;
- [x] tag, publish, deposit, and assign the real DOI (#1).

v0.10.0 is published: DOI [10.5281/zenodo.21747548](https://doi.org/10.5281/zenodo.21747548), 2026-08-01. The production dossier for the released commit reports `READY`, 15 / 15 required gates passed.

### Validation datasets and benchmarking

- [x] retain tiny synthetic fixtures in every CI run;
- [x] adopt a licensed, checksum-pinned canonical dataset under an approved registry entry;
- [x] define production baseline, scientific concordance, performance-budget, and release-evidence contracts;
- [x] execute and retain the first reviewable production quantitative baseline proposal from the canonical real dataset;
- [x] scientifically approve or revise the production quantitative baseline proposal;
- [x] execute and publish complete external-tool scientific concordance evidence;
- [x] execute and approve a real-data three-backend ancestry validation case;
- [x] publish runtime, memory, scaling, and historical regression artifacts per release;
- [ ] publish or externally host a checksum-pinned medium or large benchmark tier when licensing and storage policy permit (deferred; `canonical`/`medium`/`large` tiers remain unimplemented placeholders in `R/benchmark_datasets.R`, and no trend dashboard has been published -- see the 0.10.0 release note above, "0.9.29", and the #43 closure comment).

### Documentation and metadata

- [x] first-analysis tutorial, scientific interpretation, troubleshooting, reproducibility, and deployment/HPC guide set;
- [x] maintained publication figure gallery and end-to-end narrative example;
- [x] backend-specific ancestry installation, configuration, smoke-check, and provenance guidance;
- [x] canonical `CITATION.cff`, `codemeta.json`, installed citation, FAIR software identity, and reproducibility statement;
- [x] DOI-ready, development-safe Zenodo deposition metadata;
- [x] source and OCI SBOMs, checksums, provenance, and archival verification instructions;
- [x] release-candidate evidence-bundle and reviewer-dossier operating guidance;
- [x] record the real release date, DOI, concept DOI, and archive identifiers after successful publication.

## Open tracking issues and deferred enhancements

- **#4 — Publication-quality platform:** the only remaining open tracker; umbrella issue for the explicit 1.0 decision (see below). Its 0.10.0 checklist is fully satisfied by the released state.
- **#22, #24, #43, #1:** closed 2026-08-02 against the real, checksum-verified v0.10.0 evidence and the `READY` 15 / 15 production dossier. #43 was closed with a note that the `canonical`/`medium`/`large` benchmark tiers and a trend dashboard remain deferred, unimplemented follow-up work, not a release blocker.

## 1.0: stable scientific release

Release 1.0 requires stable CLI, YAML, R API, module and output contracts; validated core modules and canonical real-data results; a complete report engine; validated container and Apptainer artifacts; complete documentation and citation metadata; and reproducible release artifacts with checksums, SBOM, provenance, and persistent archive identifiers.

As of the released v0.10.0 state (2026-08-01), every one of those technical criteria is met: the Phase 10 public API/CLI/YAML contracts are stable and CI-enforced; canonical real-data validation, external-tool concordance, and cross-backend ancestry evidence are approved; the Phase 9 publication report engine is complete; container and Apptainer distributions are validated; documentation and citation metadata are complete; and release artifacts (checksums, SBOMs, provenance, DOI) are real and published. What remains before 1.0 is not a technical gate but an explicit decision: 1.0 is a semver commitment (the public API is now stable; future breaking changes require a major version bump), not something CI can prove on its own. Tracked in #4.

## Post-0.10.0 feature development (in progress toward 1.0)

The maintainer chose to add features before making the 1.0 decision above. This section tracks that work as it lands, kept current rather than left to drift the way the pre-reconciliation roadmap did.

- **2026-08-02 -- DAPC and PCA SNP-loading Manhattan plots, rank-ordered loading plots, and top-contributing-SNP tables.** For DAPC: per configured K, `adegenet::dapc()`'s `var.contr` (contribution to each discriminant function) is exposed as `15_DAPC_loadings_manhattan_K<k>`/`16_DAPC_loadings_ranked_K<k>` figures and a `22f_DAPC_loadings_K<k>.tsv` top-N table (`analyses.dapc_loading_top_n`, default 20). For PCA: `SNPRelate::snpgdsPCASNPLoading()`'s signed per-SNP correlations are exposed as `17_PCA_loadings_manhattan`/`18_PCA_loadings_ranked` figures and a `31_PCA_loadings.tsv` top-N table (`analyses.pca_loading_top_n`, default 20; ranked by magnitude, not raw signed value). Neither adds new model fitting -- both reuse results the existing analyses already compute. No public API changes. See `NEWS.md` for full detail on the real data-shape issues found and fixed during implementation (adegenet's `var.contr` rownames being positional indices rather than locus names; `snpgdsPCASNPLoading()` returning a strict subset of requested SNPs).
- **2026-08-02 -- Per-population Hardy-Weinberg equilibrium testing and private-allele detection.** Chosen after auditing the package's analyses against a standard population-genomics toolkit and confirming both were genuinely absent (no `hwe`/`hardy` anywhere in `R/`; a `private_alleles` slot existed only in the unused, not-wired-in `publication_diversity_outputs.R` manifest layer). Both are reporting-only additions to the `diversity` analysis -- no new QC gate, no SNP filtering. HWE via `SNPRelate::snpgdsHWE()` (Wigginton et al. 2005 exact test) per population; private alleles via the standard presence/absence definition (matches hierfstat's/poppr's `private.alleles()`). New outputs: 4 new `09_population_diversity.tsv` summary columns, 3 new `10_population_locus_diversity.tsv` per-locus columns, a new filtered `32_private_alleles.tsv`, and two new figures (`19_HWE_pvalues`, `20_private_alleles`). New `analyses.hwe_alpha` config key (default 0.05). No public API changes. See `NEWS.md` for full detail, including the hand-verified synthetic-GDS test fixture covering the rare "private to one population because every other population is fully missing at that locus" edge case.
- **2026-08-02 -- Made the manuscript report surface the DAPC/PCA loading and HWE/private-allele outputs above, and fixed a real data-loss bug found in the process.** `run_module_pca()` was storing the analysis result as `pca[c("scores", "variance")]`, silently dropping the computed `loadings` before they ever reached `analysis_results.rds`; fixed. Added 3 new NULL-safe report sections ("Private alleles", "PCA SNP loadings", "DAPC SNP loadings"), each capped at 20 rows with a pointer to the complete on-disk table. The figure gallery and the diversity summary counts needed no changes -- both were already showing the new outputs automatically (dynamic directory globbing and new columns on an already-rendered table, respectively), confirmed by rendering a real report before assuming anything was missing.
- **2026-08-02 -- Wired the `CODECOV_TOKEN` secret into `test-coverage.yaml`.** The badge had been stuck on "unknown": `covr::codecov()`'s tokenless upload was rate-limited (confirmed from real run logs). Switched to `covr::to_cobertura()` + `codecov/codecov-action` (pinned to `v5.5.5`'s real commit SHA); verified end to end (real token used, badge went from "unknown" to a real percentage), then tightened `fail_ci_if_error` to `true`.
- **2026-08-02 -- Fixed a real regression this session introduced**: `plot_diversity()` unconditionally referenced `hwe_pvalue` on `div$locus`, breaking two pre-existing tests that build a minimal `div` fixture without a `locus` element -- caught by CI, not locally, because the session's own aggregate test-verification script only ever checked `sum(df$failed)`, never `sum(df$error)` (an uncaught R error inside `test_that()` sets `error=TRUE`, `failed=0`). Both are now checked on every verification pass in this repository going forward.
- **2026-08-02 -- Canonical/medium/large continuous-benchmark dataset tiers.** `scripts/build_release_benchmark_archive.R` previously only ever produced the `synthetic` tier despite 4 being declared. `canonical` reuses the real, already-approved chr22 source and the same bounded region `production_baseline` uses (no new data decision); `medium`/`large` are larger *synthetic* datasets, an explicit scoping decision -- no medium/large real dataset is registered/approved anywhere in this repository, and sourcing one is a separate data-governance decision. All 4 tiers are opt-in (`POPGENVCF_BENCHMARK_TIERS`, default `synthetic`; `release-benchmark-archive.yml`'s `benchmark_tiers` workflow_dispatch input), so routine pull-request/tag-push CI is unaffected. Sizes came from real local timing measurements, not guesses. See `docs/CONTINUOUS_RELEASE_BENCHMARKING.md` for the measured timings and `NEWS.md` for full implementation detail.
- **2026-08-08 -- Pairwise sample kinship/relatedness (KING-robust).** Flagged during a gap audit as a genuine missing analysis distinct from the existing IBS analysis: IBS answers "how similar are these samples overall," kinship answers "are these two individuals related, and how closely" -- the standard upstream check for cryptic relatives, which silently biases FST, diversity, and structure analyses computed downstream. Uses `SNPRelate::snpgdsIBDKING()` (Manichaikul et al. 2010, already an SNPRelate dependency). New `kinship_module_spec()` writes the kinship/IBS0 matrices, a full pairs table classified into KING's standard relationship degrees, and a filtered close-relatives table (new `analyses.kinship_close_relative_threshold` config key, default 0.0442); new `21_kinship_heatmap`/`22_kinship_IBS0_vs_kinship` figures. New `analyses.kinship` config key (default `TRUE`, needs only genotypes). Important interpretive finding from real chr22 data: KING-robust kinship is bounded above at 0.5 but *not* bounded below at -0.5 for highly divergent population pairs (real values as low as -8 observed on the pipeline's own LD-pruned SNPs), so validation only enforces the upper bound. No public API changes. See `NEWS.md` for full implementation detail.

- **2026-08-08 -- Runs of homozygosity (ROH) and per-sample FROH.** The other genuine gap flagged alongside kinship: diversity's `inbreeding_coefficient` is a population-level FIS summary and kinship is pairwise, but neither captures per-sample, per-genomic-region homozygous stretches. Uses `bcftools roh` (Narasimhan et al. 2016, HMM-based) -- `bcftools` is already a required `SystemRequirements` dependency, so this adds no new dependency (the only CRAN alternative, `detectRUNS`, was checked and rejected: last updated 2019, compiled code, 6 dated transitive dependencies). Deliberately does not apply the fixed MAF filter before ROH calling (known in the literature to bias run lengths downward), reusing only the missingness half of the QC contract. New `roh_module_spec()` writes `37_ROH_runs.tsv`/`38_ROH_sample_summary.tsv` and `23_ROH_length_distribution`/`24_ROH_FROH_by_sample` figures. New `analyses.roh` (default `TRUE`) and `analyses.roh_gt_error_phred` (default 30) config keys. Important interpretive point: `FROH` is scaled to the analyzed genomic footprint, not the whole genome, since this package accepts arbitrary VCF inputs. No public API changes. See `NEWS.md` for full implementation detail.

- **2026-08-08 -- Bundled real quickstart dataset, wiki/vignette example figures, and a committed example PDF report.** Lets a prospective user see real output, and reproduce it locally in a few minutes, before installing anything for their own data. New `inst/extdata/quickstart/` (160 real samples, 8 populations, same already-approved chr22 source/region other gates use; deliberately includes two known real duplicate/MZ-twin pairs, one of them cross-population) and exported `quickstart_dataset_paths()`. One real reference `run_pipeline()` execution is the single source for every figure/table shown in the wiki, the new `quickstart` vignette, and `docs/examples/chr22-quickstart-report.pdf`. Rendering that reference report surfaced and fixed two real, previously-latent LaTeX defects in the PDF path that small fixtures never triggered: a "too many unprocessed floats" compile failure once every per-K DAPC/PCA figure was included, and wide result tables overflowing the page width into illegible text. See `NEWS.md` for full detail.

- **2026-08-08 -- Sliding-window genome scans (windowed FST and diversity).** The other genuine gap flagged alongside kinship and ROH: everything else this package computes is either genome-wide or per-chromosome-summary, with no positional resolution within a chromosome to spot localized differentiation/diversity outliers. `run_genome_scan_fst()` computes only the global FST per window (not the full pairwise matrix, disproportionate for an exploratory scan); `run_genome_scan_diversity()` aggregates the diversity module's already-computed per-locus table into the same windows -- no new per-locus computation. New `genome_scan_module_spec()` writes `39_genome_scan_fst.tsv`/`40_genome_scan_diversity.tsv`/`41_genome_scan_FST_outliers.tsv` and two new Manhattan-style figures, reusing the existing `manhattan_layout()` helper. New `analyses.genome_scan`/`genome_scan_window_bp`/`genome_scan_step_bp`/`genome_scan_min_snps` config keys. Required adding the module to `R/metadata_capabilities.R`'s centralized capability-gating lists (module availability isn't decided by the module spec's own `enabled` field in this codebase). No public API changes. See `NEWS.md` for full implementation detail, including two real bugs found and fixed during verification.

- **2026-08-08 -- Real geographic coordinates for the quickstart dataset, and a real isolation-by-distance demonstration.** The quickstart dataset previously had no coordinates, so Mantel/isolation-by-distance correctly skipped for it. Added real, documented population-level collection-site coordinates (source: `igsr/1000Genomes_data_indexes` `README_populations.md`) for all 8 quickstart populations -- not invented values. GBR/ITU/STU share one representative UK point since the source names no more specific city for any of the three (ITU/STU are genuinely UK-collected diaspora cohorts). Sample selection, the VCF, and its index are unchanged; only the metadata gained `latitude`/`longitude` columns. A real `run_pipeline()` re-run confirms a genuine, statistically significant signal: Mantel r = 0.3129, p = 0.001, positive slope, R² = 0.137, across 12,720 real sample pairs. Found and fixed a real, pre-existing latent bug in `scripts/derive-quickstart-dataset.R` while regenerating the dataset (`fread()`'s automatic header detection misfired on the upstream 1000 Genomes panel file). All downstream docs (wiki, vignette, example PDF report) now show the real figure and values instead of text-only guidance. No public API changes. See `NEWS.md` for full detail.

- **2026-08-08 -- Renamed 35 escalating-`z`-prefixed `R/` source files to descriptive names.** Verified first that the scheme (`z-`, `zz_`, ..., up to `zzzzzzzzzz_`) bought nothing: no `Collate` field in `DESCRIPTION`, and none of the files contain load-order-dependent top-level code -- confirmed by inspection, not assumed. `R/zzz.R` (the standard R-package idiom for `utils::globalVariables()`) was left alone. Fixed the real cross-references found by a repo-wide search, including a `release-metadata.yml` trigger path that would have silently stopped firing on changes to the renamed file. Regenerated `NAMESPACE`/`man/*.Rd` with roxygen2 8.1.0 (matching CI's unpinned install) via the repository's exact documented sequence; `NAMESPACE` is byte-identical. No public API changes. See `NEWS.md` for full detail.

- **2026-08-08 -- Fixed documentation that had drifted behind the kinship, ROH, and genome-scan features added earlier in this cycle.** `DESCRIPTION`, `README.md`, `wiki/User-Guide.md`, `vignettes/getting-started.Rmd`, and `inst/doc/architecture.md` were all missing or misstating these three analyses (e.g. describing kinship and ROH as needing population metadata, which neither does). Verified the correct classification against the actual module code (`run_module_kinship()`, `run_module_roh()`, `run_module_genome_scan()`, `template_config()`) before fixing rather than guessing. See `NEWS.md` for full detail.

- **2026-08-08 -- Fixed report figure gallery ordering for per-K figure series.** `report_figure_inventory()` sorted figures with a plain lexicographic string sort, so `11_DAPC_K10` placed right after `11_DAPC_K1`-prefixed names, before `K2`-`K9` -- confirmed as a real, user-visible bug in the already-shipped example report ("Figure 13: DAPC K 10" before "Figure 14: DAPC K 3"). Added a natural-sort helper and re-rendered the real committed quickstart report from its cached analysis results to confirm the fix and ship the corrected PDF. See `NEWS.md` for full detail.

- **2026-08-09 -- Fixed the same lexicographic-sort bug in three more places, all real for multi-chromosome VCFs.** Reported against the PCA/DAPC SNP-loading figures (`PC10`/`LD10` faceted right after `PC1`/`LD1`, before `PC2`-`PC9`/`LD2`-`LD9`, since `ggplot2::facet_wrap()` alphabetizes character columns). The same root cause turned out to affect `manhattan_layout()` (shared by every Manhattan-style figure), `chromosome_summary.tsv`'s chromosome order, and the genome-scan/ROH tables' chromosome order -- none visible in this package's single-chromosome test data, but real for any genome-wide human VCF (would order chromosomes 1, 10, 11, ..., 2, 20, ..., 9, X, Y instead of 1-22, X, Y). Added a shared `natural_sort_key()`/`natural_sort_levels()` helper and applied it consistently. No public API changes. See `NEWS.md` for full detail.

- **2026-08-09 -- Added the popgenVCF version to the manuscript report.** `PopgenVCFAnalysis` already recorded `package_version`, but the report template never displayed it, despite already claiming to. Now shown as "Generated with popgenVCF vX.Y.Z" in the Executive Summary of both the HTML and PDF report. See `NEWS.md` for full detail.

- **2026-08-09 -- Published the Wiki.** `scripts/publish-wiki.sh --push` had never been run against this cycle's changes, so the live GitHub Wiki was missing all 9 documentation figures and several pages of content, unlike the pkgdown vignette site (which rebuilds automatically). Published with the maintainer's explicit confirmation. Remains a manual, maintainer-triggered step -- no workflow runs it automatically.

- **2026-08-09 -- Added the PCA/DAPC loading figures to the wiki and `interpreting-results` vignette.** Both sections' prose already referenced "the Manhattan/ranked loading figures" but never embedded them. Added from the same real reference pipeline run used for every other quickstart figure; confirmed the pre-existing scatter-plot figures were byte-identical before and after the natural-sort fix, confirming only the loadings figures were actually affected. Republished the Wiki with these additions.

- **2026-08-09 -- Basepair-position tick marks for the PCA/DAPC loading Manhattan plots.** Previously one uninformative chromosome-name tick per chromosome. New `manhattan_bp_breaks()` (`R/utils.R`) computes compact, Mb-formatted breaks per chromosome, with the total tick budget divided across chromosomes so genome-wide multi-chromosome data stays readable. Scoped to only these two plots. See `NEWS.md` for full detail.

- **2026-08-09 -- Reworked the wiki/vignette DAPC and ancestry-backends sections to show automatic K/cluster-number selection first, then only the selected model's figures.** DAPC switched from a hardcoded, unreproducible K=8 (RMSE above threshold) to the real consensus K=3 (RMSE effectively 0). Ancestry backends gained real ADMIXTURE figures for the first time (previously none), with an explicit reminder that these backends are not run by default -- generated via a one-off enabled config in a documentation-generation script only; `default_config()` is unchanged. See `NEWS.md` for full detail.

- **2026-08-09 -- Fixed `docs/examples/README.md`'s regeneration instructions**, which had gone stale as a side effect of the ADMIXTURE demonstration above: they claimed no non-standard settings were used, but the committed PDF was generated with ADMIXTURE force-enabled. Added the exact settings used and a note that the quickstart vignette (pure defaults) will not reproduce this exact file. See `NEWS.md` for full detail.

- **2026-08-09 -- Extended the ancestry-backends demonstration to fastStructure and sNMF**, matching the ADMIXTURE treatment: cluster-number-selection figure first, then only the consensus K's figures. The three backends' independently-selected consensus K values differ (7, 3, 6) -- called out explicitly as a real illustration of why this section already advised against treating cross-backend agreement as automatic. `docs/examples/README.md` and the committed example PDF (129 pages) updated accordingly. See `NEWS.md` for full detail.

- **2026-08-09 -- Added the genome-scan figures to the wiki and vignette.** The "Genome scans" section was text-only despite genome scans being a default-on module already shown in the committed report. Real quickstart values: FST windows range 0.051-0.172; window diversity ranges 0.099-0.398 across 8 populations. Already present in the committed example PDF's automatic figure gallery; only the hand-curated wiki/vignette selection was missing them. See `NEWS.md` for full detail.

- **2026-08-10 -- Added windowed Tajima's D (Tajima 1989) to the `genome_scan` module.** Natural extension of the existing windowed-FST/windowed-diversity engine: new `segregating_sites`/`tajima_d` columns on the same window grid, reusing already-computed per-locus data (no new per-locus computation). Formula independently verified against a published worked example before shipping (n=10, S=16, pi=3.888889 -> D=-1.446172, matched to 6 decimal places for both the final statistic and every intermediate correction constant). Haploid sample size held constant per population per window, matching vcftools' own `--TajimaD` simplifying assumption. New `26b_genome_scan_tajima_d_manhattan` figure; no new config keys. Real quickstart result: 152/160 windows have a defined D, almost entirely positive (median 1.34) -- flagged honestly as an expected artifact of this pipeline's own MAF QC filter (which removes rare variants before the scan runs, a well-known source of upward Tajima's D bias), not necessarily a real selection/demography signal. No public API changes. See `NEWS.md` for full detail.

- **2026-08-10 -- Added Nei's (1972) standard genetic distance between populations and a population-level neighbour-joining tree.** Surfaced by inspecting the existing `tree` module closely: it builds its NJ tree from IBS distance between individuals, not from allele-frequency-based distance between populations -- a genuinely different, standard "relationships among populations" deliverable. New `population_tree` module uses `adegenet::dist.genpop()` (already a dependency), building the `genpop` object directly from `compute_diversity()`'s already-computed allele counts. Verified two ways: an independent hand calculation of Nei's D matched adegenet's output to 7 decimal places, and a synthetic fixture with known population allele frequencies recovered the correct monotonic distance ordering. The real quickstart result is a strong additional correctness signal: LWK-YRI (both African) is the closest pair (D=0.0112), and the largest distances are all African-vs-non-African pairs -- the expected continental-structure pattern from real data, not a test assertion. Caught and fixed a real bug during the reference-pipeline verification run that the local `pkgload::load_all()`-based test suite had been masking: `methods::new("genpop", ...)` needs `@importClassesFrom adegenet genpop` to resolve correctly under a real installed-package load -- the second time this session a real bug was only caught by insisting on a real library()-based reference run rather than trusting the faster dev loop. Refreshed the public API baseline (634 -> 635 entries). No public API changes beyond the new export. See `NEWS.md` for full detail.

- **2026-08-10 -- Added LD-based contemporary effective population size (Ne), a natural follow-on to the LD-decay module added just below (same underlying r-squared computation, opposite pair-selection strategy).** New `ne_ld` module implements Waples (2006)/Waples & Do (2008)'s "LDNe" method per population: mean r-squared between cross-chromosome (unlinked) SNP pairs, bias-corrected for sampling noise via a piecewise formula in harmonic-mean sample size, converted to Ne via the standard quadratic formula. Both formulas independently cross-checked against multiple secondary sources before shipping, not just recalled or self-consistency-checked. New `45_Ne_LD.tsv`/figure and `analyses.ne_ld_max_snps` config key (default 2000, bounding the O(n^2) full pairwise-LD computation). Deliberately shipped without a jackknife CI: the point estimate and bias correction were verified with high confidence, but NeEstimator's own parametric CI construction could not be independently confirmed -- an honest, documented v1 limitation rather than a plausible-looking but unverified formula. Verified against a real Wright-Fisher forward simulation with a known true Ne (not just formula algebra): true N=50 recovered as 38-48 across 3 seeds, true N=200 as 175-202. The quickstart dataset's chr22-only (single-chromosome) autosomal marker set can't exercise a cross-chromosome method, so every population correctly reports an honest "insufficient data" skip on the real reference run -- documented as text-only, no fabricated demo, the same treatment already given to Mantel/IBD and sex_check before their respective missing data was added. Refreshed the public API baseline (633 -> 634 entries). No public API changes beyond the new export. See `NEWS.md` for full detail.

- **2026-08-10 -- Added linkage-disequilibrium decay and rarefaction-corrected allelic richness, the next two gaps found by the same standard-toolkit audit that previously surfaced kinship, ROH, and genome scans.** LD decay: new `ld_decay` module reports mean r-squared by physical distance bin via `SNPRelate::snpgdsLDMat()` (already a dependency), computed on the unpruned QC-passing SNP set (the LD-pruned set would systematically underestimate it by construction); new `43_LD_decay.tsv`/figure and `analyses.ld_decay*` config keys. Allelic richness: `hierfstat::allelic.richness()` (Suggests-only, skips transparently like the LEA/ADMIXTURE/fastStructure backends when not installed) adds a new `allelic_richness`/`mean_allelic_richness` column to the existing diversity tables plus a new `44_allelic_richness` figure -- no new table files. Along the way, investigated and confirmed *not* a live bug (though worth documenting) that `SNPRelate::snpgdsGetGeno()` silently returns genotypes in GDS native storage order regardless of requested order, same behavior class as the chromosome Y call-rate finding below; and fixed two small real staleness gaps this same audit surfaced: `inst/example_config.yml` was missing the chromosome-Y sex-check keys, and the report `skeleton.Rmd`'s sex-check row-priority sort omitted `discordant` (silently ranking it below plain matches). Refreshed the public API baseline (614 -> 633 entries; it had drifted unrefreshed since before kinship/ROH/sex_check/genome_scan were added). Verified end to end: real quickstart LD decay falls from r-squared 0.238 at 0-5kb to under 0.04 by ~50kb; real allelic richness ranges 1.82-1.94 across the 8 (equally-sized) quickstart populations. PNG-diffed against the prior reference run: only the two new figures changed, everything else byte-identical. Committed example PDF now 136 pages. No public API changes beyond the two new exports. See `NEWS.md` for full detail.

- **2026-08-10 -- Extended sex_check with chromosome Y call rate as a corroborating signal, and found/fixed two more severe pooling bugs affecting any real user's whole-genome VCF.** Y-chromosome genotype call rate (`SNPRelate::snpgdsSampMissRate()`, no new dependency) gives an almost perfectly binary second signal (real 1000 Genomes data: females exactly 0.0, males 0.995-1.0). `run_sex_check()` combines it with the existing X signal: agreement is high-confidence, confident disagreement is reported as `discordant` rather than averaged away. New `analyses.sex_check_y_*` config keys. Added real chromosome Y data to the quickstart dataset (male-only source, female samples padded as explicit correctly-missing genotypes via `bcftools merge`), which surfaced two pooling bugs more severe than the chromosome X kinship one: (1) `harmonize_samples()` computed sample-level missingness across all SNPs including chromosome Y, silently dropping every sample of one sex out of the *entire pipeline* (chromosome Y's ~100% cross-sex "missingness" is expected biology, not a QC failure); (2) `variant_qc()` likewise failed every chromosome Y SNP out of QC for the same reason. Fixed with a `snp_ids` restriction on `harmonize_samples()` and a `sex_limited_chromosome_names` exemption on `variant_qc()`'s missingness check (MAF filtering unaffected). Verified end to end: all 160 samples retained, 60,468 real chromosome Y SNPs available to sex_check, known duplicate pairs' kinship unaffected. Real result on the quickstart data: combining X+Y resolved all 40 chromosome-X-alone `ambiguous` calls into confident matches, and correctly flagged 5 samples as `discordant` where chromosome X's small-region noise gave a wrong call that chromosome Y's cleaner signal caught. Regenerated the committed example PDF and wiki. No public API changes. See `NEWS.md` for full detail.

- **2026-08-09 -- Added real chromosome X data to the quickstart dataset, and found/fixed a real correctness gap affecting any real user's whole-genome VCF.** Added a bounded, non-PAR chromosome X region from the same already-approved Zenodo record chr22 uses, giving the sex-check module real data. Found this 1000 Genomes release represents male chromosome X as genuinely haploid `GT` fields, which `SNPRelate::snpgdsVCF2GDS()` mis-parses (silently padding a missing second allele with the ALT index); fixed at dataset-derivation time with `bcftools +fixploidy`. Concatenating chromosome X into the shared marker pool then revealed a much bigger issue: `run_pipeline()` had no autosome-only filtering anywhere, so pooling hemizygous X markers into kinship/PCA/DAPC/ADMIXTURE/ROH/FST/genome-scan/diversity/AMOVA silently corrupted results for any real whole-genome VCF -- proven concretely (a known duplicate pair's kinship collapsed from 0.4459 to 0.044 purely from adding chromosome X markers). Fixed with new `qc.autosome_only` (default `TRUE`) and `qc.non_autosomal_chromosome_names` config keys, restricting every ploidy-sensitive module to autosomal markers by default while `sex_check` keeps its own unrestricted access. A related, independent finding: `HG03873`/`HG03998`, documented all cycle as a "known duplicate/MZ-twin pair," is not one -- their real chromosome X genotypes prove they are different biological sexes, which MZ twins cannot be, despite a real high chr22-only kinship value. Corrected throughout the wiki, both vignettes, the quickstart README, and test comments; `NA19331`/`NA19334` remains a fully confirmed real duplicate. Regenerated the committed example PDF (132 pages) and republished the wiki. No public API changes. See `NEWS.md` for full detail.

- **2026-08-09 -- Added genetic sex-check QC (X-chromosome heterozygosity vs reported sex).** Infers each sample's sex from X-chromosome heterozygosity (`SNPRelate::snpgdsIndInb(method="mom.visscher")`, Visscher et al. 2010, already a dependency and already used elsewhere for population inbreeding -- no new dependency) and flags mismatches against a supplied `sex` metadata column, the standard upstream check for sample swaps or metadata labeling errors. Default classification thresholds (male F > 0.8, female F < 0.2) are PLINK's own `--check-sex` defaults, verified against a hand-built synthetic GDS with known male/female X genotypes before shipping (clean separation: males F in [0.84,1.13], females F in [-0.06,0.10]). Deliberately does not exclude pseudoautosomal regions (build-specific boundaries, no genome-build-identification mechanism for arbitrary VCFs -- the same reasoning already applied to ROH's genetic-map scope decision). New `sex_check_module_spec()`, on by default (`analyses.sex_check`), VCF-only; skips transparently (no error) when fewer than 20 QC-passing X-chromosome SNPs are found, exactly like Mantel/IBD skipping without coordinates -- which is exactly what happens for the chr22-only quickstart dataset, documented honestly rather than faking a demonstration figure. Also fixed a real pre-existing gap: `inst/example_config.yml` was stale, missing the earlier per-metadata-column PCA feature's config keys. No public API changes beyond the new module-spec export. See `NEWS.md` for full detail.

- **2026-08-09 -- Propagated the new `sex` metadata and per-metadata-column PCA feature to the wiki, vignette, and committed example PDF.** Regenerated the reference pipeline run against the updated quickstart dataset with the same one-off ancestry-enabled config `docs/examples/README.md` documents; confirmed every figure unaffected by the new column is byte-identical to before, and that the VCF hash is unchanged. Added the real `07b_PCA_PC1_PC2_by_sex` figure with empirically-verified interpretive prose (no significant sex separation, as expected for an autosomal PCA) to the wiki and vignette's PCA sections, and the real cross-population-duplicate sex-mismatch finding to their kinship sections. Committed PDF is now 130 pages. No public API changes. See `NEWS.md` for full detail.

- **2026-08-09 -- Added real, self-reported sex to the bundled quickstart dataset's metadata.** Sourced from the same authoritative panel file already used for population assignment (its `gender` column, MD5-verified against the declared canonical checksum before use), carried through by `scripts/derive-quickstart-dataset.R` as a new `sex` column; the VCF and its index are unchanged, verified byte-for-byte against the already-shipped file. 77 male / 83 female across the 160 samples. This gives the new per-metadata-column PCA feature below real data to demonstrate: `sex` now qualifies and `07b_PCA_PC1_PC2_by_sex` actually renders for this dataset. Also surfaced a real, interesting finding: the cross-population "duplicate" pair (`HG03873`/`HG03998`, ITU/STU) records different sex for its two entries, while the same-population duplicate pair (`NA19331`/`NA19334`, LWK) records matching sex -- consistent with the cross-population pair being a same-individual cataloguing artifact rather than genuine identical twins. No public API changes. See `NEWS.md` for full detail.

- **2026-08-09 -- Added static PCA panels coloured by arbitrary sample metadata columns, not just `population`.** `plot_pca()` gains one extra PC1/PC2 panel per *qualifying* metadata column (`07b_PCA_PC1_PC2_by_<column>`), reusing the already-fitted PCA -- no new model fitting. A column qualifies when it has at least 2 distinct values with at least `analyses.pca_metadata_color_min_group` samples each (default 3) and no more than `analyses.pca_metadata_color_max_levels` distinct values (default 12, keeping the legend readable); `population`, coordinates, and identifier-like columns are excluded automatically. New `analyses.pca_metadata_color`/`pca_metadata_color_min_group`/`pca_metadata_color_max_levels` config keys. The bundled quickstart dataset has no additional qualifying metadata column yet, so the committed example report is unaffected for now. No public API changes. See `NEWS.md` for full detail.

## Beyond 1.0

Potential post-1.0 work includes selection scans, genomic landscapes, spatial resistance models, GWAS interoperability, community plugins, interactive exploration, optional Docker Hub publication, and cloud/workflow-platform execution.
