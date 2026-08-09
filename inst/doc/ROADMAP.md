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

## Beyond 1.0

Potential post-1.0 work includes selection scans, genomic landscapes, spatial resistance models, GWAS interoperability, community plugins, interactive exploration, optional Docker Hub publication, and cloud/workflow-platform execution.
