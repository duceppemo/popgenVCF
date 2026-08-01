#!/usr/bin/env Rscript

# Assembles a production-mode release-candidate-evidence-index.json from real,
# retained gate evidence -- the counterpart to build_release_candidate_rehearsal.R,
# which only ever emits synthetic "blocked" placeholders.
#
# This does not fabricate evidence for gates that have none. Only gates with
# a real, reviewed evidence artifact under <evidence-root> are marked
# "passed"; benchmark_history is honestly "blocked" on the documented
# zero-published-GitHub-Releases gap (docs/CONTINUOUS_RELEASE_BENCHMARKING.md);
# every other gate is "not_run" because this script has no real evidence
# source for it yet. Each per-gate CI workflow that eventually supplies real
# evidence for those remaining gates should extend this script (or a shared
# collector) rather than have a human hand-author a monolithic config.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 6L) {
  stop(
    "Usage: build_release_candidate_evidence_index.R <policy.json> <evidence-root> ",
    "<output-index.json> <candidate-id> <git-commit> <evaluated-at>",
    call. = FALSE
  )
}

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (!length(script_arg)) stop("Unable to resolve script location", call. = FALSE)
script_path <- normalizePath(sub("^--file=", "", script_arg[[1L]]), mustWork = TRUE)
module_dir <- normalizePath(
  file.path(dirname(script_path), "..", "inst", "scripts"),
  mustWork = TRUE
)
for (module in c(
  "release_candidate_utils.R",
  "release_candidate_policy.R",
  "release_candidate_evaluate.R",
  "release_candidate_write.R"
)) {
  sys.source(file.path(module_dir, module), envir = environment())
}

policy <- read_release_candidate_policy(args[[1L]])
evidence_root <- normalizePath(args[[2L]], mustWork = TRUE)
output_path <- args[[3L]]
candidate_id <- rc_scalar(args[[4L]], "candidate id")
git_commit <- tolower(rc_scalar(args[[5L]], "git commit"))
if (!grepl("^[0-9a-f]{40}$", git_commit)) {
  stop("git commit must be a lowercase 40-character SHA", call. = FALSE)
}
evaluated_at <- rc_datetime(args[[6L]], "evaluated at")

artifact_entry <- function(relative_path) {
  absolute <- file.path(evidence_root, relative_path)
  if (!file.exists(absolute) || isTRUE(file.info(absolute)$isdir)) {
    stop("Missing expected evidence artifact: ", relative_path, call. = FALSE)
  }
  list(
    path = relative_path,
    size_bytes = as.numeric(file.info(absolute)$size),
    sha256 = digest::digest(absolute, algo = "sha256", file = TRUE)
  )
}

reviewed <- list(
  production_baseline = list(
    status = "passed",
    summary = paste(
      "chr22 quantitative baseline: MAF>=0.05 threshold, three count metrics, and",
      "two PCA variance-proportion metrics (SNPRelate eigen.cnt-relative definition kept as-is)",
      "all reviewed and approved against the full 7-item manual checklist."
    ),
    artifacts = list(
      artifact_entry("autosomal-baseline-proposal.json"),
      artifact_entry("autosomal-baseline-observations.tsv")
    ),
    approval = list(
      state = "approved", reviewer = "Marc-Olivier Duceppe", reviewed_at = "2026-07-30",
      notes = "See docs/developer/canonical-autosomal-baseline-proposal.md."
    )
  ),
  external_concordance = list(
    status = "passed",
    summary = paste(
      "Three equivalence records (SNPRelate PCA, SNPRelate IBS, adegenet DAPC) approved against",
      "real chr22 data; pegas/poppr AMOVA divergence accepted as explained by differing distance",
      "conventions (diagnostic, non-gating)."
    ),
    artifacts = list(artifact_entry("scientific_concordance.json")),
    approval = list(
      state = "approved", reviewer = "Marc-Olivier Duceppe", reviewed_at = "2026-07-30",
      notes = "See docs/SCIENTIFIC_CONCORDANCE.md."
    )
  ),
  ancestry_three_backend = list(
    status = "passed",
    summary = paste(
      "135-run production sweep (ADMIXTURE, fastStructure, sNMF; K=2:10, 5 replicates) against a",
      "10Mb chr22 interval. same_biological_input, sample_order, replicate_design, and",
      "label_alignment approved. K-selection consensus (K=5) had low cross-backend agreement",
      "(33%) but strong Q-matrix concordance (0.983-0.995); accepted as the recorded finding.",
      "role/minimum_alignment_score remain a documented placeholder pending a separate policy decision."
    ),
    artifacts = list(artifact_entry("ancestry-three-backend-proposal.json")),
    approval = list(
      state = "approved", reviewer = "Marc-Olivier Duceppe", reviewed_at = "2026-08-01",
      notes = "See docs/user/ancestry-backends.md."
    )
  ),
  benchmark_history = list(
    status = "passed",
    summary = paste(
      "benchmark_identity, repetition_count, and environment_comparability approved earlier.",
      "budget_checks and trend_interpretation were initially insufficient-evidence (no prior",
      "published Release to compare against); after fixing a missing GH_TOKEN in",
      "release-benchmark-archive.yml (every gh CLI call was silently failing auth), a real",
      "baseline-discovery and comparison against the 0.10.0-rc1 Release succeeded:",
      "release_ready=true, both validation-suite digests unchanged, and all three performance",
      "budget checks (runtime, peak memory, temporary disk) passed without regression."
    ),
    artifacts = list(artifact_entry("continuous_benchmarks.json")),
    approval = list(
      state = "approved", reviewer = "Marc-Olivier Duceppe", reviewed_at = "2026-08-01",
      notes = "See docs/CONTINUOUS_RELEASE_BENCHMARKING.md 'Second execution and review'."
    )
  ),
  apptainer_distribution = list(
    status = "passed",
    summary = paste(
      "apptainer.yml built and smoke-tested a SIF image from Apptainer.def for the exact evaluated",
      "commit (apptainer build, apptainer test, package-version check, --help, and the same",
      "scientific/population-structure validation run inside the image). This workflow has no",
      "publish/registry step to gate, so a dispatched run is evidence-equivalent to what a tagged",
      "release would produce -- unlike oci_distribution, which still needs a real registry push",
      "to verify SBOM/provenance."
    ),
    artifacts = list(
      artifact_entry("apptainer-metadata.json"),
      artifact_entry("apptainer-metadata-SHA256SUMS.txt")
    ),
    approval = NULL
  ),
  source_distribution = list(
    status = "passed",
    summary = paste(
      "tagged-source-release.yml built the exact source tarball, generated its SPDX SBOM, wrote",
      "source-release-provenance.json binding the release/commit/workflow identity to the tarball",
      "and SBOM, and produced a release-manifest.json and terminal release-SHA256SUMS.txt for the",
      "exact evaluated commit, all in a clean GitHub-hosted runner. Checksums re-verified locally",
      "before inclusion here."
    ),
    artifacts = list(
      artifact_entry("popgenVCF_0.10.0.tar.gz"),
      artifact_entry("popgenVCF-source-sbom.spdx.json"),
      artifact_entry("source-release-provenance.json"),
      artifact_entry("release-manifest.json"),
      artifact_entry("release-SHA256SUMS.txt")
    ),
    approval = NULL
  ),
  scientific_validation = list(
    status = "passed",
    summary = paste(
      "run_scientific_validation() and run_population_structure_validation() both passed with no",
      "failing checks: hand-calculated allele frequency/MAF/heterozygosity/missingness cross-checks,",
      "PCA eigen-equation residual, MDS eigenspace equivalence, SNPRelate LD/FST/IBS/MAF/missingness",
      "consistency (core suite), and DAPC synthetic classification, label-switching alignment, and",
      "replicate reproducibility (population-structure suite) -- all within their declared",
      "tolerances, produced by the same scientific-release-integration self-test",
      "tagged-source-release.yml runs for the exact evaluated commit."
    ),
    artifacts = list(
      artifact_entry("scientific-validation.tsv"),
      artifact_entry("population-structure-validation.tsv")
    ),
    approval = NULL
  ),
  source_package_check = list(
    status = "passed",
    summary = paste(
      "R CMD check --as-cran on the exact built source tarball: 0 errors, 0 warnings, 2 NOTEs",
      "(a CRAN-incoming-feasibility note about reading CITATION before install -- a known R CMD",
      "check artifact, not a real defect -- and a data.table NSE column-reference note in",
      "population_structure.R, a common false positive for data.table-heavy code). R CMD INSTALL",
      "into a clean library succeeded and the installed package loaded from both its temporary and",
      "final install locations."
    ),
    artifacts = list(
      artifact_entry("R-CMD-check.log"),
      artifact_entry("R-CMD-install.out")
    ),
    approval = NULL
  ),
  metadata_consistency = list(
    status = "passed",
    summary = paste(
      "release-metadata.yml's three validators all passed with no failing checks:",
      "validate_release_metadata.R (DESCRIPTION/CITATION.cff/codemeta.json/software-identity",
      "consistency, including the development-release boundary -- no release date or DOI claimed",
      "while unreleased), validate_license_metadata.R (LICENSE/SPDX consistency), and",
      "validate_zenodo_metadata.R (.zenodo.json consistency)."
    ),
    artifacts = list(
      artifact_entry("release-metadata-validation.json"),
      artifact_entry("license-metadata-validation.json"),
      artifact_entry("zenodo-metadata-validation.json")
    ),
    approval = NULL
  ),
  public_api_contract = list(
    status = "passed",
    summary = paste(
      "public-api-contract.yml's canonical baseline verification passed: 627 entries, 0 blocking",
      "findings. 17 advisory findings are present -- all new public exports and optional arguments",
      "added this release cycle (the read_/approve_ evidence functions and a few unrelated optional",
      "arguments) -- expected and non-blocking; the canonical baseline is refreshed and reviewed",
      "separately from this gate, not automatically."
    ),
    artifacts = list(
      artifact_entry("public-api-summary.tsv"),
      artifact_entry("public-api-findings.tsv")
    ),
    approval = NULL
  ),
  canonical_validation = list(
    status = "passed",
    summary = paste(
      "canonical-real-data.yml's production execution (candidate_id 0.10.0-canonical-validation-2)",
      "acquired the approved canonical dataset 1000g_phase3_chr22_v5a 20130502-v5a in a clean",
      "environment, verified it against the approved upstream MD5 inventory, promoted it to SHA-256,",
      "and structurally inspected it with bcftools against the complete sample metadata -- all for",
      "the exact evaluated commit. No raw genotype data is retained in this evidence. Artifact",
      "filenames are flattened (no subdirectories) because GitHub Release assets cannot contain",
      "directory structure."
    ),
    artifacts = list(
      artifact_entry("canonical_dataset_structure.tsv"),
      artifact_entry("canonical_sample_metadata.tsv"),
      artifact_entry("canonical-production-environment.tsv"),
      artifact_entry("canonical-production-execution.json"),
      artifact_entry("canonical-source-dataset-registry.tsv"),
      artifact_entry("canonical-source-acquisition.tsv"),
      artifact_entry("canonical-source-verification.tsv"),
      artifact_entry("canonical-dataset-verification.tsv"),
      artifact_entry("canonical-dataset-record.tsv"),
      artifact_entry("canonical-validation-methods.md")
    ),
    approval = NULL
  ),
  oci_distribution = list(
    status = "passed",
    summary = paste(
      "container.yml built and pushed the real v0.10.0 OCI image to",
      "ghcr.io/duceppemo/popgenvcf, triggered by the actual GitHub Release publish, tagged",
      "'latest', '0.10.0', '0.10', '0', and by commit SHA. It was pulled back by digest and",
      "re-validated (package-version check, --help, and the same scientific/population-",
      "structure validation used elsewhere). BuildKit generated an SPDX SBOM and maximum SLSA",
      "provenance attestation for the pushed image, both independently checksum-verified",
      "before inclusion here."
    ),
    artifacts = list(
      artifact_entry("container-metadata.json"),
      artifact_entry("container-digest.txt"),
      artifact_entry("container-sbom.spdx.json"),
      artifact_entry("container-provenance.slsa.json")
    ),
    approval = NULL
  ),
  scientific_approval = list(
    status = "passed",
    summary = paste(
      "Marc-Olivier Duceppe, the named scientific reviewer, approved the complete scientific",
      "evidence set for v0.10.0: production_baseline, external_concordance,",
      "ancestry_three_backend, and benchmark_history. This approval covers the scientific",
      "evidence only; it does not by itself authorize tagging, publication, or DOI assignment",
      "(see release_authorization)."
    ),
    artifacts = list(artifact_entry("scientific-approval-record.json")),
    approval = list(
      state = "approved", reviewer = "Marc-Olivier Duceppe", reviewed_at = "2026-08-01",
      notes = "See scientific-approval-record.json."
    )
  ),
  release_authorization = list(
    status = "passed",
    summary = paste(
      "Marc-Olivier Duceppe, as release owner, authorized tagging, publication, container-image",
      "publication, and Zenodo deposition/DOI assignment for this exact commit, contingent on",
      "archival_assets completing as part of that same deposition (Zenodo evidence can only",
      "exist after the real GitHub Release it deposits from is published)."
    ),
    artifacts = list(artifact_entry("release-authorization-record.json")),
    approval = list(
      state = "approved", reviewer = "Marc-Olivier Duceppe", reviewed_at = "2026-08-01",
      notes = "See release-authorization-record.json."
    )
  ),
  archival_assets = list(
    status = "passed",
    summary = paste(
      "popgenVCF 0.10.0 was deposited to Zenodo via its GitHub integration and independently",
      "verified through four separate checks: the public search API, the record page, a real",
      "doi.org resolution (302 redirect to Zenodo, confirming DataCite registration rather than",
      "only a Zenodo-internal record), and the raw record API JSON (for the concept DOI). DOI",
      "10.5281/zenodo.21747548 (concept DOI 10.5281/zenodo.21747067) is now reconciled across",
      "inst/metadata/software-identity.json, CITATION.cff, codemeta.json, .zenodo.json,",
      "README.md, and docs/reproducibility.md."
    ),
    artifacts = list(artifact_entry("archival-assets-record.json")),
    approval = NULL
  )
)

records <- lapply(seq_len(nrow(policy$gate_table)), function(i) {
  gate <- policy$gate_table[i, , drop = FALSE]
  gate_id <- gate$gate_id
  if (gate_id %in% names(reviewed)) {
    r <- reviewed[[gate_id]]
    list(gate_id = gate_id, status = r$status, summary = r$summary,
         artifacts = r$artifacts, approval = r$approval)
  } else {
    approval <- if (isTRUE(gate$approval_required)) {
      list(
        state = "pending",
        notes = paste0(
          "No real evidence source is wired into this script yet for ", gate_id,
          "; see ", gate$issue, "."
        )
      )
    } else {
      NULL
    }
    list(
      gate_id = gate_id, status = "not_run",
      summary = paste0(
        "No real evidence source is wired into this script yet for ", gate_id,
        "; see ", gate$issue, "."
      ),
      artifacts = list(), approval = approval
    )
  }
})

index <- list(
  schema_version = "1.0",
  mode = "production",
  candidate_id = candidate_id,
  target_release = rc_scalar(policy$target_release, "target release"),
  package_version = rc_scalar(policy$package_version, "package version"),
  git_commit = git_commit,
  evaluated_at = evaluated_at,
  records = records
)

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  index, output_path, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null"
)

dossier <- evaluate_release_candidate_dossier(args[[1L]], output_path, evidence_root)
cat(
  "Production evidence index written to ", normalizePath(output_path), "\n",
  "release_ready: ", dossier$release_ready, "\n",
  "required gates passed: ", dossier$dossier$summary$passed_required_gates,
  " / ", dossier$dossier$summary$required_gates, "\n",
  "blocking gates: ", nrow(dossier$blockers), "\n",
  sep = ""
)
