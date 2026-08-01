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
    status = "blocked",
    summary = paste(
      "benchmark_identity and repetition_count approved; environment_comparability approved as",
      "well-formed only. budget_checks and trend_interpretation recorded as insufficient-evidence:",
      "this repository has zero published GitHub Releases, so",
      "release-benchmark-archive.yml's baseline-discovery step has never had anything to compare",
      "against. Not a defect in the evidence itself -- blocked on the first real GitHub Release."
    ),
    artifacts = list(artifact_entry("continuous_benchmarks.json")),
    approval = list(
      state = "pending",
      notes = "See docs/CONTINUOUS_RELEASE_BENCHMARKING.md; cannot approve budget/trend evidence that does not yet exist."
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
