#!/usr/bin/env Rscript

# Materializes a human-authored scientific-review determination into a
# checksum-bound "<gate_id>-gate-record.json" fragment -- the same format
# every CI-producible gate writes for itself (see
# inst/scripts/release_candidate_gate_record.R and
# docs/developer/release-candidate-closure.md). Intended for the gates that
# genuinely require a named reviewer's judgment rather than an objective
# technical check: production_baseline, external_concordance,
# ancestry_three_backend, benchmark_history, scientific_approval,
# release_authorization, and archival_assets.
#
# A reviewer (or whoever is helping them record a decision) writes a small
# declarative review-spec JSON:
#
#   {
#     "gate_id": "production_baseline",
#     "status": "passed",
#     "summary": "One paragraph: what was reviewed and why the status holds.",
#     "artifacts": ["autosomal-baseline-proposal.json", "autosomal-baseline-observations.tsv"],
#     "approval": {
#       "state": "approved", "reviewer": "Full Name", "reviewed_at": "YYYY-MM-DD",
#       "notes": "Optional."
#     }
#   }
#
# "approval" may be omitted or null for gates that do not require named
# sign-off. "artifacts" are paths relative to <evidence-dir>; this script
# never invents evidence -- every artifact must already exist there, and its
# checksum is computed from the real file, not copied from the spec.
#
# Usage: write_scientific_review_gate_record.R <review-spec.json> <evidence-dir> <output-path>

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) {
  stop(
    "Usage: write_scientific_review_gate_record.R <review-spec.json> <evidence-dir> <output-path>",
    call. = FALSE
  )
}

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (!length(script_arg)) stop("Unable to resolve script location", call. = FALSE)
script_path <- normalizePath(sub("^--file=", "", script_arg[[1L]]), mustWork = TRUE)
source(file.path(dirname(script_path), "..", "inst", "scripts", "release_candidate_gate_record.R"))

spec_path <- args[[1L]]
evidence_dir <- normalizePath(args[[2L]], mustWork = TRUE)
output_path <- args[[3L]]

if (!file.exists(spec_path)) stop("Review-spec file does not exist: ", spec_path, call. = FALSE)
spec <- jsonlite::read_json(spec_path, simplifyVector = FALSE)

required <- c("gate_id", "status", "summary", "artifacts")
missing <- setdiff(required, names(spec))
if (length(missing)) {
  stop("Review-spec is missing required field(s): ", paste(missing, collapse = ", "), call. = FALSE)
}
if (!length(spec$artifacts)) {
  stop("Review-spec must list at least one artifact", call. = FALSE)
}

artifact_paths <- file.path(evidence_dir, vapply(spec$artifacts, function(x) x[[1L]], character(1L)))
missing_files <- artifact_paths[!file.exists(artifact_paths)]
if (length(missing_files)) {
  stop(
    "Review-spec references artifact(s) that do not exist under ", evidence_dir, ": ",
    paste(basename(missing_files), collapse = ", "),
    call. = FALSE
  )
}

written <- write_release_candidate_gate_record(
  gate_id = spec$gate_id[[1L]],
  status = spec$status[[1L]],
  summary = spec$summary[[1L]],
  artifact_paths = artifact_paths,
  root = evidence_dir,
  output_path = output_path,
  approval = spec$approval
)
cat("Gate-record fragment written to ", written, "\n", sep = "")
