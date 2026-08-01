#!/usr/bin/env Rscript

# Assembles a production-mode release-candidate-evidence-index.json from
# real, retained gate evidence -- the counterpart to
# build_release_candidate_rehearsal.R, which only ever emits synthetic
# "blocked" placeholders.
#
# Unlike earlier versions of this script, gate evidence is not hand-authored
# in R source here. Instead, every gate -- whether produced automatically by
# a CI workflow (metadata_consistency, public_api_contract,
# source_package_check, scientific_validation, canonical_validation,
# source_distribution, oci_distribution, apptainer_distribution) or supplied
# by a human after real scientific review or sign-off (production_baseline,
# external_concordance, ancestry_three_backend, benchmark_history,
# scientific_approval, release_authorization, archival_assets) -- is a small,
# self-describing "<gate_id>-gate-record.json" fragment:
#
#   {
#     "gate_id": "<one of the release-candidate-policy.json gate ids>",
#     "status": "passed" | "failed" | "blocked" | "not_run",
#     "summary": "<one paragraph, what was checked and why the status holds>",
#     "artifacts": [{"path": "<relative to this fragment's own directory>",
#                     "size_bytes": <int>, "sha256": "<hex>"}, ...],
#     "approval": null | {"state": "approved"|"rejected"|"pending",
#                          "reviewer": "<name>", "reviewed_at": "YYYY-MM-DD",
#                          "notes": "<optional>"}
#   }
#
# This script recursively finds every such fragment under
# <evidence-sources-dir>, flattens and copies each fragment's referenced
# artifacts into <output-evidence-dir> (GitHub Release assets cannot contain
# directory structure, so nested artifact paths are rewritten to flat
# filenames), verifies each artifact's recorded checksum against the real
# file, and merges the fragments into one evidence index. Gates with no
# fragment are honestly "not_run" -- this script never fabricates evidence.
#
# See docs/developer/release-candidate-closure.md for the fragment contract
# and .github/workflows/release-candidate-collector.yml for how the
# CI-producible fragments are generated and gathered automatically.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 7L) {
  stop(
    "Usage: build_release_candidate_evidence_index.R <policy.json> ",
    "<evidence-sources-dir> <output-index.json> <output-evidence-dir> ",
    "<candidate-id> <git-commit> <evaluated-at>",
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
sources_dir <- normalizePath(args[[2L]], mustWork = TRUE)
output_index_path <- args[[3L]]
output_evidence_dir <- args[[4L]]
candidate_id <- rc_scalar(args[[5L]], "candidate id")
git_commit <- tolower(rc_scalar(args[[6L]], "git commit"))
if (!grepl("^[0-9a-f]{40}$", git_commit)) {
  stop("git commit must be a lowercase 40-character SHA", call. = FALSE)
}
evaluated_at <- rc_datetime(args[[7L]], "evaluated at")

dir.create(output_evidence_dir, recursive = TRUE, showWarnings = FALSE)
output_evidence_dir <- normalizePath(output_evidence_dir, mustWork = TRUE)

fragment_paths <- sort(list.files(
  sources_dir, pattern = "-gate-record\\.json$",
  recursive = TRUE, full.names = TRUE
))
if (!length(fragment_paths)) {
  stop("No *-gate-record.json fragments found under ", sources_dir, call. = FALSE)
}

known_gate_ids <- policy$gate_table$gate_id
allowed_statuses <- as.character(unlist(policy$allowed_statuses))
flattened_names <- character()

flatten_artifact <- function(fragment_dir, relative_path, recorded_sha256, gate_id) {
  source_path <- normalizePath(file.path(fragment_dir, relative_path), mustWork = TRUE)
  actual_sha256 <- tolower(digest::digest(source_path, algo = "sha256", file = TRUE))
  if (!identical(actual_sha256, tolower(recorded_sha256))) {
    stop(
      "Checksum mismatch for ", gate_id, " artifact ", relative_path,
      ": fragment recorded ", recorded_sha256, ", file is actually ", actual_sha256,
      call. = FALSE
    )
  }
  flat_name <- paste0(gsub("_", "-", gate_id, fixed = TRUE), "--", gsub("/", "-", relative_path, fixed = TRUE))
  destination <- file.path(output_evidence_dir, flat_name)
  if (flat_name %in% names(flattened_names)) {
    if (!identical(flattened_names[[flat_name]], actual_sha256)) {
      stop(
        "Flattened artifact name collision with different content: ", flat_name,
        call. = FALSE
      )
    }
  } else {
    file.copy(source_path, destination, overwrite = TRUE)
    flattened_names[[flat_name]] <<- actual_sha256
  }
  list(
    path = flat_name,
    size_bytes = as.numeric(file.info(destination)$size),
    sha256 = actual_sha256
  )
}

records_by_gate <- list()
for (fragment_path in fragment_paths) {
  fragment <- jsonlite::read_json(fragment_path, simplifyVector = FALSE)
  required <- c("gate_id", "status", "summary", "artifacts", "approval")
  if (!is.list(fragment) || !all(required %in% names(fragment))) {
    stop("Malformed gate-record fragment: ", fragment_path, call. = FALSE)
  }
  gate_id <- rc_scalar(fragment$gate_id, "fragment gate_id")
  if (!gate_id %in% known_gate_ids) {
    stop("Fragment references an unknown gate_id '", gate_id, "': ", fragment_path, call. = FALSE)
  }
  if (gate_id %in% names(records_by_gate)) {
    stop(
      "Duplicate gate-record fragments for gate '", gate_id, "': ",
      records_by_gate[[gate_id]]$source, " and ", fragment_path,
      call. = FALSE
    )
  }
  status <- rc_scalar(fragment$status, paste0(gate_id, " status"))
  if (!status %in% allowed_statuses) {
    stop("Invalid status '", status, "' in fragment: ", fragment_path, call. = FALSE)
  }
  summary <- rc_scalar(fragment$summary, paste0(gate_id, " summary"))
  fragment_dir <- dirname(fragment_path)
  artifact_list <- fragment$artifacts
  artifacts <- if (is.null(artifact_list) || !length(artifact_list)) {
    list()
  } else {
    lapply(artifact_list, function(a) {
      flatten_artifact(fragment_dir, rc_scalar(a$path, "artifact path"), a$sha256, gate_id)
    })
  }
  records_by_gate[[gate_id]] <- list(
    gate_id = gate_id, status = status, summary = summary,
    artifacts = artifacts, approval = fragment$approval, source = fragment_path
  )
}

records <- lapply(seq_len(nrow(policy$gate_table)), function(i) {
  gate <- policy$gate_table[i, , drop = FALSE]
  gate_id <- gate$gate_id
  if (gate_id %in% names(records_by_gate)) {
    r <- records_by_gate[[gate_id]]
    list(gate_id = gate_id, status = r$status, summary = r$summary,
         artifacts = r$artifacts, approval = r$approval)
  } else {
    approval <- if (isTRUE(gate$approval_required)) {
      list(
        state = "pending",
        notes = paste0(
          "No gate-record fragment was found for ", gate_id, "; see ", gate$issue, "."
        )
      )
    } else {
      NULL
    }
    list(
      gate_id = gate_id, status = "not_run",
      summary = paste0(
        "No gate-record fragment was found for ", gate_id, "; see ", gate$issue, "."
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

dir.create(dirname(output_index_path), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  index, output_index_path, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null"
)

dossier <- evaluate_release_candidate_dossier(args[[1L]], output_index_path, output_evidence_dir)
cat(
  "Production evidence index written to ", normalizePath(output_index_path), "\n",
  "Fragments merged: ", length(records_by_gate), " (",
  paste(sort(names(records_by_gate)), collapse = ", "), ")\n",
  "release_ready: ", dossier$release_ready, "\n",
  "required gates passed: ", dossier$dossier$summary$passed_required_gates,
  " / ", dossier$dossier$summary$required_gates, "\n",
  "blocking gates: ", nrow(dossier$blockers), "\n",
  sep = ""
)
