#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 5L) {
  stop(
    "Usage: build_release_candidate_rehearsal.R <policy.json> <output-index.json> ",
    "<candidate-id> <git-commit> <evaluated-at>",
    call. = FALSE
  )
}

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (!length(script_arg)) stop("Unable to resolve script location", call. = FALSE)
script_path <- normalizePath(sub("^--file=", "", script_arg[[1L]]), mustWork = TRUE)
implementation <- normalizePath(
  file.path(dirname(script_path), "..", "inst", "scripts", "build_release_candidate_dossier.R"),
  mustWork = TRUE
)
sys.source(implementation, envir = environment())

policy <- read_release_candidate_policy(args[[1L]])
git_commit <- tolower(release_candidate_scalar(args[[4L]], "git commit"))
if (!grepl("^[0-9a-f]{40}$", git_commit)) {
  stop("git commit must be a lowercase 40-character SHA", call. = FALSE)
}
evaluated_at <- release_candidate_iso_datetime(args[[5L]], "evaluated at")

records <- lapply(seq_len(nrow(policy$gate_table)), function(i) {
  gate <- policy$gate_table[i, , drop = FALSE]
  approval <- if (isTRUE(gate$approval_required)) {
    list(
      state = "pending",
      notes = paste0(
        "Production approval is not supplied by rehearsal mode; complete ",
        gate$issue,
        " and retain a named approval record."
      )
    )
  } else {
    NULL
  }
  list(
    gate_id = gate$gate_id,
    status = "blocked",
    summary = paste0(
      "Rehearsal mode does not supply production evidence for ",
      gate$gate_id,
      "; see ",
      gate$issue,
      "."
    ),
    artifacts = list(),
    approval = approval
  )
})

index <- list(
  schema_version = "1.0",
  mode = "rehearsal",
  candidate_id = release_candidate_scalar(args[[3L]], "candidate id"),
  target_release = release_candidate_scalar(policy$target_release, "target release"),
  package_version = release_candidate_scalar(policy$package_version, "package version"),
  git_commit = git_commit,
  evaluated_at = evaluated_at,
  records = records
)

dir.create(dirname(args[[2L]]), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  index,
  args[[2L]],
  auto_unbox = TRUE,
  pretty = TRUE,
  null = "null",
  na = "null"
)
cat("Blocked rehearsal evidence index written to ", normalizePath(args[[2L]]), "\n", sep = "")
