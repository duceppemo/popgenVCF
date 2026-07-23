rc_dossier_paths <- function(output_dir) {
  names <- c(
    gates = "release-candidate-gates.tsv",
    blockers = "release-candidate-blockers.tsv",
    artifacts = "release-candidate-artifacts.tsv",
    dossier = "release-candidate-dossier.json",
    report = "release-candidate-readiness.md",
    checksums = "release-candidate-SHA256SUMS.txt"
  )
  stats::setNames(file.path(output_dir, unname(names)), names(names))
}

write_release_candidate_dossier <- function(result, output_dir) {
  rc_need()
  if (!inherits(result, "PopgenVCFReleaseCandidateDossier")) {
    stop("result must be a PopgenVCFReleaseCandidateDossier", call. = FALSE)
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  paths <- rc_dossier_paths(output_dir)

  data.table::fwrite(
    result$gates,
    paths[["gates"]],
    sep = "\t",
    quote = FALSE,
    na = "NA"
  )
  data.table::fwrite(
    result$blockers,
    paths[["blockers"]],
    sep = "\t",
    quote = FALSE,
    na = "NA"
  )
  data.table::fwrite(
    result$artifacts,
    paths[["artifacts"]],
    sep = "\t",
    quote = FALSE,
    na = "NA"
  )
  jsonlite::write_json(
    result$dossier,
    paths[["dossier"]],
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null",
    na = "null",
    digits = NA
  )

  status <- if (result$release_ready) "READY" else "BLOCKED"
  lines <- c(
    paste0("# popgenVCF release candidate: ", status),
    "",
    paste0("- Candidate: `", result$dossier$candidate_id, "`"),
    paste0("- Target release: `", result$dossier$target_release, "`"),
    paste0("- Package version: `", result$dossier$package_version, "`"),
    paste0("- Git commit: `", result$dossier$git_commit, "`"),
    paste0("- Evaluation mode: `", result$dossier$mode, "`"),
    paste0("- Evaluated: `", result$dossier$evaluated_at, "`"),
    "",
    paste0(
      "Required gates passed: ",
      result$dossier$summary$passed_required_gates,
      "/",
      result$dossier$summary$required_gates,
      "."
    ),
    paste0(
      "Retained evidence artifacts: ",
      result$dossier$summary$retained_artifacts,
      "."
    )
  )

  if (nrow(result$blockers)) {
    lines <- c(
      lines,
      "",
      "## Blocking gates",
      "",
      paste0(
        "- `",
        result$blockers$gate_id,
        "` (",
        result$blockers$issue,
        "): ",
        result$blockers$blocking_reason
      )
    )
  } else {
    lines <- c(
      lines,
      "",
      "## Release authorization",
      "",
      paste(
        "Every required gate is passed and every approval-required gate",
        "has a named approval record."
      ),
      paste(
        "This dossier authorizes the documented publication sequence for",
        "this exact commit."
      )
    )
  }

  lines <- c(
    lines,
    "",
    "## Evidence boundary",
    "",
    paste(
      "This dossier reports supplied evidence. It does not infer scientific",
      "approval from execution, and a rehearsal cannot become release-ready."
    )
  )
  writeLines(lines, paths[["report"]], useBytes = TRUE)

  payload <- paths[c("gates", "blockers", "artifacts", "dossier", "report")]
  checksum_lines <- vapply(
    unname(payload),
    function(path) {
      paste0(
        digest::digest(path, algo = "sha256", file = TRUE),
        "  ",
        basename(path)
      )
    },
    character(1L)
  )
  writeLines(checksum_lines, paths[["checksums"]], useBytes = TRUE)

  verify_release_candidate_dossier(output_dir)
  stats::setNames(
    vapply(
      unname(paths),
      normalizePath,
      character(1L),
      winslash = "/",
      mustWork = TRUE
    ),
    names(paths)
  )
}

verify_release_candidate_dossier <- function(output_dir) {
  rc_need()
  paths <- rc_dossier_paths(output_dir)
  missing <- names(paths)[!file.exists(paths)]
  if (length(missing)) {
    stop(
      "Release-candidate dossier is missing: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  lines <- readLines(paths[["checksums"]], warn = FALSE)
  entries <- strsplit(lines, "  ", fixed = TRUE)
  expected <- basename(paths[c("gates", "blockers", "artifacts", "dossier", "report")])
  observed <- vapply(entries, function(entry) {
    if (length(entry) != 2L) return(NA_character_)
    entry[[2L]]
  }, character(1L))
  if (length(entries) != length(expected) || anyNA(observed) ||
      !identical(observed, unname(expected))) {
    stop("Invalid release-candidate checksum inventory", call. = FALSE)
  }

  for (entry in entries) {
    path <- file.path(output_dir, entry[[2L]])
    expected_sha <- rc_sha(entry[[1L]], paste0(entry[[2L]], " checksum"))
    actual_sha <- digest::digest(path, algo = "sha256", file = TRUE)
    if (!identical(actual_sha, expected_sha)) {
      stop(
        "Release-candidate dossier checksum mismatch: ",
        entry[[2L]],
        call. = FALSE
      )
    }
  }

  dossier <- jsonlite::read_json(
    paths[["dossier"]],
    simplifyVector = FALSE
  )
  if (!identical(dossier$schema_version, "1.0") ||
      !identical(
        dossier$record_type,
        "popgenvcf_release_candidate_dossier"
      )) {
    stop("Unsupported release-candidate dossier schema", call. = FALSE)
  }
  if (!is.logical(dossier$release_ready) ||
      length(dossier$release_ready) != 1L ||
      is.na(dossier$release_ready)) {
    stop("Invalid release-candidate readiness state", call. = FALSE)
  }

  invisible(TRUE)
}
