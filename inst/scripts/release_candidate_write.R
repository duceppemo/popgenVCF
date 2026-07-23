write_release_candidate_dossier <- function(result, output_dir) {
  rc_need()
  if (!inherits(result, "PopgenVCFReleaseCandidateDossier"))
    stop("result must be a PopgenVCFReleaseCandidateDossier", call. = FALSE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  p <- c(gates = "release-candidate-gates.tsv", blockers = "release-candidate-blockers.tsv",
    artifacts = "release-candidate-artifacts.tsv", dossier = "release-candidate-dossier.json",
    report = "release-candidate-readiness.md", checksums = "release-candidate-SHA256SUMS.txt")
  p <- file.path(output_dir, p)
  data.table::fwrite(result$gates, p[["gates"]], sep = "\t", quote = FALSE, na = "NA")
  data.table::fwrite(result$blockers, p[["blockers"]], sep = "\t", quote = FALSE, na = "NA")
  data.table::fwrite(result$artifacts, p[["artifacts"]], sep = "\t", quote = FALSE, na = "NA")
  jsonlite::write_json(result$dossier, p[["dossier"]], auto_unbox = TRUE, pretty = TRUE,
    null = "null", na = "null", digits = NA)
  status <- if (result$release_ready) "READY" else "BLOCKED"
  lines <- c(paste0("# popgenVCF release candidate: ", status), "",
    paste0("- Candidate: `", result$dossier$candidate_id, "`"),
    paste0("- Target release: `", result$dossier$target_release, "`"),
    paste0("- Package version: `", result$dossier$package_version, "`"),
    paste0("- Git commit: `", result$dossier$git_commit, "`"),
    paste0("- Evaluation mode: `", result$dossier$mode, "`"),
    paste0("- Evaluated: `", result$dossier$evaluated_at, "`"), "",
    paste0("Required gates passed: ", result$dossier$summary$passed_required_gates,
      "/", result$dossier$summary$required_gates, "."),
    paste0("Retained evidence artifacts: ", result$dossier$summary$retained_artifacts, "."))
  if (nrow(result$blockers)) {
    lines <- c(lines, "", "## Blocking gates", "",
      paste0("- `", result$blockers$gate_id, "` (", result$blockers$issue, "): ",
        result$blockers$blocking_reason))
  } else {
    lines <- c(lines, "", "## Release authorization", "",
      "Every required gate is passed and every approval-required gate has a named approval record.",
      "This dossier authorizes the documented publication sequence for this exact commit.")
  }
  lines <- c(lines, "", "## Evidence boundary", "",
    "This dossier reports supplied evidence. It does not infer scientific approval from execution and a rehearsal cannot become release-ready.")
  writeLines(lines, p[["report"]], useBytes = TRUE)
  payload <- unname(p[c("gates", "blockers", "artifacts", "dossier", "report")])
  writeLines(vapply(payload, function(x) paste0(digest::digest(x, "sha256", file = TRUE),
    "  ", basename(x)), character(1L)), p[["checksums"]], useBytes = TRUE)
  verify_release_candidate_dossier(output_dir)
  stats::setNames(vapply(p, normalizePath, character(1L), winslash = "/", mustWork = TRUE), names(p))
}

verify_release_candidate_dossier <- function(output_dir) {
  rc_need()
  required <- c("release-candidate-gates.tsv", "release-candidate-blockers.tsv",
    "release-candidate-artifacts.tsv", "release-candidate-dossier.json",
    "release-candidate-readiness.md", "release-candidate-SHA256SUMS.txt")
  missing <- required[!file.exists(file.path(output_dir, required))]
  if (length(missing)) stop("Release-candidate dossier is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  entries <- strsplit(readLines(file.path(output_dir, required[[6L]]), warn = FALSE), "  ", fixed = TRUE)
  if (length(entries) != 5L || any(lengths(entries) != 2L))
    stop("Invalid release-candidate checksum inventory", call. = FALSE)
  for (e in entries) {
    path <- file.path(output_dir, e[[2L]])
    if (!file.exists(path)) stop("Checksummed dossier file is missing: ", e[[2L]], call. = FALSE)
    if (!identical(digest::digest(path, "sha256", file = TRUE), rc_sha(e[[1L]])))
      stop("Release-candidate dossier checksum mismatch: ", e[[2L]], call. = FALSE)
  }
  d <- jsonlite::read_json(file.path(output_dir, "release-candidate-dossier.json"),
    simplifyVector = FALSE)
  if (!identical(d$schema_version, "1.0") ||
      !identical(d$record_type, "popgenvcf_release_candidate_dossier"))
    stop("Unsupported release-candidate dossier schema", call. = FALSE)
  invisible(TRUE)
}
