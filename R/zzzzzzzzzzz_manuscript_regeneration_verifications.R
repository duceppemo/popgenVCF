regeneration_verification_checks <- function(x) {
  required <- c("section_id", "output_identity", "verified_identity", "status", "verifier_id", "note")
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  if (!all(required %in% names(x))) stop("checks must contain: ", paste(required, collapse = ", "), call. = FALSE)
  x <- x[, required, drop = FALSE]
  for (column in required) x[[column]] <- trimws(as.character(x[[column]]))
  if (!nrow(x)) stop("checks must contain at least one row", call. = FALSE)
  if (anyNA(x[c("section_id", "output_identity", "verified_identity", "status", "verifier_id")]) ||
      any(!nzchar(unlist(x[c("section_id", "output_identity", "verified_identity", "status", "verifier_id")], use.names = FALSE)))) {
    stop("verification check identities, status, and verifier_id must be non-empty", call. = FALSE)
  }
  if (anyDuplicated(x$section_id)) stop("checks must contain unique section_id values", call. = FALSE)
  if (any(!x$status %in% c("verified", "mismatch", "not_verified"))) stop("invalid verification status", call. = FALSE)
  x$note[is.na(x$note)] <- ""
  x <- x[order(x$section_id), , drop = FALSE]
  rownames(x) <- NULL
  x
}

regeneration_verification_payload <- function(x) {
  list(
    schema_version = x$schema_version,
    manuscript_id = x$manuscript_id,
    revision_id = x$revision_id,
    plan_digest = x$plan_digest,
    execution_digest = x$execution_digest,
    verification_id = x$verification_id,
    checks = as.data.frame(x$checks, stringsAsFactors = FALSE)
  )
}

#' Create a deterministic manuscript regeneration verification record
#'
#' @param execution A validated `PopgenVCFRegenerationExecution`.
#' @param checks Data frame describing section-level identity verification.
#' @param verification_id Stable verification identifier.
#' @param plan Optional linked `PopgenVCFRegenerationPlan`.
#' @return A validated `PopgenVCFRegenerationVerification`.
#' @export
new_manuscript_regeneration_verification <- function(execution, checks, verification_id, plan = NULL) {
  validate_manuscript_regeneration_execution(execution, plan = plan)
  checks <- regeneration_verification_checks(checks)
  out <- list(
    schema_version = "1.0",
    manuscript_id = execution$manuscript_id,
    revision_id = execution$revision_id,
    plan_digest = execution$plan_digest,
    execution_digest = execution$digest,
    verification_id = regeneration_id(verification_id, "verification_id"),
    checks = data.table::as.data.table(checks)
  )
  out$digest <- digest::digest(regeneration_verification_payload(out), algo = "sha256", serialize = TRUE)
  out <- structure(out, class = "PopgenVCFRegenerationVerification")
  validate_manuscript_regeneration_verification(out, execution = execution, plan = plan)
  out
}

#' Return the canonical regeneration verification table
#' @param x A regeneration verification record.
#' @return A deterministic data table.
#' @export
manuscript_regeneration_verification_table <- function(x) {
  if (!inherits(x, "PopgenVCFRegenerationVerification")) stop("x must be a PopgenVCFRegenerationVerification", call. = FALSE)
  data.table::as.data.table(regeneration_verification_checks(x$checks))
}

#' Validate a manuscript regeneration verification record or bundle
#' @param x A verification record or written directory.
#' @param execution Optional linked regeneration execution.
#' @param plan Optional linked regeneration plan.
#' @param strict Whether every check must be verified.
#' @return `TRUE` invisibly.
#' @export
validate_manuscript_regeneration_verification <- function(x, execution = NULL, plan = NULL, strict = FALSE) {
  if (is.character(x) && length(x) == 1L) {
    required <- c("regeneration-verification.json", "regeneration-verification.md", "regeneration-verification.tsv", "regeneration-verification-manifest.tsv")
    missing <- required[!file.exists(file.path(x, required))]
    if (length(missing)) stop("regeneration verification directory is missing: ", paste(missing, collapse = ", "), call. = FALSE)
    manifest <- data.table::fread(file.path(x, "regeneration-verification-manifest.tsv"))
    for (i in seq_len(nrow(manifest))) {
      path <- file.path(x, manifest$path[[i]])
      actual <- if (file.exists(path)) digest::digest(path, algo = "sha256", file = TRUE) else ""
      if (!identical(actual, manifest$sha256[[i]])) stop("regeneration verification checksum mismatch: ", manifest$path[[i]], call. = FALSE)
    }
    return(invisible(TRUE))
  }
  if (!inherits(x, "PopgenVCFRegenerationVerification")) stop("x must be a PopgenVCFRegenerationVerification or directory", call. = FALSE)
  checks <- regeneration_verification_checks(x$checks)
  expected <- digest::digest(regeneration_verification_payload(x), algo = "sha256", serialize = TRUE)
  if (!identical(expected, x$digest)) stop("regeneration verification digest mismatch", call. = FALSE)
  if (!is.null(execution)) {
    validate_manuscript_regeneration_execution(execution, plan = plan)
    if (!identical(x$execution_digest, execution$digest)) stop("verification does not reference the supplied execution", call. = FALSE)
    completed <- as.data.frame(execution$actions[execution$actions$status == "completed", ], stringsAsFactors = FALSE)
    missing <- setdiff(completed$section_id, checks$section_id)
    if (length(missing)) stop("verification is missing completed sections: ", paste(missing, collapse = ", "), call. = FALSE)
    joined <- merge(checks, completed[c("section_id", "output_identity")], by = "section_id", suffixes = c("", ".execution"), sort = FALSE)
    bad <- joined$output_identity != joined$output_identity.execution
    if (any(bad)) stop("verification output identities do not match execution: ", paste(joined$section_id[bad], collapse = ", "), call. = FALSE)
  }
  identity_match <- checks$output_identity == checks$verified_identity
  if (any(checks$status == "verified" & !identity_match)) stop("verified checks require matching identities", call. = FALSE)
  if (any(checks$status == "mismatch" & identity_match)) stop("mismatch checks require different identities", call. = FALSE)
  if (isTRUE(strict) && any(checks$status != "verified")) stop("regeneration verification contains unverified checks", call. = FALSE)
  invisible(TRUE)
}

#' Render a manuscript regeneration verification record as Markdown
#' @param x A validated verification record.
#' @param execution Optional linked execution.
#' @param plan Optional linked plan.
#' @return Markdown lines.
#' @export
render_manuscript_regeneration_verification <- function(x, execution = NULL, plan = NULL) {
  validate_manuscript_regeneration_verification(x, execution = execution, plan = plan)
  checks <- manuscript_regeneration_verification_table(x)
  rows <- vapply(seq_len(nrow(checks)), function(i) {
    row <- checks[i]
    paste0("| `", row$section_id, "` | `", row$output_identity, "` | `", row$verified_identity, "` | ", row$status, " | `", row$verifier_id, "` | ", row$note, " |")
  }, character(1))
  c("# Manuscript regeneration verification", "",
    paste0("- Manuscript ID: `", x$manuscript_id, "`"),
    paste0("- Revision ID: `", x$revision_id, "`"),
    paste0("- Verification ID: `", x$verification_id, "`"),
    paste0("- Execution digest: `", x$execution_digest, "`"),
    paste0("- Verification digest: `", x$digest, "`"), "",
    "| Section | Expected identity | Verified identity | Status | Verifier | Note |",
    "|---|---|---|---|---|---|", rows)
}

#' Write a deterministic manuscript regeneration verification bundle
#' @param x A validated verification record.
#' @param path Output directory.
#' @param execution Optional linked execution.
#' @param plan Optional linked plan.
#' @param overwrite Whether an existing directory may be replaced.
#' @return Normalized output path invisibly.
#' @export
write_manuscript_regeneration_verification <- function(x, path, execution = NULL, plan = NULL, overwrite = FALSE) {
  validate_manuscript_regeneration_verification(x, execution = execution, plan = plan)
  if (dir.exists(path)) {
    if (!isTRUE(overwrite)) stop("output directory already exists", call. = FALSE)
    unlink(path, recursive = TRUE, force = TRUE)
  }
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(unclass(x), file.path(path, "regeneration-verification.json"), auto_unbox = TRUE, pretty = TRUE, null = "null")
  writeLines(render_manuscript_regeneration_verification(x, execution = execution, plan = plan), file.path(path, "regeneration-verification.md"), useBytes = TRUE)
  data.table::fwrite(manuscript_regeneration_verification_table(x), file.path(path, "regeneration-verification.tsv"), sep = "\t")
  files <- c("regeneration-verification.json", "regeneration-verification.md", "regeneration-verification.tsv")
  manifest <- data.table::data.table(path = files, size_bytes = as.numeric(file.info(file.path(path, files))$size), sha256 = vapply(file.path(path, files), digest::digest, character(1), algo = "sha256", file = TRUE))
  data.table::fwrite(manifest, file.path(path, "regeneration-verification-manifest.tsv"), sep = "\t")
  validate_manuscript_regeneration_verification(path)
  invisible(normalizePath(path, winslash = "/", mustWork = TRUE))
}
