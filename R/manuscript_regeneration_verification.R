regeneration_verification_reviews <- function(x) {
  required <- c("section_id", "decision", "reviewer_id", "evidence_identity", "note")
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  if (!all(required %in% names(x))) {
    stop("reviews must contain: ", paste(required, collapse = ", "), call. = FALSE)
  }
  x <- x[, required, drop = FALSE]
  for (column in required) x[[column]] <- trimws(as.character(x[[column]]))
  if (!nrow(x)) stop("reviews must contain at least one row", call. = FALSE)
  if (anyNA(x[c("section_id", "decision", "reviewer_id")]) ||
      any(!nzchar(unlist(x[c("section_id", "decision", "reviewer_id")], use.names = FALSE)))) {
    stop("section_id, decision, and reviewer_id must be non-empty", call. = FALSE)
  }
  if (anyDuplicated(x$section_id)) stop("reviews must contain unique section_id values", call. = FALSE)
  if (any(!x$decision %in% c("accepted", "rejected", "manual_review"))) {
    stop("invalid regeneration verification decision", call. = FALSE)
  }
  x$evidence_identity[is.na(x$evidence_identity)] <- ""
  x$note[is.na(x$note)] <- ""
  if (any(x$decision == "accepted" & !nzchar(x$evidence_identity))) {
    stop("accepted reviews require evidence_identity", call. = FALSE)
  }
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
    reviews = as.data.frame(x$reviews, stringsAsFactors = FALSE)
  )
}

#' Deterministic manuscript regeneration verification records
#'
#' Create, validate, render, and write immutable section-level verification
#' decisions linked to a manuscript regeneration execution.
#'
#' @param execution A validated `PopgenVCFRegenerationExecution`.
#' @param reviews Data frame containing section review decisions.
#' @param verification_id Stable verification identifier.
#' @param x A verification record or written verification directory.
#' @param plan Optional linked `PopgenVCFRegenerationPlan`.
#' @param strict Whether every required review must be accepted.
#' @param path Output directory.
#' @param overwrite Whether an existing directory may be replaced.
#' @return `new_manuscript_regeneration_verification()` returns a validated
#'   `PopgenVCFRegenerationVerification`; the table and renderer return canonical
#'   representations; validators return `TRUE` invisibly; the writer returns the
#'   normalized output path invisibly.
#' @name manuscript-regeneration-verification
NULL

#' @rdname manuscript-regeneration-verification
#' @export
new_manuscript_regeneration_verification <- function(execution, reviews, verification_id) {
  validate_manuscript_regeneration_execution(execution)
  reviews <- regeneration_verification_reviews(reviews)
  out <- list(
    schema_version = "1.0",
    manuscript_id = execution$manuscript_id,
    revision_id = execution$revision_id,
    plan_digest = execution$plan_digest,
    execution_digest = execution$digest,
    verification_id = regeneration_id(verification_id, "verification_id"),
    reviews = data.table::as.data.table(reviews)
  )
  out$digest <- digest::digest(regeneration_verification_payload(out), algo = "sha256", serialize = TRUE)
  out <- structure(out, class = "PopgenVCFRegenerationVerification")
  validate_manuscript_regeneration_verification(out, execution = execution)
  out
}

#' @rdname manuscript-regeneration-verification
#' @export
manuscript_regeneration_verification_table <- function(x) {
  if (!inherits(x, "PopgenVCFRegenerationVerification")) {
    stop("x must be a PopgenVCFRegenerationVerification", call. = FALSE)
  }
  data.table::as.data.table(regeneration_verification_reviews(x$reviews))
}

#' @rdname manuscript-regeneration-verification
#' @param execution Optional linked `PopgenVCFRegenerationExecution` when validating.
#' @export
validate_manuscript_regeneration_verification <- function(x, execution = NULL, plan = NULL, strict = FALSE) {
  if (is.character(x) && length(x) == 1L) {
    required <- c("regeneration-verification.json", "regeneration-verification.md",
                  "regeneration-verification.tsv", "regeneration-verification-manifest.tsv")
    missing <- required[!file.exists(file.path(x, required))]
    if (length(missing)) stop("regeneration verification directory is missing: ", paste(missing, collapse = ", "), call. = FALSE)
    manifest <- data.table::fread(file.path(x, "regeneration-verification-manifest.tsv"))
    for (i in seq_len(nrow(manifest))) {
      file <- file.path(x, manifest$path[[i]])
      actual <- if (file.exists(file)) digest::digest(file, algo = "sha256", file = TRUE) else ""
      if (!identical(actual, manifest$sha256[[i]])) {
        stop("regeneration verification checksum mismatch: ", manifest$path[[i]], call. = FALSE)
      }
    }
    return(invisible(TRUE))
  }
  if (!inherits(x, "PopgenVCFRegenerationVerification")) {
    stop("x must be a PopgenVCFRegenerationVerification or directory", call. = FALSE)
  }
  reviews <- regeneration_verification_reviews(x$reviews)
  expected <- digest::digest(regeneration_verification_payload(x), algo = "sha256", serialize = TRUE)
  if (!identical(expected, x$digest)) stop("regeneration verification digest mismatch", call. = FALSE)

  if (!is.null(execution)) {
    validate_manuscript_regeneration_execution(execution, plan = plan)
    if (!identical(x$execution_digest, execution$digest)) {
      stop("verification does not reference the supplied regeneration execution", call. = FALSE)
    }
    required_actions <- as.data.frame(execution$actions[
      execution$actions$status == "completed" & nzchar(execution$actions$output_identity),
    ], stringsAsFactors = FALSE)
    unknown <- setdiff(reviews$section_id, required_actions$section_id)
    if (length(unknown)) stop("reviews reference unknown or unverifiable sections: ", paste(unknown, collapse = ", "), call. = FALSE)
    missing <- setdiff(required_actions$section_id, reviews$section_id)
    if (length(missing)) stop("verification is missing required sections: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (isTRUE(strict)) {
    unresolved <- reviews$decision != "accepted"
    if (any(unresolved)) stop("regeneration verification contains unaccepted sections: ", paste(reviews$section_id[unresolved], collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

#' @rdname manuscript-regeneration-verification
#' @export
render_manuscript_regeneration_verification <- function(x, execution = NULL, plan = NULL) {
  validate_manuscript_regeneration_verification(x, execution = execution, plan = plan)
  reviews <- manuscript_regeneration_verification_table(x)
  rows <- vapply(seq_len(nrow(reviews)), function(i) {
    row <- reviews[i]
    paste0("| `", row$section_id, "` | ", row$decision, " | `", row$reviewer_id,
           "` | `", row$evidence_identity, "` | ", row$note, " |")
  }, character(1))
  c(
    "# Manuscript regeneration verification", "",
    paste0("- Manuscript ID: `", x$manuscript_id, "`"),
    paste0("- Revision ID: `", x$revision_id, "`"),
    paste0("- Verification ID: `", x$verification_id, "`"),
    paste0("- Execution digest: `", x$execution_digest, "`"),
    paste0("- Verification digest: `", x$digest, "`"), "",
    "This record stores explicit review decisions only; it does not determine scientific correctness.", "",
    "| Section | Decision | Reviewer | Evidence identity | Note |",
    "|---|---|---|---|---|", rows
  )
}

#' @rdname manuscript-regeneration-verification
#' @export
write_manuscript_regeneration_verification <- function(x, path, execution = NULL, plan = NULL, overwrite = FALSE) {
  validate_manuscript_regeneration_verification(x, execution = execution, plan = plan)
  if (dir.exists(path)) {
    if (!isTRUE(overwrite)) stop("output directory already exists", call. = FALSE)
    unlink(path, recursive = TRUE, force = TRUE)
  }
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(unclass(x), file.path(path, "regeneration-verification.json"), auto_unbox = TRUE, pretty = TRUE, null = "null")
  writeLines(render_manuscript_regeneration_verification(x, execution = execution, plan = plan),
             file.path(path, "regeneration-verification.md"), useBytes = TRUE)
  data.table::fwrite(manuscript_regeneration_verification_table(x),
                     file.path(path, "regeneration-verification.tsv"), sep = "\t")
  files <- c("regeneration-verification.json", "regeneration-verification.md", "regeneration-verification.tsv")
  manifest <- data.table::data.table(
    path = files,
    size_bytes = as.numeric(file.info(file.path(path, files))$size),
    sha256 = vapply(file.path(path, files), digest::digest, character(1), algo = "sha256", file = TRUE)
  )
  data.table::fwrite(manifest, file.path(path, "regeneration-verification-manifest.tsv"), sep = "\t")
  validate_manuscript_regeneration_verification(path)
  invisible(normalizePath(path, winslash = "/", mustWork = TRUE))
}