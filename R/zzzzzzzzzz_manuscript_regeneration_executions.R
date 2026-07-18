regeneration_execution_actions <- function(x) {
  required <- c("section_id", "action", "status", "executor_id", "output_identity", "note")
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  if (!all(required %in% names(x))) {
    stop("actions must contain: ", paste(required, collapse = ", "), call. = FALSE)
  }
  x <- x[, required, drop = FALSE]
  for (column in required) x[[column]] <- trimws(as.character(x[[column]]))
  if (!nrow(x)) stop("actions must contain at least one row", call. = FALSE)
  if (anyNA(x[c("section_id", "action", "status", "executor_id")]) ||
      any(!nzchar(unlist(x[c("section_id", "action", "status", "executor_id")], use.names = FALSE)))) {
    stop("section_id, action, status, and executor_id must be non-empty", call. = FALSE)
  }
  if (anyDuplicated(x$section_id)) stop("actions must contain unique section_id values", call. = FALSE)
  if (any(!x$action %in% c("regenerate", "manual_review", "resolve_block", "no_action"))) {
    stop("invalid regeneration action", call. = FALSE)
  }
  if (any(!x$status %in% c("pending", "completed", "skipped", "failed"))) {
    stop("invalid regeneration action status", call. = FALSE)
  }
  x$output_identity[is.na(x$output_identity)] <- ""
  x$note[is.na(x$note)] <- ""
  x <- x[order(x$section_id), , drop = FALSE]
  rownames(x) <- NULL
  x
}

regeneration_execution_payload <- function(x) {
  list(
    schema_version = x$schema_version,
    manuscript_id = x$manuscript_id,
    revision_id = x$revision_id,
    plan_digest = x$plan_digest,
    execution_id = x$execution_id,
    actions = as.data.frame(x$actions, stringsAsFactors = FALSE)
  )
}

#' Create a deterministic manuscript regeneration execution record
#'
#' @param plan A validated `PopgenVCFRegenerationPlan`.
#' @param actions Data frame of explicit section actions.
#' @param execution_id Stable execution identifier.
#' @return A validated `PopgenVCFRegenerationExecution`.
#' @export
new_manuscript_regeneration_execution <- function(plan, actions, execution_id) {
  validate_manuscript_regeneration_plan(plan)
  actions <- regeneration_execution_actions(actions)
  out <- list(
    schema_version = "1.0",
    manuscript_id = plan$manuscript_id,
    revision_id = plan$revision_id,
    plan_digest = plan$digest,
    execution_id = regeneration_id(execution_id, "execution_id"),
    actions = data.table::as.data.table(actions)
  )
  out$digest <- digest::digest(regeneration_execution_payload(out), algo = "sha256", serialize = TRUE)
  out <- structure(out, class = "PopgenVCFRegenerationExecution")
  validate_manuscript_regeneration_execution(out, plan = plan)
  out
}

#' Return the canonical regeneration execution table
#'
#' @param x A regeneration execution record.
#' @return A deterministic data table.
#' @export
manuscript_regeneration_execution_table <- function(x) {
  if (!inherits(x, "PopgenVCFRegenerationExecution")) {
    stop("x must be a PopgenVCFRegenerationExecution", call. = FALSE)
  }
  data.table::as.data.table(regeneration_execution_actions(x$actions))
}

#' Validate a manuscript regeneration execution record or bundle
#'
#' @param x A `PopgenVCFRegenerationExecution` or written directory.
#' @param plan Optional linked `PopgenVCFRegenerationPlan`.
#' @param strict Whether pending, failed, skipped required, or unresolved actions raise an error.
#' @return `TRUE` invisibly.
#' @export
validate_manuscript_regeneration_execution <- function(x, plan = NULL, strict = FALSE) {
  if (is.character(x) && length(x) == 1L) {
    required <- c("regeneration-execution.json", "regeneration-execution.md", "regeneration-execution.tsv", "regeneration-execution-manifest.tsv")
    missing <- required[!file.exists(file.path(x, required))]
    if (length(missing)) stop("regeneration execution directory is missing: ", paste(missing, collapse = ", "), call. = FALSE)
    manifest <- data.table::fread(file.path(x, "regeneration-execution-manifest.tsv"))
    for (i in seq_len(nrow(manifest))) {
      path <- file.path(x, manifest$path[[i]])
      actual <- if (file.exists(path)) digest::digest(path, algo = "sha256", file = TRUE) else ""
      if (!identical(actual, manifest$sha256[[i]])) {
        stop("regeneration execution checksum mismatch: ", manifest$path[[i]], call. = FALSE)
      }
    }
    return(invisible(TRUE))
  }
  if (!inherits(x, "PopgenVCFRegenerationExecution")) {
    stop("x must be a PopgenVCFRegenerationExecution or directory", call. = FALSE)
  }
  actions <- regeneration_execution_actions(x$actions)
  expected <- digest::digest(regeneration_execution_payload(x), algo = "sha256", serialize = TRUE)
  if (!identical(expected, x$digest)) stop("regeneration execution digest mismatch", call. = FALSE)

  if (!is.null(plan)) {
    validate_manuscript_regeneration_plan(plan)
    if (!identical(x$plan_digest, plan$digest)) stop("execution does not reference the supplied regeneration plan", call. = FALSE)
    unknown <- setdiff(actions$section_id, plan$plan$section_id)
    if (length(unknown)) stop("actions reference unknown sections: ", paste(unknown, collapse = ", "), call. = FALSE)
    required_plan <- as.data.frame(plan$plan[plan$plan$state != "unaffected", ], stringsAsFactors = FALSE)
    missing <- setdiff(required_plan$section_id, actions$section_id)
    if (length(missing)) stop("execution is missing required sections: ", paste(missing, collapse = ", "), call. = FALSE)
    joined <- merge(actions, required_plan[c("section_id", "state")], by = "section_id", all.x = TRUE, sort = FALSE)
    expected_action <- c(affected = "regenerate", manual_review = "manual_review", blocked = "resolve_block")
    bad <- !is.na(joined$state) & joined$action != unname(expected_action[joined$state])
    if (any(bad)) stop("actions are incompatible with regeneration-plan states: ", paste(joined$section_id[bad], collapse = ", "), call. = FALSE)
  }

  needs_output <- actions$action %in% c("regenerate", "resolve_block") & actions$status == "completed"
  if (any(needs_output & !nzchar(actions$output_identity))) {
    stop("completed regeneration and block-resolution actions require output_identity", call. = FALSE)
  }
  if (isTRUE(strict)) {
    incomplete <- actions$status != "completed"
    if (any(incomplete)) stop("regeneration execution contains incomplete actions: ", paste(actions$section_id[incomplete], collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

#' Render a manuscript regeneration execution record as Markdown
#'
#' @param x A validated execution record.
#' @param plan Optional linked regeneration plan.
#' @return Markdown lines.
#' @export
render_manuscript_regeneration_execution <- function(x, plan = NULL) {
  validate_manuscript_regeneration_execution(x, plan = plan)
  actions <- manuscript_regeneration_execution_table(x)
  rows <- vapply(seq_len(nrow(actions)), function(i) {
    row <- actions[i]
    paste0("| `", row$section_id, "` | ", row$action, " | ", row$status, " | `", row$executor_id,
           "` | `", row$output_identity, "` | ", row$note, " |")
  }, character(1))
  c(
    "# Manuscript regeneration execution", "",
    paste0("- Manuscript ID: `", x$manuscript_id, "`"),
    paste0("- Revision ID: `", x$revision_id, "`"),
    paste0("- Execution ID: `", x$execution_id, "`"),
    paste0("- Plan digest: `", x$plan_digest, "`"),
    paste0("- Execution digest: `", x$digest, "`"), "",
    "This record documents explicit actions only. It does not assess scientific correctness or rewrite manuscript prose.", "",
    "| Section | Action | Status | Executor | Output identity | Note |",
    "|---|---|---|---|---|---|", rows
  )
}

#' Write a deterministic manuscript regeneration execution bundle
#'
#' @param x A validated execution record.
#' @param path Output directory.
#' @param plan Optional linked regeneration plan.
#' @param overwrite Whether an existing directory may be replaced.
#' @return Normalized output path invisibly.
#' @export
write_manuscript_regeneration_execution <- function(x, path, plan = NULL, overwrite = FALSE) {
  validate_manuscript_regeneration_execution(x, plan = plan)
  if (dir.exists(path)) {
    if (!isTRUE(overwrite)) stop("output directory already exists", call. = FALSE)
    unlink(path, recursive = TRUE, force = TRUE)
  }
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(unclass(x), file.path(path, "regeneration-execution.json"), auto_unbox = TRUE, pretty = TRUE, null = "null")
  writeLines(render_manuscript_regeneration_execution(x, plan = plan), file.path(path, "regeneration-execution.md"), useBytes = TRUE)
  data.table::fwrite(manuscript_regeneration_execution_table(x), file.path(path, "regeneration-execution.tsv"), sep = "\t")
  files <- c("regeneration-execution.json", "regeneration-execution.md", "regeneration-execution.tsv")
  manifest <- data.table::data.table(
    path = files,
    size_bytes = as.numeric(file.info(file.path(path, files))$size),
    sha256 = vapply(file.path(path, files), digest::digest, character(1), algo = "sha256", file = TRUE)
  )
  data.table::fwrite(manifest, file.path(path, "regeneration-execution-manifest.tsv"), sep = "\t")
  validate_manuscript_regeneration_execution(path)
  invisible(normalizePath(path, winslash = "/", mustWork = TRUE))
}
