reviewer_response_text <- function(x) {
  x <- trimws(as.character(x %||% ""))
  x[is.na(x)] <- ""
  x
}

reviewer_response_comments <- function(comments) {
  comments <- data.table::as.data.table(comments)
  required <- c("reviewer", "comment_id", "section", "comment", "status",
                "response", "action", "evidence", "location")
  missing <- setdiff(required, names(comments))
  if (length(missing)) {
    stop("comments must contain: ", paste(required, collapse = ", "), call. = FALSE)
  }
  comments <- data.table::copy(comments)[, ..required]
  for (column in required) comments[[column]] <- reviewer_response_text(comments[[column]])
  if (!nrow(comments)) stop("comments must contain at least one reviewer comment", call. = FALSE)
  if (any(!nzchar(comments$reviewer))) stop("reviewer values must be non-empty", call. = FALSE)
  if (any(!nzchar(comments$comment_id))) stop("comment_id values must be non-empty", call. = FALSE)
  if (any(!nzchar(comments$comment))) stop("reviewer comments must be non-empty", call. = FALSE)
  key <- paste(comments$reviewer, comments$comment_id, sep = "::")
  if (anyDuplicated(key)) stop("reviewer/comment_id pairs must be unique", call. = FALSE)
  allowed <- c("unanswered", "addressed", "partially_addressed", "declined", "not_applicable")
  if (any(!comments$status %in% allowed)) {
    stop("status must be one of: ", paste(allowed, collapse = ", "), call. = FALSE)
  }
  data.table::setorderv(comments, c("reviewer", "comment_id"))
  comments[]
}

#' Create a deterministic reviewer-response record
#'
#' @param comments Data frame containing reviewer comments and explicit responses.
#' @param manuscript_id Stable manuscript identifier.
#' @param revision_id Stable revision identifier.
#' @param title Human-readable response title.
#' @param version Schema content version.
#' @return A validated `PopgenVCFReviewerResponse`.
#' @export
new_reviewer_response <- function(comments, manuscript_id, revision_id,
                                  title = "Response to reviewers", version = "1.0") {
  manuscript_id <- reviewer_response_text(manuscript_id)[1L]
  revision_id <- reviewer_response_text(revision_id)[1L]
  title <- reviewer_response_text(title)[1L]
  version <- reviewer_response_text(version)[1L]
  if (!nzchar(manuscript_id)) stop("manuscript_id must be non-empty", call. = FALSE)
  if (!nzchar(revision_id)) stop("revision_id must be non-empty", call. = FALSE)
  if (!nzchar(title)) stop("title must be non-empty", call. = FALSE)
  if (!nzchar(version)) stop("version must be non-empty", call. = FALSE)
  payload <- list(
    schema_version = "1.0",
    manuscript_id = manuscript_id,
    revision_id = revision_id,
    title = title,
    version = version,
    comments = reviewer_response_comments(comments)
  )
  payload$digest <- digest::digest(payload, algo = "sha256", serialize = TRUE)
  out <- structure(payload, class = "PopgenVCFReviewerResponse")
  validate_reviewer_response(out)
  out
}

#' Validate a reviewer-response record or written bundle
#'
#' @param x A `PopgenVCFReviewerResponse` or bundle directory.
#' @return `TRUE` invisibly.
#' @export
validate_reviewer_response <- function(x) {
  if (is.character(x) && length(x) == 1L) {
    required <- c("reviewer-response.json", "reviewer-response.md",
                  "reviewer-response.tsv", "reviewer-response-report.tsv",
                  "reviewer-response-manifest.tsv")
    missing <- required[!file.exists(file.path(x, required))]
    if (length(missing)) stop("reviewer-response bundle is missing: ", paste(missing, collapse = ", "), call. = FALSE)
    manifest <- data.table::fread(file.path(x, "reviewer-response-manifest.tsv"))
    if (!all(c("path", "size_bytes", "sha256") %in% names(manifest))) {
      stop("reviewer-response manifest is malformed", call. = FALSE)
    }
    for (i in seq_len(nrow(manifest))) {
      path <- file.path(x, manifest$path[[i]])
      if (!file.exists(path)) stop("reviewer-response file is missing: ", manifest$path[[i]], call. = FALSE)
      actual <- digest::digest(path, algo = "sha256", file = TRUE)
      if (!identical(actual, manifest$sha256[[i]])) {
        stop("reviewer-response checksum mismatch: ", manifest$path[[i]], call. = FALSE)
      }
    }
    return(invisible(TRUE))
  }
  if (!inherits(x, "PopgenVCFReviewerResponse")) {
    stop("x must be a PopgenVCFReviewerResponse or directory", call. = FALSE)
  }
  if (!identical(x$schema_version, "1.0")) stop("unsupported reviewer-response schema version", call. = FALSE)
  comments <- reviewer_response_comments(x$comments)
  if (!identical(comments, x$comments)) stop("reviewer comments are not canonically ordered", call. = FALSE)
  for (field in c("manuscript_id", "revision_id", "title", "version")) {
    if (!is.character(x[[field]]) || length(x[[field]]) != 1L || !nzchar(x[[field]])) {
      stop(field, " must be one non-empty character value", call. = FALSE)
    }
  }
  payload <- x[setdiff(names(x), "digest")]
  expected <- digest::digest(payload, algo = "sha256", serialize = TRUE)
  if (!identical(expected, x$digest)) stop("reviewer-response digest mismatch", call. = FALSE)
  invisible(TRUE)
}

#' Return reviewer responses as a deterministic table
#'
#' @param x A validated reviewer-response record.
#' @return A data table.
#' @export
reviewer_response_table <- function(x) {
  validate_reviewer_response(x)
  data.table::copy(x$comments)
}

#' Evaluate reviewer-response completeness
#'
#' @param x A validated reviewer-response record.
#' @param strict Whether incomplete comments raise an error.
#' @return A deterministic completion report.
#' @export
reviewer_response_report <- function(x, strict = FALSE) {
  validate_reviewer_response(x)
  comments <- reviewer_response_table(x)
  complete <- logical(nrow(comments))
  message <- character(nrow(comments))
  for (i in seq_len(nrow(comments))) {
    row <- comments[i]
    has_response <- nzchar(row$response)
    has_action <- nzchar(row$action) || nzchar(row$location)
    has_evidence <- nzchar(row$evidence) || nzchar(row$location)
    if (row$status == "addressed") {
      complete[[i]] <- has_response && has_action && has_evidence
      message[[i]] <- "Addressed comments require a response, action/location, and evidence/location."
    } else if (row$status == "partially_addressed") {
      complete[[i]] <- has_response && has_action
      message[[i]] <- "Partially addressed comments require a response and explicit action."
    } else if (row$status %in% c("declined", "not_applicable")) {
      complete[[i]] <- has_response
      message[[i]] <- "Declined or not-applicable comments require an explicit rationale."
    } else {
      complete[[i]] <- FALSE
      message[[i]] <- "The reviewer comment remains unanswered."
    }
  }
  report <- comments[, .(reviewer, comment_id, status)]
  report[, completion := ifelse(complete, "complete", "incomplete")]
  report[, message := message]
  data.table::setorderv(report, c("reviewer", "comment_id"))
  class(report) <- c("PopgenVCFReviewerResponseReport", class(report))
  if (isTRUE(strict) && any(report$completion == "incomplete")) {
    ids <- paste(report$reviewer[report$completion == "incomplete"],
                 report$comment_id[report$completion == "incomplete"], sep = ":")
    stop("Reviewer responses are incomplete: ", paste(ids, collapse = ", "), call. = FALSE)
  }
  report
}

#' Render a reviewer-response letter as Markdown
#'
#' @param x A validated reviewer-response record.
#' @return Character vector containing Markdown.
#' @export
render_reviewer_response <- function(x) {
  validate_reviewer_response(x)
  lines <- c(
    paste0("# ", x$title), "",
    paste0("- Manuscript ID: `", x$manuscript_id, "`"),
    paste0("- Revision ID: `", x$revision_id, "`"),
    paste0("- Digest: `", x$digest, "`"), ""
  )
  comments <- reviewer_response_table(x)
  for (i in seq_len(nrow(comments))) {
    row <- comments[i]
    lines <- c(lines,
      paste0("## ", row$reviewer, " - ", row$comment_id), "",
      "### Reviewer comment", "", row$comment, "",
      paste0("**Status:** ", row$status), "",
      "### Author response", "", if (nzchar(row$response)) row$response else "[Author response required]", "",
      "### Manuscript action", "", if (nzchar(row$action)) row$action else "[Action not supplied]", "",
      paste0("**Section:** ", if (nzchar(row$section)) row$section else "not specified"),
      paste0("**Location:** ", if (nzchar(row$location)) row$location else "not specified"),
      paste0("**Evidence:** ", if (nzchar(row$evidence)) row$evidence else "not specified"), ""
    )
  }
  lines
}

#' Write a deterministic reviewer-response bundle
#'
#' @param x A validated reviewer-response record.
#' @param directory Parent output directory.
#' @param overwrite Whether an existing bundle may be replaced.
#' @return Normalized bundle directory invisibly.
#' @export
write_reviewer_response <- function(x, directory, overwrite = FALSE) {
  validate_reviewer_response(x)
  out <- file.path(directory, "reviewer-response")
  if (dir.exists(out)) {
    if (!isTRUE(overwrite)) stop("reviewer-response directory already exists", call. = FALSE)
    unlink(out, recursive = TRUE, force = TRUE)
  }
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(unclass(x), file.path(out, "reviewer-response.json"),
                       auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null")
  writeLines(render_reviewer_response(x), file.path(out, "reviewer-response.md"), useBytes = TRUE)
  data.table::fwrite(reviewer_response_table(x), file.path(out, "reviewer-response.tsv"), sep = "\t")
  data.table::fwrite(reviewer_response_report(x), file.path(out, "reviewer-response-report.tsv"), sep = "\t")
  files <- sort(list.files(out, full.names = TRUE))
  manifest <- data.table::data.table(
    path = basename(files),
    size_bytes = file.info(files)$size,
    sha256 = vapply(files, digest::digest, character(1L), algo = "sha256", file = TRUE)
  )
  data.table::fwrite(manifest, file.path(out, "reviewer-response-manifest.tsv"), sep = "\t")
  validate_reviewer_response(out)
  invisible(normalizePath(out, winslash = "/", mustWork = TRUE))
}
