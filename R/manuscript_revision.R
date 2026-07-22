revision_scalar <- function(x, label, allow_empty = FALSE) {
  x <- as.character(x)[1L]
  if (is.na(x)) x <- ""
  x <- trimws(x)
  if (!allow_empty && !nzchar(x)) stop(label, " must be non-empty", call. = FALSE)
  x
}

revision_sections <- function(sections) {
  sections <- data.table::as.data.table(sections)
  required <- c("section_id", "title", "content")
  if (!all(required %in% names(sections))) {
    stop("sections must contain: ", paste(required, collapse = ", "), call. = FALSE)
  }
  out <- data.table::copy(sections)[, ..required]
  out[, section_id := trimws(as.character(section_id))]
  out[, title := trimws(as.character(title))]
  out[, content := as.character(content)]
  if (!nrow(out)) stop("sections must contain at least one section", call. = FALSE)
  if (anyNA(out$section_id) || any(!nzchar(out$section_id))) stop("section_id values must be non-empty", call. = FALSE)
  if (anyNA(out$title) || any(!nzchar(out$title))) stop("section titles must be non-empty", call. = FALSE)
  if (anyNA(out$content)) stop("section content must not be NA", call. = FALSE)
  if (anyDuplicated(out$section_id)) stop("section_id values must be unique", call. = FALSE)
  if (any(!grepl("^[a-z0-9][a-z0-9._-]*$", out$section_id))) {
    stop("section_id values must use lowercase letters, numbers, dots, underscores, or hyphens", call. = FALSE)
  }
  data.table::setorderv(out, "section_id")
  out[, content_sha256 := vapply(content, digest::digest, character(1L), algo = "sha256", serialize = FALSE)]
  out[, character_count := nchar(content, type = "chars")]
  out[, word_count := vapply(content, function(value) {
    value <- trimws(value)
    if (!nzchar(value)) return(0L)
    length(strsplit(value, "[[:space:]]+", perl = TRUE)[[1L]])
  }, integer(1L))]
  out
}

#' Create an immutable manuscript revision record
#'
#' @param manuscript_id Stable manuscript identifier.
#' @param revision_id Stable revision identifier.
#' @param sections Data frame containing section_id, title, and content.
#' @param parent_revision_id Optional parent revision identifier.
#' @param summary Optional explicit author-supplied revision summary.
#' @param created_by Optional explicit creator identifier.
#' @return A validated `PopgenVCFManuscriptRevision`.
#' @export
new_manuscript_revision <- function(manuscript_id, revision_id, sections,
                                    parent_revision_id = "", summary = "", created_by = "") {
  payload <- list(
    schema_version = "1.0",
    manuscript_id = revision_scalar(manuscript_id, "manuscript_id"),
    revision_id = revision_scalar(revision_id, "revision_id"),
    parent_revision_id = revision_scalar(parent_revision_id, "parent_revision_id", TRUE),
    summary = revision_scalar(summary, "summary", TRUE),
    created_by = revision_scalar(created_by, "created_by", TRUE),
    sections = revision_sections(sections)
  )
  if (nzchar(payload$parent_revision_id) && identical(payload$parent_revision_id, payload$revision_id)) {
    stop("parent_revision_id must differ from revision_id", call. = FALSE)
  }
  payload$digest <- digest::digest(payload, algo = "sha256", serialize = TRUE)
  revision <- structure(payload, class = "PopgenVCFManuscriptRevision")
  validate_manuscript_revision(revision)
  revision
}

#' Validate a manuscript revision record
#'
#' @param x A `PopgenVCFManuscriptRevision`.
#' @return `TRUE` invisibly.
#' @export
validate_manuscript_revision <- function(x) {
  if (!inherits(x, "PopgenVCFManuscriptRevision")) stop("x must be a PopgenVCFManuscriptRevision", call. = FALSE)
  if (!identical(x$schema_version, "1.0")) stop("unsupported manuscript revision schema version", call. = FALSE)
  revision_scalar(x$manuscript_id, "manuscript_id")
  revision_scalar(x$revision_id, "revision_id")
  revision_sections(x$sections)
  payload <- x[setdiff(names(x), "digest")]
  expected <- digest::digest(payload, algo = "sha256", serialize = TRUE)
  if (!identical(expected, x$digest)) stop("manuscript revision digest mismatch", call. = FALSE)
  invisible(TRUE)
}

revision_annotations <- function(annotations, section_ids) {
  if (is.null(annotations)) {
    return(data.table::data.table(section_id = character(), explanation = character(), reviewer_comments = character()))
  }
  out <- data.table::as.data.table(annotations)
  if (!"section_id" %in% names(out)) stop("annotations must contain section_id", call. = FALSE)
  if (!"explanation" %in% names(out)) out[, explanation := ""]
  if (!"reviewer_comments" %in% names(out)) out[, reviewer_comments := ""]
  out <- out[, .(
    section_id = trimws(as.character(section_id)),
    explanation = trimws(as.character(explanation)),
    reviewer_comments = trimws(as.character(reviewer_comments))
  )]
  if (anyDuplicated(out$section_id)) stop("annotations must contain at most one row per section_id", call. = FALSE)
  unknown <- setdiff(out$section_id, section_ids)
  if (length(unknown)) stop("annotations contain unknown section_id values: ", paste(sort(unknown), collapse = ", "), call. = FALSE)
  out
}

#' Compare two immutable manuscript revisions
#'
#' @param before Earlier `PopgenVCFManuscriptRevision`.
#' @param after Later `PopgenVCFManuscriptRevision`.
#' @param annotations Optional author-supplied section explanations and reviewer-comment links.
#' @param strict Whether changed sections without explicit explanations raise an error.
#' @return A deterministic `PopgenVCFManuscriptRevisionDiff` table.
#' @noRd
compare_manuscript_revisions <- function(before, after, annotations = NULL, strict = FALSE) {
  validate_manuscript_revision(before)
  validate_manuscript_revision(after)
  if (!identical(before$manuscript_id, after$manuscript_id)) stop("revisions must belong to the same manuscript", call. = FALSE)
  if (identical(before$revision_id, after$revision_id)) stop("revision_id values must differ", call. = FALSE)

  old <- data.table::copy(before$sections)
  new <- data.table::copy(after$sections)
  data.table::setnames(old, setdiff(names(old), "section_id"), paste0(setdiff(names(old), "section_id"), "_before"))
  data.table::setnames(new, setdiff(names(new), "section_id"), paste0(setdiff(names(new), "section_id"), "_after"))
  report <- merge(old, new, by = "section_id", all = TRUE, sort = TRUE)
  report[, change_type := data.table::fcase(
    is.na(content_sha256_before), "added",
    is.na(content_sha256_after), "removed",
    content_sha256_before == content_sha256_after && title_before == title_after, "unchanged",
    default = "modified"
  )]
  numeric_columns <- c("character_count_before", "character_count_after", "word_count_before", "word_count_after")
  for (column in numeric_columns) report[is.na(get(column)), (column) := 0L]
  report[, character_delta := character_count_after - character_count_before]
  report[, word_delta := word_count_after - word_count_before]
  report[is.na(title_before), title_before := ""]
  report[is.na(title_after), title_after := ""]
  report[is.na(content_sha256_before), content_sha256_before := ""]
  report[is.na(content_sha256_after), content_sha256_after := ""]

  notes <- revision_annotations(annotations, report$section_id)
  report <- merge(report, notes, by = "section_id", all.x = TRUE, sort = TRUE)
  report[is.na(explanation), explanation := ""]
  report[is.na(reviewer_comments), reviewer_comments := ""]
  report[, status := data.table::fcase(
    change_type == "unchanged", "unchanged",
    nzchar(explanation), "documented",
    default = "undocumented"
  )]
  report[, message := data.table::fcase(
    change_type == "unchanged", "Section content is unchanged",
    nzchar(explanation), "Explicit author-supplied explanation recorded",
    default = "Changed section has no author-supplied explanation"
  )]
  data.table::setcolorder(report, c(
    "section_id", "change_type", "status", "title_before", "title_after",
    "character_count_before", "character_count_after", "character_delta",
    "word_count_before", "word_count_after", "word_delta",
    "content_sha256_before", "content_sha256_after", "explanation",
    "reviewer_comments", "message"
  ))
  class(report) <- c("PopgenVCFManuscriptRevisionDiff", class(report))
  attr(report, "manuscript_id") <- before$manuscript_id
  attr(report, "before_revision_id") <- before$revision_id
  attr(report, "after_revision_id") <- after$revision_id
  attr(report, "before_digest") <- before$digest
  attr(report, "after_digest") <- after$digest
  if (isTRUE(strict) && any(report$status == "undocumented")) {
    stop("Changed manuscript sections lack explicit explanations: ", paste(report$section_id[report$status == "undocumented"], collapse = ", "), call. = FALSE)
  }
  report
}

#' Render a manuscript revision diff as Markdown
#'
#' @param diff A `PopgenVCFManuscriptRevisionDiff`.
#' @return Character vector containing Markdown.
#' @export
render_manuscript_revision_diff <- function(diff) {
  if (!inherits(diff, "PopgenVCFManuscriptRevisionDiff")) stop("diff must be a PopgenVCFManuscriptRevisionDiff", call. = FALSE)
  lines <- c(
    "# Manuscript revision diff", "",
    paste0("- Manuscript ID: `", attr(diff, "manuscript_id"), "`"),
    paste0("- Before revision: `", attr(diff, "before_revision_id"), "`"),
    paste0("- After revision: `", attr(diff, "after_revision_id"), "`"), "",
    "| Section | Change | Status | Words before | Words after | Delta | Explanation | Reviewer comments |",
    "|---|---|---|---:|---:|---:|---|---|"
  )
  rows <- vapply(seq_len(nrow(diff)), function(i) {
    values <- diff[i]
    paste0("| `", values$section_id, "` | ", values$change_type, " | ", values$status,
           " | ", values$word_count_before, " | ", values$word_count_after, " | ", values$word_delta,
           " | ", gsub("\\|", "\\\\|", values$explanation), " | ",
           gsub("\\|", "\\\\|", values$reviewer_comments), " |")
  }, character(1L))
  c(lines, rows)
}

#' Write a deterministic manuscript revision diff bundle
#'
#' @param before Earlier manuscript revision.
#' @param after Later manuscript revision.
#' @param directory Parent output directory.
#' @param annotations Optional explicit annotations.
#' @param overwrite Whether an existing bundle may be replaced.
#' @return Normalized output directory invisibly.
#' @export
write_manuscript_revision_diff <- function(before, after, directory, annotations = NULL, overwrite = FALSE) {
  diff <- compare_manuscript_revisions(before, after, annotations)
  out <- file.path(directory, "manuscript-revision-diff")
  if (dir.exists(out)) {
    if (!isTRUE(overwrite)) stop("manuscript revision diff directory already exists", call. = FALSE)
    unlink(out, recursive = TRUE, force = TRUE)
  }
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  metadata <- list(
    schema_version = "1.0",
    manuscript_id = attr(diff, "manuscript_id"),
    before_revision_id = attr(diff, "before_revision_id"),
    after_revision_id = attr(diff, "after_revision_id"),
    before_digest = attr(diff, "before_digest"),
    after_digest = attr(diff, "after_digest")
  )
  jsonlite::write_json(metadata, file.path(out, "revision-diff.json"), auto_unbox = TRUE, pretty = TRUE)
  data.table::fwrite(data.table::as.data.table(diff), file.path(out, "revision-diff.tsv"), sep = "\t")
  writeLines(render_manuscript_revision_diff(diff), file.path(out, "revision-diff.md"), useBytes = TRUE)
  files <- sort(list.files(out, full.names = TRUE))
  manifest <- data.table::data.table(
    path = basename(files),
    size_bytes = file.info(files)$size,
    sha256 = vapply(files, digest::digest, character(1L), algo = "sha256", file = TRUE)
  )
  data.table::fwrite(manifest, file.path(out, "revision-diff-manifest.tsv"), sep = "\t")
  invisible(normalizePath(out, winslash = "/", mustWork = TRUE))
}

#' Validate a written manuscript revision diff bundle
#'
#' @param directory Written bundle directory.
#' @return `TRUE` invisibly.
#' @export
validate_manuscript_revision_diff_bundle <- function(directory) {
  required <- c("revision-diff.json", "revision-diff.tsv", "revision-diff.md", "revision-diff-manifest.tsv")
  missing <- required[!file.exists(file.path(directory, required))]
  if (length(missing)) stop("manuscript revision diff bundle is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  manifest <- data.table::fread(file.path(directory, "revision-diff-manifest.tsv"))
  if (!all(c("path", "size_bytes", "sha256") %in% names(manifest))) stop("invalid manuscript revision diff manifest", call. = FALSE)
  for (i in seq_len(nrow(manifest))) {
    path <- file.path(directory, manifest$path[[i]])
    if (!file.exists(path)) stop("manuscript revision diff file is missing: ", manifest$path[[i]], call. = FALSE)
    if (!identical(digest::digest(path, algo = "sha256", file = TRUE), manifest$sha256[[i]])) {
      stop("manuscript revision diff checksum mismatch: ", manifest$path[[i]], call. = FALSE)
    }
  }
  invisible(TRUE)
}
