#' Compare two immutable manuscript revisions
#'
#' @param before Earlier `PopgenVCFManuscriptRevision`.
#' @param after Later `PopgenVCFManuscriptRevision`.
#' @param annotations Optional author-supplied section explanations and reviewer-comment links.
#' @param strict Whether changed sections without explicit explanations raise an error.
#' @return A deterministic `PopgenVCFManuscriptRevisionDiff` table.
#' @export
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
    content_sha256_before == content_sha256_after & title_before == title_after, "unchanged",
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
