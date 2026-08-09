companion_text <- function(x, placeholder) {
  value <- manuscript_text(x, placeholder)
  trimws(value)
}

companion_people <- function(x) {
  if (is.null(x)) return(data.table::data.table(name = character(), email = character(), affiliation = character(), reason = character()))
  tab <- data.table::as.data.table(x)
  if (!"name" %in% names(tab)) stop("reviewer records must contain name", call. = FALSE)
  for (column in c("email", "affiliation", "reason")) if (!column %in% names(tab)) tab[, (column) := NA_character_]
  tab[, .(name = trimws(as.character(name)), email = as.character(email), affiliation = as.character(affiliation), reason = as.character(reason))]
}

#' Create deterministic submission companion documents
#'
#' @param manuscript A validated `PopgenVCFManuscript` or written manuscript directory.
#' @param journal,editor Journal and editor names supplied by the authors.
#' @param significance,novelty Author-supplied scientific statements.
#' @param highlights Author-supplied highlight statements.
#' @param suggested_reviewers,opposed_reviewers Reviewer metadata.
#' @param confirmations Named logical author confirmations.
#' @param max_highlights Maximum highlight count.
#' @param max_highlight_characters Maximum characters per highlight.
#' @return A `PopgenVCFSubmissionCompanions` object.
#' @export
new_submission_companions <- function(manuscript, journal = NULL, editor = NULL,
                                      significance = NULL, novelty = NULL,
                                      highlights = character(), suggested_reviewers = NULL,
                                      opposed_reviewers = NULL, confirmations = list(),
                                      max_highlights = 5L, max_highlight_characters = 125L) {
  if (is.character(manuscript) && length(manuscript) == 1L) {
    validate_manuscript(manuscript)
    manuscript <- readRDS(file.path(manuscript, "manuscript.rds"))
  }
  validate_manuscript(manuscript)
  confirmations <- project_named_list(confirmations, "confirmations")
  if (length(confirmations)) confirmations <- lapply(confirmations, as.logical)
  highlights <- trimws(as.character(highlights))
  highlights <- highlights[nzchar(highlights)]
  object <- structure(list(
    schema_version = "1.0",
    manuscript_project_id = manuscript$project_id,
    manuscript_publication_digest = manuscript$publication_digest,
    title = manuscript$title,
    corresponding_authors = manuscript$authors[corresponding == TRUE],
    journal = companion_text(journal, "[Target journal required.]"),
    editor = companion_text(editor, "[Handling editor, if known.]"),
    significance = companion_text(significance, "[Author-supplied significance statement required.]"),
    novelty = companion_text(novelty, "[Author-supplied novelty statement required.]"),
    highlights = highlights,
    suggested_reviewers = companion_people(suggested_reviewers),
    opposed_reviewers = companion_people(opposed_reviewers),
    confirmations = confirmations,
    declarations = manuscript$declarations,
    limits = list(max_highlights = as.integer(max_highlights)[1L], max_highlight_characters = as.integer(max_highlight_characters)[1L])
  ), class = "PopgenVCFSubmissionCompanions")
  object$digest <- digest::digest(object[setdiff(names(object), "digest")], algo = "sha256", serialize = TRUE)
  validate_submission_companions(object, strict = FALSE)
  object
}

submission_companion_missing <- function(x) {
  placeholders <- c(
    journal = "[Target journal required.]",
    significance = "[Author-supplied significance statement required.]",
    novelty = "[Author-supplied novelty statement required.]"
  )
  missing <- names(placeholders)[vapply(names(placeholders), function(n) identical(x[[n]], placeholders[[n]]), logical(1L))]
  if (!length(x$highlights)) missing <- c(missing, "highlights")
  if (!nrow(x$corresponding_authors)) missing <- c(missing, "corresponding_author")
  missing
}

#' Validate submission companion documents
#'
#' @param x A `PopgenVCFSubmissionCompanions` object or written companion directory.
#' @param strict Require all author-supplied fields.
#' @return `TRUE` invisibly.
#' @export
validate_submission_companions <- function(x, strict = TRUE) {
  if (is.character(x) && length(x) == 1L) {
    required <- c("cover-letter.md", "highlights.md", "author-declarations.md", "companions-record.json", "companions-manifest.tsv")
    absent <- required[!file.exists(file.path(x, required))]
    if (length(absent)) stop("companion directory is missing: ", paste(absent, collapse = ", "), call. = FALSE)
    manifest <- data.table::fread(file.path(x, "companions-manifest.tsv"))
    for (i in seq_len(nrow(manifest))) {
      path <- file.path(x, manifest$path[[i]])
      actual <- digest::digest(path, algo = "sha256", file = TRUE)
      if (!identical(actual, manifest$sha256[[i]])) stop("companion checksum mismatch: ", manifest$path[[i]], call. = FALSE)
    }
    return(invisible(TRUE))
  }
  if (!inherits(x, "PopgenVCFSubmissionCompanions")) stop("x must be a PopgenVCFSubmissionCompanions or directory", call. = FALSE)
  expected <- digest::digest(x[setdiff(names(x), "digest")], algo = "sha256", serialize = TRUE)
  if (!identical(expected, x$digest)) stop("companion digest mismatch", call. = FALSE)
  if (length(x$highlights) > x$limits$max_highlights) stop("too many highlights", call. = FALSE)
  if (length(x$highlights) && any(nchar(x$highlights, type = "chars") > x$limits$max_highlight_characters)) stop("highlight exceeds character limit", call. = FALSE)
  missing <- submission_companion_missing(x)
  if (isTRUE(strict) && length(missing)) stop("companion fields are incomplete: ", paste(missing, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}

render_cover_letter <- function(x) {
  authors <- if (nrow(x$corresponding_authors)) paste(x$corresponding_authors$name, collapse = ", ") else "[Corresponding author required.]"
  c(paste0("# Cover letter: ", x$title), "", paste0("**Journal:** ", x$journal), paste0("**Editor:** ", x$editor), "",
    "Dear Editor,", "", paste0("Please consider our manuscript, **", x$title, "**, for publication."), "",
    "## Significance", "", x$significance, "", "## Novelty", "", x$novelty, "",
    "## Author confirmations", "", if (length(x$confirmations)) paste0("- ", names(x$confirmations), ": ", vapply(x$confirmations, function(z) if (isTRUE(z[[1L]])) "confirmed" else "not confirmed", character(1L))) else "- [Author confirmations required.]", "",
    paste0("Sincerely,  ", authors))
}

render_highlights <- function(x) c("# Highlights", "", if (length(x$highlights)) paste0("- ", x$highlights) else "- [Author-supplied highlights required.]")

render_author_declarations <- function(x) {
  declarations <- x$declarations
  c("# Author declarations", "", unlist(Map(function(name, value) c(paste0("## ", gsub("_", " ", name)), "", manuscript_text(value, "[Statement required.]"), ""), names(declarations), declarations), use.names = FALSE))
}

#' Write deterministic submission companion documents
#'
#' @param companions A `PopgenVCFSubmissionCompanions` object.
#' @param directory Output directory.
#' @param overwrite Permit replacement of a non-empty directory.
#' @param strict Require complete author inputs before writing.
#' @return Normalized directory invisibly.
#' @export
write_submission_companions <- function(companions, directory, overwrite = FALSE, strict = FALSE) {
  validate_submission_companions(companions, strict = strict)
  if (dir.exists(directory) && length(list.files(directory, all.files = TRUE, no.. = TRUE)) && !isTRUE(overwrite)) stop("companion directory is not empty", call. = FALSE)
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  writeLines(render_cover_letter(companions), file.path(directory, "cover-letter.md"), useBytes = TRUE)
  writeLines(render_highlights(companions), file.path(directory, "highlights.md"), useBytes = TRUE)
  writeLines(render_author_declarations(companions), file.path(directory, "author-declarations.md"), useBytes = TRUE)
  record <- list(schema_version = "1.0", project_id = companions$manuscript_project_id, publication_digest = companions$manuscript_publication_digest, companion_digest = companions$digest, missing = submission_companion_missing(companions), roles = list(cover_letter = "cover-letter.md", highlights = "highlights.md", author_declarations = "author-declarations.md"))
  jsonlite::write_json(record, file.path(directory, "companions-record.json"), auto_unbox = TRUE, pretty = TRUE, null = "null")
  files <- list.files(directory, full.names = TRUE)
  files <- files[basename(files) != "companions-manifest.tsv"]
  manifest <- data.table::data.table(path = basename(files), role = c(author_declarations = "author-declarations.md", cover_letter = "cover-letter.md", highlights = "highlights.md", record = "companions-record.json")[basename(files)], size_bytes = file.info(files)$size, sha256 = vapply(files, digest::digest, character(1L), algo = "sha256", file = TRUE))
  data.table::setorderv(manifest, "path")
  data.table::fwrite(manifest, file.path(directory, "companions-manifest.tsv"), sep = "\t")
  validate_submission_companions(directory, strict = FALSE)
  invisible(normalizePath(directory, winslash = "/"))
}
