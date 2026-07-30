manuscript_text <- function(x, default = "") {
  if (is.null(x) || !length(x) || is.na(x[[1L]])) return(default)
  as.character(x[[1L]])
}

manuscript_authors <- function(authors = NULL) {
  if (is.null(authors)) {
    return(data.table::data.table(
      name = character(), affiliation = character(), email = character(),
      orcid = character(), corresponding = logical()
    ))
  }
  authors <- data.table::as.data.table(authors)
  if (!"name" %in% names(authors)) stop("authors must contain a name column", call. = FALSE)
  for (column in c("affiliation", "email", "orcid")) {
    if (!column %in% names(authors)) authors[, (column) := NA_character_]
  }
  if (!"corresponding" %in% names(authors)) authors[, corresponding := FALSE]
  authors[, name := trimws(as.character(name))]
  if (anyNA(authors$name) || any(!nzchar(authors$name))) stop("author names must be non-empty", call. = FALSE)
  authors[, corresponding := as.logical(corresponding)]
  authors[, .(name, affiliation = as.character(affiliation), email = as.character(email),
              orcid = as.character(orcid), corresponding)]
}

manuscript_default_declarations <- function(project) {
  project_id <- manuscript_text(project$project_id, "unknown")
  list(
    data_availability = paste0(
      "The analysis inputs and derived artifacts are identified in the popgenVCF project record ",
      "`", project_id, "`. Public repository accessions or restricted-access instructions must be supplied by the authors."
    ),
    software_availability = paste0(
      "Analyses were performed with popgenVCF ", manuscript_text(project$package_version, "unknown"),
      ". Software versions and analysis parameters are preserved in the publication companion and project provenance records."
    ),
    reproducibility = paste0(
      "The complete analysis context, including input identities, parameters, software identity, random-number state, ",
      "canonical results, artifacts, checksums, and provenance, is preserved in the popgenVCF project record `",
      project_id, "`."
    ),
    competing_interests = "The authors must provide a competing-interests statement.",
    funding = "The authors must provide funding information.",
    author_contributions = "The authors must provide an author-contributions statement."
  )
}

#' Create a canonical manuscript specification
#'
#' @param project A validated `PopgenVCFProject`.
#' @param publication A `PopgenVCFPublicationBundle`; generated from `project` when omitted.
#' @param title Manuscript title.
#' @param authors Author metadata with at least a `name` column.
#' @param abstract Author-supplied abstract text.
#' @param keywords Character vector of keywords.
#' @param introduction Author-supplied introduction text.
#' @param results Author-supplied scientific interpretation for the Results section.
#' @param discussion Author-supplied discussion text.
#' @param declarations Named declaration overrides.
#' @return A validated `PopgenVCFManuscript` object.
#' @export
new_manuscript <- function(project, publication = NULL, title = project$name,
                           authors = NULL, abstract = NULL, keywords = character(),
                           introduction = NULL, results = NULL, discussion = NULL,
                           declarations = list()) {
  validate_popgenvcf_project(project)
  if (is.null(publication)) publication <- new_publication_bundle(project, title = title)
  validate_publication_bundle(publication)
  defaults <- manuscript_default_declarations(project)
  if (length(declarations)) defaults[names(declarations)] <- declarations

  manuscript <- structure(list(
    schema_version = "1.0",
    project_id = project$project_id,
    project_digest = publication$project_digest,
    publication_digest = digest::digest(publication, algo = "sha256", serialize = TRUE),
    title = manuscript_text(title),
    authors = manuscript_authors(authors),
    abstract = manuscript_text(abstract, "[Author-supplied abstract required.]"),
    keywords = sort(unique(trimws(as.character(keywords[nzchar(trimws(as.character(keywords)))])))),
    introduction = manuscript_text(introduction, "[Author-supplied introduction required.]"),
    methods = manuscript_text(publication$methods),
    results = manuscript_text(results, "[Author-supplied scientific interpretation required. Generated artifact indexes follow.]"),
    discussion = manuscript_text(discussion, "[Author-supplied discussion required.]"),
    captions = data.table::copy(publication$captions),
    artifacts = data.table::copy(publication$artifacts),
    software = data.table::copy(publication$software),
    parameters = data.table::copy(publication$parameters),
    declarations = defaults,
    bibliography = publication$bibliography %||% publication$citations %||% NULL
  ), class = "PopgenVCFManuscript")
  validate_manuscript(manuscript)
  manuscript
}

#' Validate a canonical manuscript specification
#'
#' @param x A `PopgenVCFManuscript` object or generated manuscript directory.
#' @return `TRUE` invisibly, or an error.
#' @export
validate_manuscript <- function(x) {
  if (is.character(x) && length(x) == 1L) {
    required <- c("manuscript.md", "manuscript.rds", "manuscript-manifest.tsv")
    missing <- required[!file.exists(file.path(x, required))]
    if (length(missing)) stop("manuscript directory is missing: ", paste(missing, collapse = ", "), call. = FALSE)
    manifest <- data.table::fread(file.path(x, "manuscript-manifest.tsv"))
    for (i in seq_len(nrow(manifest))) {
      path <- file.path(x, manifest$path[[i]])
      if (!file.exists(path)) stop("manuscript file is missing: ", manifest$path[[i]], call. = FALSE)
      actual <- digest::digest(path, algo = "sha256", file = TRUE)
      if (!identical(actual, manifest$sha256[[i]])) stop("manuscript checksum mismatch: ", manifest$path[[i]], call. = FALSE)
    }
    return(invisible(TRUE))
  }
  if (!inherits(x, "PopgenVCFManuscript")) stop("x must be a PopgenVCFManuscript or directory", call. = FALSE)
  if (!identical(x$schema_version, "1.0")) stop("unsupported manuscript schema version", call. = FALSE)
  if (!is.character(x$title) || length(x$title) != 1L || !nzchar(x$title)) stop("manuscript title is invalid", call. = FALSE)
  if (!is.character(x$project_id) || length(x$project_id) != 1L || !nzchar(x$project_id)) stop("manuscript project identifier is invalid", call. = FALSE)
  manuscript_authors(x$authors)
  required_sections <- c("abstract", "introduction", "methods", "results", "discussion")
  if (any(!vapply(x[required_sections], is.character, logical(1L)))) stop("manuscript sections must be character values", call. = FALSE)
  if (anyDuplicated(x$captions$id)) stop("manuscript caption identifiers must be unique", call. = FALSE)
  invisible(TRUE)
}

manuscript_author_lines <- function(authors) {
  if (!nrow(authors)) return("[Author list required.]")
  vapply(seq_len(nrow(authors)), function(i) {
    suffix <- character()
    if (!is.na(authors$affiliation[[i]]) && nzchar(authors$affiliation[[i]])) suffix <- c(suffix, authors$affiliation[[i]])
    if (!is.na(authors$orcid[[i]]) && nzchar(authors$orcid[[i]])) suffix <- c(suffix, paste0("ORCID: ", authors$orcid[[i]]))
    if (isTRUE(authors$corresponding[[i]])) suffix <- c(suffix, "corresponding author")
    paste0("- ", authors$name[[i]], if (length(suffix)) paste0(" (", paste(suffix, collapse = "; "), ")") else "")
  }, character(1L))
}

