jats_xml_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  gsub("'", "&apos;", x, fixed = TRUE)
}

jats_id <- function(prefix, value) {
  value <- tolower(gsub("[^A-Za-z0-9_.-]+", "-", as.character(value)))
  value <- gsub("-+", "-", value)
  value <- gsub("^-|-$", "", value)
  paste0(prefix, "-", value)
}

jats_paragraph <- function(x) paste0("<p>", jats_xml_escape(x), "</p>")

jats_section <- function(id, title, text) {
  c(paste0('<sec id="', jats_id("sec", id), '">'),
    paste0("<title>", jats_xml_escape(title), "</title>"),
    jats_paragraph(text), "</sec>")
}

jats_contributors <- function(authors) {
  if (!nrow(authors)) return("<contrib-group/>")
  affiliations <- unique(authors$affiliation[!is.na(authors$affiliation) & nzchar(authors$affiliation)])
  affiliation_ids <- setNames(paste0("aff", seq_along(affiliations)), affiliations)
  contributors <- unlist(lapply(seq_len(nrow(authors)), function(i) {
    name <- authors$name[[i]]
    parts <- strsplit(name, "[[:space:]]+")[[1L]]
    surname <- tail(parts, 1L)
    given <- if (length(parts) > 1L) paste(head(parts, -1L), collapse = " ") else ""
    lines <- c('<contrib contrib-type="author">', "<name>",
      paste0("<surname>", jats_xml_escape(surname), "</surname>"),
      paste0("<given-names>", jats_xml_escape(given), "</given-names>"), "</name>")
    affiliation <- authors$affiliation[[i]]
    if (!is.na(affiliation) && nzchar(affiliation)) {
      lines <- c(lines, paste0('<xref ref-type="aff" rid="', affiliation_ids[[affiliation]], '"/>'))
    }
    orcid <- authors$orcid[[i]]
    if (!is.na(orcid) && nzchar(orcid)) {
      lines <- c(lines, paste0('<contrib-id contrib-id-type="orcid">', jats_xml_escape(orcid), "</contrib-id>"))
    }
    email <- authors$email[[i]]
    if (!is.na(email) && nzchar(email)) lines <- c(lines, paste0("<email>", jats_xml_escape(email), "</email>"))
    if (isTRUE(authors$corresponding[[i]])) lines <- c(lines, '<xref ref-type="corresp" rid="cor1"/>')
    c(lines, "</contrib>")
  }), use.names = FALSE)
  affiliation_lines <- unlist(Map(function(value, id) {
    c(paste0('<aff id="', id, '">'), paste0("<institution>", jats_xml_escape(value), "</institution>"), "</aff>")
  }, affiliations, unname(affiliation_ids)), use.names = FALSE)
  c("<contrib-group>", contributors, "</contrib-group>", affiliation_lines)
}

jats_artifacts <- function(manuscript, category) {
  refs <- manuscript_cross_reference_table(manuscript)
  refs <- refs[refs[["category"]] == category]
  if (!nrow(refs)) return(character())
  tag <- if (identical(category, "figure")) "fig" else if (identical(category, "table")) "table-wrap" else "supplementary-material"
  unlist(lapply(seq_len(nrow(refs)), function(i) {
    row <- refs[i]
    c(paste0('<', tag, ' id="', jats_id(category, row$id), '">'),
      paste0("<label>", jats_xml_escape(row$label), "</label>"),
      paste0("<caption>", jats_paragraph(row$caption), "</caption>"),
      paste0('<media xlink:href="', jats_xml_escape(row$path), '"/>'),
      paste0("</", tag, ">"))
  }), use.names = FALSE)
}

jats_references <- function(manuscript) {
  keys <- manuscript_citation_keys(manuscript)
  if (!length(keys)) return("<ref-list/>")
  c("<ref-list>", paste0('<ref id="', jats_id("ref", keys), '"><label>',
    jats_xml_escape(keys), '</label><mixed-citation>', jats_xml_escape(keys),
    "</mixed-citation></ref>"), "</ref-list>")
}

#' Render deterministic JATS XML
#'
#' @param manuscript A validated `PopgenVCFManuscript`.
#' @return A character vector containing JATS XML.
#' @export
render_manuscript_jats <- function(manuscript) {
  validate_manuscript(manuscript)
  declarations <- manuscript$declarations
  keywords <- if (length(manuscript$keywords)) paste0("<kwd>", jats_xml_escape(manuscript$keywords), "</kwd>") else character()
  c('<?xml version="1.0" encoding="UTF-8"?>',
    '<article xmlns:xlink="http://www.w3.org/1999/xlink" article-type="research-article">',
    "<front><article-meta>", paste0("<article-id pub-id-type=\"publisher-id\">", jats_xml_escape(manuscript$project_id), "</article-id>"),
    paste0("<title-group><article-title>", jats_xml_escape(manuscript$title), "</article-title></title-group>"),
    jats_contributors(manuscript$authors),
    if (any(manuscript$authors$corresponding)) '<author-notes><corresp id="cor1">Corresponding author</corresp></author-notes>' else character(),
    paste0("<abstract>", jats_paragraph(manuscript$abstract), "</abstract>"),
    c("<kwd-group>", keywords, "</kwd-group>"), "</article-meta></front>",
    "<body>",
    jats_section("introduction", "Introduction", manuscript$introduction),
    jats_section("methods", "Methods", manuscript$methods),
    jats_section("results", "Results", manuscript$results),
    jats_artifacts(manuscript, "figure"), jats_artifacts(manuscript, "table"),
    jats_section("discussion", "Discussion", manuscript$discussion),
    jats_section("data-availability", "Data availability", declarations$data_availability),
    jats_section("software-availability", "Software availability", declarations$software_availability),
    jats_section("reproducibility", "Reproducibility statement", declarations$reproducibility),
    jats_artifacts(manuscript, "supplementary"), "</body>",
    "<back>", jats_section("funding", "Funding", declarations$funding),
    jats_section("author-contributions", "Author contributions", declarations$author_contributions),
    jats_section("competing-interests", "Competing interests", declarations$competing_interests),
    jats_references(manuscript), "</back>", "</article>")
}

#' Validate generated JATS XML output
#'
#' @param x JATS text, a JATS file, or a `PopgenVCFJATSRecord`.
#' @return `TRUE` invisibly, or an error.
#' @export
validate_manuscript_jats <- function(x) {
  if (inherits(x, "PopgenVCFJATSRecord")) {
    if (!file.exists(x$path)) stop("JATS output is missing", call. = FALSE)
    actual <- digest::digest(x$path, algo = "sha256", file = TRUE)
    if (!identical(actual, x$sha256)) stop("JATS checksum mismatch", call. = FALSE)
    x <- x$path
  }
  text <- if (is.character(x) && length(x) == 1L && file.exists(x)) readLines(x, warn = FALSE, encoding = "UTF-8") else as.character(x)
  joined <- paste(text, collapse = "\n")
  if (!grepl("^<\\?xml", joined) || !grepl("<article[ >]", joined) || !grepl("</article>[[:space:]]*$", joined)) {
    stop("JATS XML article structure is invalid", call. = FALSE)
  }
  opens <- gregexpr("<([A-Za-z][A-Za-z0-9:-]*)([[:space:]][^>]*)?>", joined, perl = TRUE)[[1L]]
  closes <- gregexpr("</([A-Za-z][A-Za-z0-9:-]*)>", joined, perl = TRUE)[[1L]]
  if (length(opens[opens > 0L]) < length(closes[closes > 0L])) stop("JATS XML tags are unbalanced", call. = FALSE)
  invisible(TRUE)
}

#' Write deterministic JATS XML
#'
#' @param manuscript A manuscript object or written manuscript directory.
#' @param directory Output directory when `manuscript` is an object.
#' @param overwrite Permit replacing existing JATS output.
#' @return A validated `PopgenVCFJATSRecord`.
#' @export
write_manuscript_jats <- function(manuscript, directory = NULL, overwrite = FALSE) {
  if (is.character(manuscript) && length(manuscript) == 1L) {
    validate_manuscript(manuscript)
    root <- manuscript
    manuscript <- readRDS(file.path(root, "manuscript.rds"))
  } else {
    validate_manuscript(manuscript)
    if (is.null(directory)) stop("directory is required for a manuscript object", call. = FALSE)
    root <- directory
    dir.create(root, recursive = TRUE, showWarnings = FALSE)
  }
  target_dir <- file.path(root, "jats")
  target <- file.path(target_dir, "manuscript.xml")
  if (file.exists(target) && !isTRUE(overwrite)) stop("JATS output already exists", call. = FALSE)
  dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(render_manuscript_jats(manuscript), target, useBytes = TRUE)
  validate_manuscript_jats(target)
  record <- structure(list(schema_version = "1.0", profile = "jats-articleauthoring-1.3",
    project_id = manuscript$project_id, path = normalizePath(target, winslash = "/"),
    sha256 = digest::digest(target, algo = "sha256", file = TRUE)), class = "PopgenVCFJATSRecord")
  jsonlite::write_json(unclass(record), file.path(target_dir, "jats-record.json"), pretty = TRUE, auto_unbox = TRUE)
  data.table::fwrite(data.table::data.table(path = "manuscript.xml", sha256 = record$sha256),
    file.path(target_dir, "jats-manifest.tsv"), sep = "\t")
  validate_manuscript_jats(record)
  record
}
