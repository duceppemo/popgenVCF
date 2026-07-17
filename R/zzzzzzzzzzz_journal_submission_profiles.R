journal_profile_character <- function(x, field, allow_empty = TRUE) {
  x <- trimws(as.character(x %||% character()))
  x <- x[!is.na(x) & nzchar(x)]
  x <- sort(unique(x))
  if (!allow_empty && !length(x)) stop(field, " must contain at least one value", call. = FALSE)
  x
}

journal_profile_limit <- function(x, field) {
  if (is.null(x) || !length(x) || is.na(x[[1L]])) return(NA_integer_)
  x <- as.integer(x[[1L]])
  if (length(x) != 1L || is.na(x) || x < 0L) stop(field, " must be a non-negative integer or NA", call. = FALSE)
  x
}

#' Create a deterministic journal submission profile
#'
#' @param key Stable profile key.
#' @param version Version identifier for the encoded requirements.
#' @param journal Journal or generic profile name.
#' @param publisher Publisher name, when applicable.
#' @param status Profile status: `generic`, `verified`, or `deprecated`.
#' @param source_url Source URL for named profiles.
#' @param source_date Date on which named requirements were verified.
#' @param required_sections,optional_sections Manuscript section requirements.
#' @param required_declarations Required declaration names.
#' @param required_companions Required companion document names.
#' @param title_max_chars,abstract_max_words,keyword_min,keyword_max Numeric limits.
#' @param highlight_min,highlight_max,highlight_max_chars Highlight constraints.
#' @param graphical_abstract Whether a graphical abstract is required.
#' @param figure_max,table_max,supplementary_max Maximum artifact counts.
#' @param allowed_figure_extensions Allowed figure filename extensions.
#' @param filename_pattern Optional regular expression for submission filenames.
#' @param overrides Named author-supplied requirement overrides.
#' @return A validated `PopgenVCFJournalProfile`.
#' @export
new_journal_profile <- function(key, version = "1.0", journal,
                                publisher = NA_character_,
                                status = c("generic", "verified", "deprecated"),
                                source_url = NA_character_, source_date = NA_character_,
                                required_sections = c("abstract", "introduction", "methods", "results", "discussion"),
                                optional_sections = character(),
                                required_declarations = c("data_availability", "software_availability", "reproducibility",
                                                          "funding", "author_contributions", "competing_interests"),
                                required_companions = character(),
                                title_max_chars = NA_integer_, abstract_max_words = NA_integer_,
                                keyword_min = 0L, keyword_max = NA_integer_,
                                highlight_min = 0L, highlight_max = NA_integer_, highlight_max_chars = NA_integer_,
                                graphical_abstract = FALSE,
                                figure_max = NA_integer_, table_max = NA_integer_, supplementary_max = NA_integer_,
                                allowed_figure_extensions = c("pdf", "png", "tif", "tiff", "svg"),
                                filename_pattern = NA_character_, overrides = list()) {
  status <- match.arg(status)
  scalar <- function(x, field, required = TRUE) {
    x <- trimws(as.character(x %||% NA_character_)[[1L]])
    if (required && (is.na(x) || !nzchar(x))) stop(field, " must be a non-empty scalar", call. = FALSE)
    x
  }
  if (!is.list(overrides) || (length(overrides) && (is.null(names(overrides)) || any(!nzchar(names(overrides)))))) {
    stop("overrides must be a named list", call. = FALSE)
  }
  payload <- list(
    schema_version = "1.0",
    key = scalar(key, "key"), version = scalar(version, "version"), journal = scalar(journal, "journal"),
    publisher = scalar(publisher, "publisher", FALSE), status = status,
    source = list(url = scalar(source_url, "source_url", FALSE), date = scalar(source_date, "source_date", FALSE)),
    sections = list(required = journal_profile_character(required_sections, "required_sections", FALSE),
                    optional = journal_profile_character(optional_sections, "optional_sections")),
    declarations = journal_profile_character(required_declarations, "required_declarations"),
    companions = journal_profile_character(required_companions, "required_companions"),
    limits = list(
      title_max_chars = journal_profile_limit(title_max_chars, "title_max_chars"),
      abstract_max_words = journal_profile_limit(abstract_max_words, "abstract_max_words"),
      keyword_min = journal_profile_limit(keyword_min, "keyword_min"),
      keyword_max = journal_profile_limit(keyword_max, "keyword_max"),
      highlight_min = journal_profile_limit(highlight_min, "highlight_min"),
      highlight_max = journal_profile_limit(highlight_max, "highlight_max"),
      highlight_max_chars = journal_profile_limit(highlight_max_chars, "highlight_max_chars"),
      figure_max = journal_profile_limit(figure_max, "figure_max"),
      table_max = journal_profile_limit(table_max, "table_max"),
      supplementary_max = journal_profile_limit(supplementary_max, "supplementary_max")
    ),
    graphical_abstract = isTRUE(graphical_abstract),
    allowed_figure_extensions = journal_profile_character(allowed_figure_extensions, "allowed_figure_extensions", FALSE),
    filename_pattern = scalar(filename_pattern, "filename_pattern", FALSE),
    overrides = overrides
  )
  if (status == "verified" && (is.na(payload$source$url) || is.na(payload$source$date))) {
    stop("verified profiles require source_url and source_date", call. = FALSE)
  }
  payload$id <- paste0("journal-profile:", digest::digest(payload, algo = "sha256", serialize = TRUE))
  class(payload) <- c("PopgenVCFJournalProfile", "list")
  validate_journal_profile(payload)
  payload
}

#' Return built-in generic journal profiles
#'
#' @param name One of `research-article`, `short-communication`, or `data-note`.
#' @return A `PopgenVCFJournalProfile`.
#' @export
journal_profile <- function(name = c("research-article", "short-communication", "data-note")) {
  name <- match.arg(name)
  switch(name,
    `research-article` = new_journal_profile(
      key = "generic-research-article", journal = "Generic research article",
      keyword_min = 3L, keyword_max = 10L
    ),
    `short-communication` = new_journal_profile(
      key = "generic-short-communication", journal = "Generic short communication",
      required_sections = c("abstract", "methods", "results", "discussion"),
      optional_sections = "introduction", abstract_max_words = 250L,
      keyword_min = 3L, keyword_max = 8L, figure_max = 4L, table_max = 4L
    ),
    `data-note` = new_journal_profile(
      key = "generic-data-note", journal = "Generic data note",
      required_sections = c("abstract", "methods", "results"),
      optional_sections = c("introduction", "discussion"),
      required_declarations = c("data_availability", "software_availability", "reproducibility"),
      keyword_min = 3L, keyword_max = 10L
    )
  )
}

#' Validate a journal submission profile
#'
#' @param x A `PopgenVCFJournalProfile` or written profile directory.
#' @return `TRUE` invisibly.
#' @export
validate_journal_profile <- function(x) {
  if (is.character(x) && length(x) == 1L) {
    required <- c("journal-profile.json", "journal-profile.md", "journal-profile-manifest.tsv")
    missing <- required[!file.exists(file.path(x, required))]
    if (length(missing)) stop("journal profile directory is missing: ", paste(missing, collapse = ", "), call. = FALSE)
    manifest <- data.table::fread(file.path(x, "journal-profile-manifest.tsv"))
    for (i in seq_len(nrow(manifest))) {
      path <- file.path(x, manifest$path[[i]])
      if (!file.exists(path)) stop("journal profile file is missing: ", manifest$path[[i]], call. = FALSE)
      if (!identical(digest::digest(path, algo = "sha256", file = TRUE), manifest$sha256[[i]])) {
        stop("journal profile checksum mismatch: ", manifest$path[[i]], call. = FALSE)
      }
    }
    return(invisible(TRUE))
  }
  if (!inherits(x, "PopgenVCFJournalProfile")) stop("x must be a PopgenVCFJournalProfile or directory", call. = FALSE)
  if (!identical(x$schema_version, "1.0")) stop("unsupported journal profile schema version", call. = FALSE)
  required <- c("key", "version", "journal", "status", "source", "sections", "declarations", "companions",
                "limits", "graphical_abstract", "allowed_figure_extensions", "filename_pattern", "overrides", "id")
  if (!all(required %in% names(x))) stop("malformed journal profile", call. = FALSE)
  if (anyDuplicated(c(x$sections$required, x$sections$optional))) stop("section requirements must be unique", call. = FALSE)
  limits <- unlist(x$limits, use.names = TRUE)
  if (any(!is.na(limits) & limits < 0L)) stop("journal profile limits must be non-negative", call. = FALSE)
  pairs <- list(c("keyword_min", "keyword_max"), c("highlight_min", "highlight_max"))
  for (pair in pairs) if (!is.na(limits[[pair[[2L]]]]) && limits[[pair[[1L]]]] > limits[[pair[[2L]]]]) {
    stop(pair[[1L]], " cannot exceed ", pair[[2L]], call. = FALSE)
  }
  invisible(TRUE)
}

journal_submission_row <- function(requirement, status, observed, expected, message) {
  data.table::data.table(requirement = requirement, status = status, observed = as.character(observed),
                         expected = as.character(expected), message = message)
}

journal_word_count <- function(x) {
  x <- trimws(as.character(x %||% ""))
  if (!nzchar(x)) return(0L)
  length(strsplit(x, "[[:space:]]+", perl = TRUE)[[1L]])
}

#' Validate a manuscript and submission companions against a profile
#'
#' @param profile A validated `PopgenVCFJournalProfile`.
#' @param manuscript A validated `PopgenVCFManuscript`.
#' @param companions Optional `PopgenVCFSubmissionCompanions` object.
#' @param graphical_abstract Optional `PopgenVCFGraphicalAbstract` object.
#' @param strict Whether failed requirements should raise an error.
#' @return A deterministic `data.table` completeness report.
#' @export
validate_journal_submission <- function(profile, manuscript, companions = NULL,
                                        graphical_abstract = NULL, strict = FALSE) {
  validate_journal_profile(profile)
  validate_manuscript(manuscript)
  rows <- list()
  add <- function(requirement, ok, observed, expected, message) {
    rows[[length(rows) + 1L]] <<- journal_submission_row(requirement, if (ok) "pass" else "fail", observed, expected, message)
  }
  placeholders <- "^\\[Author-supplied|^\\[Author list required|^\\[Keywords required"
  for (section in profile$sections$required) {
    value <- manuscript[[section]] %||% ""
    ok <- is.character(value) && length(value) == 1L && nzchar(trimws(value)) && !grepl(placeholders, value)
    add(paste0("section:", section), ok, if (ok) "present" else "missing", "required", paste("Required section", section))
  }
  add("title:max_chars", is.na(profile$limits$title_max_chars) || nchar(manuscript$title) <= profile$limits$title_max_chars,
      nchar(manuscript$title), profile$limits$title_max_chars, "Maximum manuscript title length")
  abstract_words <- journal_word_count(manuscript$abstract)
  add("abstract:max_words", is.na(profile$limits$abstract_max_words) || abstract_words <= profile$limits$abstract_max_words,
      abstract_words, profile$limits$abstract_max_words, "Maximum abstract word count")
  keyword_count <- length(manuscript$keywords)
  add("keywords:min", keyword_count >= profile$limits$keyword_min, keyword_count, profile$limits$keyword_min, "Minimum keyword count")
  add("keywords:max", is.na(profile$limits$keyword_max) || keyword_count <= profile$limits$keyword_max,
      keyword_count, profile$limits$keyword_max, "Maximum keyword count")
  for (declaration in profile$declarations) {
    value <- manuscript$declarations[[declaration]] %||% ""
    ok <- nzchar(trimws(value)) && !grepl("must provide", value, ignore.case = TRUE)
    add(paste0("declaration:", declaration), ok, if (ok) "present" else "incomplete", "required", paste("Required declaration", declaration))
  }
  categories <- if (nrow(manuscript$artifacts) && "category" %in% names(manuscript$artifacts)) manuscript$artifacts$category else character()
  for (item in c(figure = "figure_max", table = "table_max", supplementary = "supplementary_max")) {
    count <- sum(categories == names(item))
    limit <- profile$limits[[unname(item)]]
    add(paste0(names(item), ":max"), is.na(limit) || count <= limit, count, limit, paste("Maximum", names(item), "count"))
  }
  if ("highlights" %in% profile$companions || profile$limits$highlight_min > 0L || !is.na(profile$limits$highlight_max)) {
    highlights <- companions$highlights %||% character()
    count <- length(highlights)
    add("highlights:min", count >= profile$limits$highlight_min, count, profile$limits$highlight_min, "Minimum highlight count")
    add("highlights:max", is.na(profile$limits$highlight_max) || count <= profile$limits$highlight_max,
        count, profile$limits$highlight_max, "Maximum highlight count")
    if (!is.na(profile$limits$highlight_max_chars)) {
      longest <- if (count) max(nchar(highlights)) else 0L
      add("highlights:max_chars", longest <= profile$limits$highlight_max_chars, longest,
          profile$limits$highlight_max_chars, "Maximum highlight length")
    }
  }
  for (companion in profile$companions) {
    present <- !is.null(companions) && companion %in% names(companions) && length(companions[[companion]]) > 0L
    add(paste0("companion:", companion), present, if (present) "present" else "missing", "required", paste("Required companion", companion))
  }
  add("graphical_abstract", !profile$graphical_abstract || !is.null(graphical_abstract),
      if (is.null(graphical_abstract)) "absent" else "present", if (profile$graphical_abstract) "required" else "optional",
      "Graphical abstract requirement")
  report <- data.table::rbindlist(rows, use.names = TRUE)
  data.table::setorderv(report, "requirement")
  class(report) <- c("PopgenVCFJournalSubmissionReport", class(report))
  if (isTRUE(strict) && any(report$status == "fail")) {
    stop("Journal submission profile requirements failed: ", paste(report$requirement[report$status == "fail"], collapse = ", "), call. = FALSE)
  }
  report
}

#' Render a journal profile as Markdown
#'
#' @param profile A validated `PopgenVCFJournalProfile`.
#' @return Character vector containing Markdown.
#' @export
render_journal_profile <- function(profile) {
  validate_journal_profile(profile)
  show_limit <- function(name) if (is.na(profile$limits[[name]])) "not specified" else as.character(profile$limits[[name]])
  c(
    paste0("# ", profile$journal), "",
    paste0("- Profile ID: `", profile$id, "`"),
    paste0("- Key: `", profile$key, "`"),
    paste0("- Version: ", profile$version),
    paste0("- Status: ", profile$status),
    paste0("- Publisher: ", ifelse(is.na(profile$publisher), "not specified", profile$publisher)),
    paste0("- Source: ", ifelse(is.na(profile$source$url), "not specified", profile$source$url)),
    paste0("- Source date: ", ifelse(is.na(profile$source$date), "not specified", profile$source$date)), "",
    "## Required sections", "", paste0("- ", profile$sections$required), "",
    "## Required declarations", "", paste0("- ", profile$declarations), "",
    "## Required companions", "", if (length(profile$companions)) paste0("- ", profile$companions) else "None.", "",
    "## Limits", "",
    paste0("- Title characters: ", show_limit("title_max_chars")),
    paste0("- Abstract words: ", show_limit("abstract_max_words")),
    paste0("- Keywords: ", show_limit("keyword_min"), " to ", show_limit("keyword_max")),
    paste0("- Highlights: ", show_limit("highlight_min"), " to ", show_limit("highlight_max")),
    paste0("- Graphical abstract: ", ifelse(profile$graphical_abstract, "required", "optional"))
  )
}

#' Write a deterministic journal profile bundle
#'
#' @param profile A validated `PopgenVCFJournalProfile`.
#' @param directory Parent output directory.
#' @param overwrite Whether an existing output directory may be replaced.
#' @return Normalized output directory invisibly.
#' @export
write_journal_profile <- function(profile, directory, overwrite = FALSE) {
  validate_journal_profile(profile)
  out <- file.path(directory, "journal-profile")
  if (dir.exists(out)) {
    if (!isTRUE(overwrite)) stop("journal profile directory already exists", call. = FALSE)
    unlink(out, recursive = TRUE, force = TRUE)
  }
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(unclass(profile), file.path(out, "journal-profile.json"), auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null")
  writeLines(render_journal_profile(profile), file.path(out, "journal-profile.md"), useBytes = TRUE)
  files <- sort(list.files(out, full.names = TRUE))
  manifest <- data.table::data.table(
    path = basename(files), size_bytes = file.info(files)$size,
    sha256 = vapply(files, digest::digest, character(1L), algo = "sha256", file = TRUE)
  )
  data.table::fwrite(manifest, file.path(out, "journal-profile-manifest.tsv"), sep = "\t")
  validate_journal_profile(out)
  invisible(normalizePath(out, winslash = "/", mustWork = TRUE))
}
