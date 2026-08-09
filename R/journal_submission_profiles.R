journal_profile_roles <- function(x, label) {
  x <- sort(unique(trimws(as.character(x))))
  if (anyNA(x) || any(!nzchar(x))) stop(label, " must contain non-empty role names", call. = FALSE)
  x
}

journal_profile_filenames <- function(x) {
  if (is.null(x) || !length(x)) return(character())
  if (!is.character(x) || is.null(names(x)) || any(!nzchar(names(x))) || any(!nzchar(x))) {
    stop("filenames must be a named character vector", call. = FALSE)
  }
  x <- x[order(names(x))]
  if (anyDuplicated(unname(x))) stop("profile filenames must be unique", call. = FALSE)
  x
}

journal_profile_values <- function(x, label, allow_empty = TRUE) {
  x <- sort(unique(trimws(as.character(x %||% character()))))
  x <- x[!is.na(x) & nzchar(x)]
  if (!allow_empty && !length(x)) stop(label, " must contain at least one value", call. = FALSE)
  x
}

journal_profile_limit <- function(x, label) {
  if (is.null(x) || !length(x) || is.na(x[[1L]])) return(NA_integer_)
  x <- as.integer(x[[1L]])
  if (is.na(x) || x < 0L) stop(label, " must be a non-negative integer or NA", call. = FALSE)
  x
}

journal_profile_requirements <- function(required_sections, optional_sections,
                                         required_declarations, required_companions,
                                         title_max_chars, abstract_max_words,
                                         keyword_min, keyword_max,
                                         highlight_min, highlight_max, highlight_max_chars,
                                         graphical_abstract, figure_max, table_max,
                                         supplementary_max, allowed_figure_extensions,
                                         filename_pattern, overrides) {
  if (!is.list(overrides) || (length(overrides) && (is.null(names(overrides)) || any(!nzchar(names(overrides)))))) {
    stop("overrides must be a named list", call. = FALSE)
  }
  sections <- list(
    required = journal_profile_values(required_sections, "required_sections", FALSE),
    optional = journal_profile_values(optional_sections, "optional_sections")
  )
  if (length(intersect(sections$required, sections$optional))) stop("required and optional sections must not overlap", call. = FALSE)
  limits <- list(
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
  )
  if (!is.na(limits$keyword_min) && !is.na(limits$keyword_max) && limits$keyword_min > limits$keyword_max) stop("keyword_min cannot exceed keyword_max", call. = FALSE)
  if (!is.na(limits$highlight_min) && !is.na(limits$highlight_max) && limits$highlight_min > limits$highlight_max) stop("highlight_min cannot exceed highlight_max", call. = FALSE)
  list(
    sections = sections,
    declarations = journal_profile_values(required_declarations, "required_declarations"),
    companions = journal_profile_values(required_companions, "required_companions"),
    limits = limits,
    graphical_abstract = isTRUE(graphical_abstract),
    allowed_figure_extensions = journal_profile_values(allowed_figure_extensions, "allowed_figure_extensions", FALSE),
    filename_pattern = if (is.null(filename_pattern) || is.na(filename_pattern) || !nzchar(trimws(filename_pattern))) NA_character_ else trimws(filename_pattern),
    overrides = overrides
  )
}

#' Create a deterministic journal submission profile
#'
#' @param id Stable profile identifier.
#' @param required_roles,optional_roles Semantic submission roles.
#' @param filenames Named character vector mapping roles to destination filenames.
#' @param description Human-readable profile description.
#' @param version Version identifier for the encoded requirements.
#' @param journal Journal or generic profile name.
#' @param publisher Publisher name, when applicable.
#' @param status Profile status: `generic`, `verified`, or `deprecated`.
#' @param source_url,source_date Source and verification date for named profiles.
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
new_journal_profile <- function(id = "generic", required_roles = c("manuscript_source"),
                                optional_roles = character(), filenames = NULL,
                                description = "Generic popgenVCF journal submission profile",
                                version = "1.0", journal = description,
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
  id <- trimws(as.character(id)[1L])
  if (is.na(id) || !nzchar(id)) stop("profile id must be non-empty", call. = FALSE)
  required_roles <- journal_profile_roles(required_roles, "required_roles")
  optional_roles <- journal_profile_roles(optional_roles, "optional_roles")
  if (length(intersect(required_roles, optional_roles))) stop("required and optional roles must not overlap", call. = FALSE)
  filenames <- journal_profile_filenames(filenames)
  known <- union(required_roles, optional_roles)
  if (length(setdiff(names(filenames), known))) stop("filename mappings contain unknown roles", call. = FALSE)
  source <- list(url = as.character(source_url)[1L], date = as.character(source_date)[1L])
  if (status == "verified" && (is.na(source$url) || !nzchar(source$url) || is.na(source$date) || !nzchar(source$date))) {
    stop("verified profiles require source_url and source_date", call. = FALSE)
  }
  payload <- list(
    schema_version = "2.0", id = id, version = as.character(version)[1L],
    journal = as.character(journal)[1L], publisher = as.character(publisher)[1L],
    status = status, source = source, description = as.character(description)[1L],
    required_roles = required_roles, optional_roles = optional_roles, filenames = filenames,
    requirements = journal_profile_requirements(
      required_sections, optional_sections, required_declarations, required_companions,
      title_max_chars, abstract_max_words, keyword_min, keyword_max,
      highlight_min, highlight_max, highlight_max_chars, graphical_abstract,
      figure_max, table_max, supplementary_max, allowed_figure_extensions,
      filename_pattern, overrides
    )
  )
  payload$digest <- digest::digest(payload, algo = "sha256", serialize = TRUE)
  profile <- structure(payload, class = "PopgenVCFJournalProfile")
  validate_journal_profile(profile)
  profile
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
  if (!identical(x$schema_version, "2.0")) stop("unsupported journal profile schema version", call. = FALSE)
  journal_profile_roles(x$required_roles, "required_roles")
  journal_profile_roles(x$optional_roles, "optional_roles")
  journal_profile_filenames(x$filenames)
  if (length(intersect(x$required_roles, x$optional_roles))) stop("required and optional roles must not overlap", call. = FALSE)
  journal_profile_requirements(
    x$requirements$sections$required, x$requirements$sections$optional,
    x$requirements$declarations, x$requirements$companions,
    x$requirements$limits$title_max_chars, x$requirements$limits$abstract_max_words,
    x$requirements$limits$keyword_min, x$requirements$limits$keyword_max,
    x$requirements$limits$highlight_min, x$requirements$limits$highlight_max,
    x$requirements$limits$highlight_max_chars, x$requirements$graphical_abstract,
    x$requirements$limits$figure_max, x$requirements$limits$table_max,
    x$requirements$limits$supplementary_max, x$requirements$allowed_figure_extensions,
    x$requirements$filename_pattern, x$requirements$overrides
  )
  payload <- x[setdiff(names(x), "digest")]
  expected <- digest::digest(payload, algo = "sha256", serialize = TRUE)
  if (!identical(expected, x$digest)) stop("journal profile digest mismatch", call. = FALSE)
  invisible(TRUE)
}

#' Return the generic journal profile
#'
#' @return A `PopgenVCFJournalProfile`.
#' @export
generic_journal_profile <- function() {
  new_journal_profile(
    required_roles = "manuscript_source",
    optional_roles = c("manuscript_docx", "manuscript_html", "jats_xml", "bibliography", "citation_style", "figure", "table", "supplementary", "provenance"),
    filenames = c(manuscript_source = "manuscript.md", manuscript_docx = "manuscript.docx",
                  manuscript_html = "manuscript.html", jats_xml = "manuscript.xml",
                  bibliography = "references.bib", citation_style = "citation-style.csl"),
    keyword_min = 3L, keyword_max = 10L
  )
}

#' Return a built-in generic journal profile
#'
#' @param name One of `research-article`, `short-communication`, or `data-note`.
#' @return A `PopgenVCFJournalProfile`.
#' @export
journal_profile <- function(name = c("research-article", "short-communication", "data-note")) {
  name <- match.arg(name)
  if (name == "research-article") return(generic_journal_profile())
  if (name == "short-communication") {
    return(new_journal_profile(
      id = "generic-short-communication", journal = "Generic short communication",
      required_sections = c("abstract", "methods", "results", "discussion"),
      optional_sections = "introduction", abstract_max_words = 250L,
      keyword_min = 3L, keyword_max = 8L, figure_max = 4L, table_max = 4L
    ))
  }
  new_journal_profile(
    id = "generic-data-note", journal = "Generic data note",
    required_sections = c("abstract", "methods", "results"),
    optional_sections = c("introduction", "discussion"),
    required_declarations = c("data_availability", "software_availability", "reproducibility"),
    keyword_min = 3L, keyword_max = 10L
  )
}

#' Apply and validate a journal profile against a submission plan
#'
#' @param plan A data frame with `role` and `destination` columns.
#' @param profile A validated journal profile.
#' @return A deterministically ordered data table with profile-aware destinations.
#' @export
apply_journal_profile <- function(plan, profile = generic_journal_profile()) {
  validate_journal_profile(profile)
  plan <- data.table::as.data.table(plan)
  if (!all(c("role", "destination") %in% names(plan))) stop("plan must contain role and destination columns", call. = FALSE)
  roles <- as.character(plan$role)
  missing <- setdiff(profile$required_roles, roles)
  if (length(missing)) stop("submission plan is missing required roles: ", paste(missing, collapse = ", "), call. = FALSE)
  allowed <- union(profile$required_roles, profile$optional_roles)
  unknown <- setdiff(unique(roles), allowed)
  if (length(unknown)) stop("submission plan contains roles not allowed by profile: ", paste(sort(unknown), collapse = ", "), call. = FALSE)
  out <- data.table::copy(plan)
  for (role in intersect(names(profile$filenames), out$role)) {
    idx <- which(out$role == role)
    if (length(idx) == 1L) out$destination[[idx]] <- profile$filenames[[role]]
  }
  if (anyDuplicated(out$destination)) stop("profile application creates duplicate destinations", call. = FALSE)
  data.table::setorderv(out, c("role", "destination"))
  attr(out, "journal_profile_id") <- profile$id
  attr(out, "journal_profile_digest") <- profile$digest
  out
}

journal_submission_row <- function(requirement, ok, observed, expected, message) {
  data.table::data.table(requirement = requirement, status = if (ok) "pass" else "fail",
                         observed = as.character(observed), expected = as.character(expected), message = message)
}

journal_submission_word_count <- function(x) {
  x <- trimws(as.character(x %||% ""))
  if (!nzchar(x)) return(0L)
  length(strsplit(x, "[[:space:]]+", perl = TRUE)[[1L]])
}

#' Validate manuscript completeness against a journal profile
#'
#' @param profile A validated `PopgenVCFJournalProfile`.
#' @param manuscript A validated `PopgenVCFManuscript`.
#' @param companions Optional `PopgenVCFSubmissionCompanions`.
#' @param graphical_abstract Optional `PopgenVCFGraphicalAbstract`.
#' @param strict Whether failed requirements raise an error.
#' @return A deterministic `PopgenVCFJournalSubmissionReport` data table.
#' @export
validate_journal_submission <- function(profile, manuscript, companions = NULL,
                                        graphical_abstract = NULL, strict = FALSE) {
  validate_journal_profile(profile)
  validate_manuscript(manuscript)
  req <- profile$requirements
  rows <- list()
  add <- function(name, ok, observed, expected, message) {
    rows[[length(rows) + 1L]] <<- journal_submission_row(name, ok, observed, expected, message)
  }
  placeholder <- "^\\[Author-supplied|^\\[Author list required|^\\[Keywords required"
  for (section in req$sections$required) {
    value <- manuscript[[section]] %||% ""
    ok <- is.character(value) && length(value) == 1L && nzchar(trimws(value)) && !grepl(placeholder, value)
    add(paste0("section:", section), ok, if (ok) "present" else "missing", "required", paste("Required section", section))
  }
  limits <- req$limits
  add("title:max_chars", is.na(limits$title_max_chars) || nchar(manuscript$title) <= limits$title_max_chars,
      nchar(manuscript$title), limits$title_max_chars, "Maximum manuscript title length")
  abstract_words <- journal_submission_word_count(manuscript$abstract)
  add("abstract:max_words", is.na(limits$abstract_max_words) || abstract_words <= limits$abstract_max_words,
      abstract_words, limits$abstract_max_words, "Maximum abstract word count")
  keyword_count <- length(manuscript$keywords)
  add("keywords:min", keyword_count >= limits$keyword_min, keyword_count, limits$keyword_min, "Minimum keyword count")
  add("keywords:max", is.na(limits$keyword_max) || keyword_count <= limits$keyword_max,
      keyword_count, limits$keyword_max, "Maximum keyword count")
  for (declaration in req$declarations) {
    value <- manuscript$declarations[[declaration]] %||% ""
    ok <- nzchar(trimws(value)) && !grepl("must provide", value, ignore.case = TRUE)
    add(paste0("declaration:", declaration), ok, if (ok) "present" else "incomplete", "required", paste("Required declaration", declaration))
  }
  categories <- if (nrow(manuscript$artifacts) && "category" %in% names(manuscript$artifacts)) as.character(manuscript$artifacts$category) else character()
  category_limits <- c(figure = "figure_max", table = "table_max", supplementary = "supplementary_max")
  for (category in names(category_limits)) {
    count <- sum(categories == category)
    limit <- limits[[category_limits[[category]]]]
    add(paste0(category, ":max"), is.na(limit) || count <= limit, count, limit, paste("Maximum", category, "count"))
  }
  for (companion in req$companions) {
    present <- !is.null(companions) && companion %in% names(companions) && length(companions[[companion]]) > 0L
    add(paste0("companion:", companion), present, if (present) "present" else "missing", "required", paste("Required companion", companion))
  }
  highlights <- companions$highlights %||% character()
  if (limits$highlight_min > 0L || !is.na(limits$highlight_max) || !is.na(limits$highlight_max_chars)) {
    count <- length(highlights)
    add("highlights:min", count >= limits$highlight_min, count, limits$highlight_min, "Minimum highlight count")
    add("highlights:max", is.na(limits$highlight_max) || count <= limits$highlight_max, count, limits$highlight_max, "Maximum highlight count")
    if (!is.na(limits$highlight_max_chars)) {
      longest <- if (count) max(nchar(highlights)) else 0L
      add("highlights:max_chars", longest <= limits$highlight_max_chars, longest, limits$highlight_max_chars, "Maximum highlight length")
    }
  }
  add("graphical_abstract", !req$graphical_abstract || !is.null(graphical_abstract),
      if (is.null(graphical_abstract)) "absent" else "present", if (req$graphical_abstract) "required" else "optional",
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
  req <- profile$requirements
  c(
    paste0("# ", profile$journal), "",
    paste0("- Profile ID: `", profile$id, "`"),
    paste0("- Digest: `", profile$digest, "`"),
    paste0("- Version: ", profile$version),
    paste0("- Status: ", profile$status),
    paste0("- Publisher: ", ifelse(is.na(profile$publisher), "not specified", profile$publisher)),
    paste0("- Source: ", ifelse(is.na(profile$source$url), "not specified", profile$source$url)),
    paste0("- Source date: ", ifelse(is.na(profile$source$date), "not specified", profile$source$date)), "",
    "## Required sections", "", paste0("- ", req$sections$required), "",
    "## Required declarations", "", paste0("- ", req$declarations), "",
    "## Required companions", "", if (length(req$companions)) paste0("- ", req$companions) else "None.", "",
    "## Submission roles", "", paste0("- Required: ", paste(profile$required_roles, collapse = ", ")),
    paste0("- Optional: ", paste(profile$optional_roles, collapse = ", "))
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
  manifest <- data.table::data.table(path = basename(files), size_bytes = file.info(files)$size,
                                     sha256 = vapply(files, digest::digest, character(1L), algo = "sha256", file = TRUE))
  data.table::fwrite(manifest, file.path(out, "journal-profile-manifest.tsv"), sep = "\t")
  validate_journal_profile(out)
  invisible(normalizePath(out, winslash = "/", mustWork = TRUE))
}
