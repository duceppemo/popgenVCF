journal_profile_roles <- function(x, label) {
  x <- sort(unique(trimws(as.character(x))))
  if (anyNA(x) || any(!nzchar(x))) stop(label, " must contain non-empty role names", call. = FALSE)
  x
}

journal_profile_filenames <- function(x) {
  if (is.null(x)) return(character())
  if (!is.character(x) || is.null(names(x)) || any(!nzchar(names(x))) || any(!nzchar(x))) {
    stop("filenames must be a named character vector", call. = FALSE)
  }
  x <- x[order(names(x))]
  if (anyDuplicated(unname(x))) stop("profile filenames must be unique", call. = FALSE)
  x
}

#' Create a deterministic journal submission profile
#'
#' @param id Stable profile identifier.
#' @param required_roles,optional_roles Semantic submission roles.
#' @param filenames Named character vector mapping roles to destination filenames.
#' @param description Human-readable profile description.
#' @return A validated `PopgenVCFJournalProfile`.
#' @export
new_journal_profile <- function(id = "generic", required_roles = c("manuscript_source"),
                                optional_roles = character(), filenames = NULL,
                                description = "Generic popgenVCF journal submission profile") {
  id <- trimws(as.character(id)[1L])
  if (is.na(id) || !nzchar(id)) stop("profile id must be non-empty", call. = FALSE)
  required_roles <- journal_profile_roles(required_roles, "required_roles")
  optional_roles <- journal_profile_roles(optional_roles, "optional_roles")
  if (length(intersect(required_roles, optional_roles))) stop("required and optional roles must not overlap", call. = FALSE)
  filenames <- journal_profile_filenames(filenames)
  known <- union(required_roles, optional_roles)
  if (length(setdiff(names(filenames), known))) stop("filename mappings contain unknown roles", call. = FALSE)
  payload <- list(schema_version = "1.0", id = id, description = as.character(description)[1L],
                  required_roles = required_roles, optional_roles = optional_roles,
                  filenames = filenames)
  payload$digest <- digest::digest(payload, algo = "sha256", serialize = TRUE)
  profile <- structure(payload, class = "PopgenVCFJournalProfile")
  validate_journal_profile(profile)
  profile
}

#' Validate a journal submission profile
#'
#' @param x A `PopgenVCFJournalProfile`.
#' @return `TRUE` invisibly.
#' @export
validate_journal_profile <- function(x) {
  if (!inherits(x, "PopgenVCFJournalProfile")) stop("x must be a PopgenVCFJournalProfile", call. = FALSE)
  if (!identical(x$schema_version, "1.0")) stop("unsupported journal profile schema version", call. = FALSE)
  journal_profile_roles(x$required_roles, "required_roles")
  journal_profile_roles(x$optional_roles, "optional_roles")
  journal_profile_filenames(x$filenames)
  if (length(intersect(x$required_roles, x$optional_roles))) stop("required and optional roles must not overlap", call. = FALSE)
  payload <- x[c("schema_version", "id", "description", "required_roles", "optional_roles", "filenames")]
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
    required_roles = c("manuscript_source"),
    optional_roles = c("manuscript_docx", "manuscript_html", "jats_xml", "bibliography", "citation_style", "figure", "table", "supplementary", "provenance"),
    filenames = c(manuscript_source = "manuscript.md", manuscript_docx = "manuscript.docx",
                  manuscript_html = "manuscript.html", jats_xml = "manuscript.xml",
                  bibliography = "references.bib", citation_style = "citation-style.csl")
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
