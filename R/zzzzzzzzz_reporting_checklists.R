reporting_checklist_scalar <- function(x, label, allow_na = FALSE) {
  x <- as.character(x)[1L]
  if (is.na(x)) {
    if (allow_na) return(NA_character_)
    stop(label, " must be non-empty", call. = FALSE)
  }
  x <- trimws(x)
  if (!nzchar(x)) stop(label, " must be non-empty", call. = FALSE)
  x
}

reporting_checklist_items <- function(items) {
  items <- data.table::as.data.table(items)
  required <- c("item_id", "category", "label", "requirement", "guidance")
  if (!all(required %in% names(items))) {
    stop("items must contain: ", paste(required, collapse = ", "), call. = FALSE)
  }
  items <- data.table::copy(items)[, ..required]
  for (column in required) items[[column]] <- trimws(as.character(items[[column]]))
  if (!nrow(items)) stop("items must contain at least one checklist item", call. = FALSE)
  if (anyNA(items) || any(!nzchar(unlist(items, use.names = FALSE)))) {
    stop("checklist items must contain non-empty values", call. = FALSE)
  }
  if (anyDuplicated(items$item_id)) stop("checklist item_id values must be unique", call. = FALSE)
  if (any(!grepl("^[a-z0-9][a-z0-9._-]*$", items$item_id))) {
    stop("checklist item_id values must use lowercase letters, numbers, dots, underscores, or hyphens", call. = FALSE)
  }
  allowed <- c("required", "recommended")
  if (any(!items$requirement %in% allowed)) {
    stop("item requirement must be required or recommended", call. = FALSE)
  }
  data.table::setorderv(items, c("category", "item_id"))
  items
}

#' Create a deterministic reporting checklist
#'
#' @param id Stable checklist identifier.
#' @param items Data frame containing item_id, category, label, requirement, and guidance.
#' @param version Checklist version.
#' @param title Human-readable checklist title.
#' @param organization Issuing organization or generic source.
#' @param status Checklist status: generic, verified, or deprecated.
#' @param source_url,source_date Source and verification date for verified checklists.
#' @param description Human-readable description.
#' @return A validated `PopgenVCFReportingChecklist`.
#' @export
new_reporting_checklist <- function(id, items, version = "1.0", title = id,
                                    organization = "popgenVCF", status = c("generic", "verified", "deprecated"),
                                    source_url = NA_character_, source_date = NA_character_,
                                    description = "Deterministic reporting checklist") {
  status <- match.arg(status)
  source <- list(
    url = reporting_checklist_scalar(source_url, "source_url", allow_na = TRUE),
    date = reporting_checklist_scalar(source_date, "source_date", allow_na = TRUE)
  )
  if (status == "verified" && (is.na(source$url) || is.na(source$date))) {
    stop("verified checklists require source_url and source_date", call. = FALSE)
  }
  payload <- list(
    schema_version = "1.0",
    id = reporting_checklist_scalar(id, "id"),
    version = reporting_checklist_scalar(version, "version"),
    title = reporting_checklist_scalar(title, "title"),
    organization = reporting_checklist_scalar(organization, "organization"),
    status = status,
    source = source,
    description = reporting_checklist_scalar(description, "description"),
    items = reporting_checklist_items(items)
  )
  payload$digest <- digest::digest(payload, algo = "sha256", serialize = TRUE)
  checklist <- structure(payload, class = "PopgenVCFReportingChecklist")
  validate_reporting_checklist(checklist)
  checklist
}

#' Validate a reporting checklist or written checklist directory
#'
#' @param x A `PopgenVCFReportingChecklist` or written checklist directory.
#' @return `TRUE` invisibly.
#' @export
validate_reporting_checklist <- function(x) {
  if (is.character(x) && length(x) == 1L) {
    required <- c("reporting-checklist.json", "reporting-checklist.md", "reporting-checklist-items.tsv", "reporting-checklist-manifest.tsv")
    missing <- required[!file.exists(file.path(x, required))]
    if (length(missing)) stop("reporting checklist directory is missing: ", paste(missing, collapse = ", "), call. = FALSE)
    manifest <- data.table::fread(file.path(x, "reporting-checklist-manifest.tsv"))
    if (!all(c("path", "size_bytes", "sha256") %in% names(manifest))) stop("invalid reporting checklist manifest", call. = FALSE)
    for (i in seq_len(nrow(manifest))) {
      path <- file.path(x, manifest$path[[i]])
      if (!file.exists(path)) stop("reporting checklist file is missing: ", manifest$path[[i]], call. = FALSE)
      if (!identical(digest::digest(path, algo = "sha256", file = TRUE), manifest$sha256[[i]])) {
        stop("reporting checklist checksum mismatch: ", manifest$path[[i]], call. = FALSE)
      }
    }
    return(invisible(TRUE))
  }
  if (!inherits(x, "PopgenVCFReportingChecklist")) stop("x must be a PopgenVCFReportingChecklist or directory", call. = FALSE)
  if (!identical(x$schema_version, "1.0")) stop("unsupported reporting checklist schema version", call. = FALSE)
  reporting_checklist_items(x$items)
  if (!x$status %in% c("generic", "verified", "deprecated")) stop("invalid reporting checklist status", call. = FALSE)
  if (identical(x$status, "verified") && (is.na(x$source$url) || is.na(x$source$date))) {
    stop("verified checklists require source_url and source_date", call. = FALSE)
  }
  payload <- x[setdiff(names(x), "digest")]
  expected <- digest::digest(payload, algo = "sha256", serialize = TRUE)
  if (!identical(expected, x$digest)) stop("reporting checklist digest mismatch", call. = FALSE)
  invisible(TRUE)
}

#' Return the generic population-genomics reporting checklist
#'
#' @return A `PopgenVCFReportingChecklist`.
#' @export
generic_reporting_checklist <- function() {
  items <- data.table::data.table(
    item_id = c(
      "samples.identity", "samples.grouping", "variants.filtering", "software.identity",
      "parameters.recorded", "randomness.recorded", "data.availability", "code.availability",
      "artifacts.traceable", "limitations.stated"
    ),
    category = c(
      "samples", "samples", "methods", "reproducibility", "reproducibility",
      "reproducibility", "availability", "availability", "results", "interpretation"
    ),
    label = c(
      "Canonical sample identities are reported", "Population or grouping definitions are reported",
      "Variant filtering criteria are reported", "Software names and versions are reported",
      "Analysis parameters are recorded", "Seeds or stochastic settings are recorded when applicable",
      "Data availability is stated", "Code and software availability are stated",
      "Figures and tables are traceable to canonical results", "Study and analysis limitations are stated"
    ),
    requirement = c("required", "recommended", "required", "required", "required", "required", "required", "required", "required", "recommended"),
    guidance = c(
      "Report immutable VCF/GDS sample keys and any public aliases.",
      "Describe population, site, treatment, or other grouping definitions used in analysis.",
      "Report filtering thresholds, missingness rules, MAF rules, and LD pruning where applicable.",
      "Record package, backend, and external-tool versions used for each analysis.",
      "Record the effective parameters used to generate canonical results.",
      "Record seeds, RNG streams, replicate counts, and stochastic backend settings where applicable.",
      "Provide repository, accession, embargo, or controlled-access information supplied by the authors.",
      "Provide source-code, package, workflow, and environment availability information supplied by the authors.",
      "Preserve identifiers or provenance links connecting each reported artifact to its producer result.",
      "State design, sampling, model, data-quality, and interpretation limitations supplied by the authors."
    )
  )
  new_reporting_checklist(
    id = "generic-population-genomics",
    title = "Generic population-genomics reporting checklist",
    description = "A conservative structural checklist for reproducible population-genomics reporting.",
    items = items
  )
}

reporting_checklist_responses <- function(responses, checklist) {
  if (is.null(responses)) responses <- data.frame(item_id = character(), response = character())
  responses <- data.table::as.data.table(responses)
  required <- c("item_id", "response")
  if (!all(required %in% names(responses))) stop("responses must contain item_id and response", call. = FALSE)
  out <- data.table::copy(responses)
  if (!"evidence" %in% names(out)) out[, evidence := ""]
  if (!"notes" %in% names(out)) out[, notes := ""]
  out <- out[, .(item_id = trimws(as.character(item_id)), response = trimws(tolower(as.character(response))),
                 evidence = trimws(as.character(evidence)), notes = trimws(as.character(notes)))]
  if (anyDuplicated(out$item_id)) stop("responses must contain at most one row per item_id", call. = FALSE)
  unknown <- setdiff(out$item_id, checklist$items$item_id)
  if (length(unknown)) stop("responses contain unknown item_id values: ", paste(sort(unknown), collapse = ", "), call. = FALSE)
  allowed <- c("yes", "no", "partial", "not_applicable", "unanswered")
  if (any(!out$response %in% allowed)) stop("response must be yes, no, partial, not_applicable, or unanswered", call. = FALSE)
  out
}

#' Evaluate explicit responses against a reporting checklist
#'
#' @param checklist A validated `PopgenVCFReportingChecklist`.
#' @param responses Data frame with item_id, response, and optional evidence and notes.
#' @param strict Whether incomplete required items raise an error.
#' @return A deterministic `PopgenVCFReportingChecklistReport` data table.
#' @export
validate_reporting_checklist_responses <- function(checklist, responses = NULL, strict = FALSE) {
  validate_reporting_checklist(checklist)
  responses <- reporting_checklist_responses(responses, checklist)
  report <- merge(checklist$items, responses, by = "item_id", all.x = TRUE, sort = FALSE)
  report[is.na(response), response := "unanswered"]
  report[is.na(evidence), evidence := ""]
  report[is.na(notes), notes := ""]
  report[, status := data.table::fcase(
    response == "yes" & nzchar(evidence), "pass",
    response == "not_applicable" & nzchar(notes), "not_applicable",
    response == "yes", "incomplete",
    response %in% c("no", "partial", "unanswered"), "incomplete",
    default = "incomplete"
  )]
  report[, message := data.table::fcase(
    status == "pass", "Explicit response and evidence supplied",
    status == "not_applicable", "Explicit not-applicable rationale supplied",
    response == "yes", "Evidence reference is required",
    response == "partial", "Partial response requires completion or explicit rationale",
    response == "no", "Checklist item is not satisfied",
    default = "Checklist item has not been answered"
  )]
  data.table::setorderv(report, c("category", "item_id"))
  class(report) <- c("PopgenVCFReportingChecklistReport", class(report))
  failed_required <- report$requirement == "required" & report$status == "incomplete"
  if (isTRUE(strict) && any(failed_required)) {
    stop("Required reporting checklist items are incomplete: ", paste(report$item_id[failed_required], collapse = ", "), call. = FALSE)
  }
  report
}

#' Render a reporting checklist as Markdown
#'
#' @param checklist A validated `PopgenVCFReportingChecklist`.
#' @param responses Optional explicit response data frame.
#' @return Character vector containing Markdown.
#' @export
render_reporting_checklist <- function(checklist, responses = NULL) {
  validate_reporting_checklist(checklist)
  report <- validate_reporting_checklist_responses(checklist, responses)
  lines <- c(
    paste0("# ", checklist$title), "",
    paste0("- Checklist ID: `", checklist$id, "`"),
    paste0("- Version: ", checklist$version),
    paste0("- Status: ", checklist$status),
    paste0("- Digest: `", checklist$digest, "`"), "",
    "| Item | Category | Requirement | Response | Status | Evidence |",
    "|---|---|---|---|---|---|"
  )
  rows <- vapply(seq_len(nrow(report)), function(i) {
    paste0("| `", report$item_id[[i]], "` | ", report$category[[i]], " | ", report$requirement[[i]],
           " | ", report$response[[i]], " | ", report$status[[i]], " | ", report$evidence[[i]], " |")
  }, character(1L))
  c(lines, rows)
}

#' Write a deterministic reporting checklist bundle
#'
#' @param checklist A validated `PopgenVCFReportingChecklist`.
#' @param directory Parent output directory.
#' @param responses Optional explicit response data frame.
#' @param overwrite Whether an existing output directory may be replaced.
#' @return Normalized output directory invisibly.
#' @export
write_reporting_checklist <- function(checklist, directory, responses = NULL, overwrite = FALSE) {
  validate_reporting_checklist(checklist)
  report <- validate_reporting_checklist_responses(checklist, responses)
  out <- file.path(directory, "reporting-checklist")
  if (dir.exists(out)) {
    if (!isTRUE(overwrite)) stop("reporting checklist directory already exists", call. = FALSE)
    unlink(out, recursive = TRUE, force = TRUE)
  }
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(unclass(checklist), file.path(out, "reporting-checklist.json"), auto_unbox = TRUE, pretty = TRUE, na = "null")
  writeLines(render_reporting_checklist(checklist, responses), file.path(out, "reporting-checklist.md"), useBytes = TRUE)
  data.table::fwrite(report, file.path(out, "reporting-checklist-items.tsv"), sep = "\t")
  files <- sort(list.files(out, full.names = TRUE))
  manifest <- data.table::data.table(
    path = basename(files), size_bytes = file.info(files)$size,
    sha256 = vapply(files, digest::digest, character(1L), algo = "sha256", file = TRUE)
  )
  data.table::fwrite(manifest, file.path(out, "reporting-checklist-manifest.tsv"), sep = "\t")
  validate_reporting_checklist(out)
  invisible(normalizePath(out, winslash = "/", mustWork = TRUE))
}
