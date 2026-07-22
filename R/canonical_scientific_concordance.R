#' Create a canonical scientific concordance record
#'
#' @param dataset_id Canonical dataset identifier.
#' @param analysis Analysis identifier.
#' @param reference_tool External implementation name.
#' @param reference_version External implementation version.
#' @param command Exact command or reproducible invocation.
#' @param result A `PopgenVCFExternalReferenceResult`.
#' @param tolerance_profile Named tolerance metadata.
#' @param environment Named environment/provenance metadata.
#' @param approval One of `proposed` or `approved`.
#' @param approved_by,approved_at Approval metadata.
#' @return A validated `PopgenVCFScientificConcordanceRecord`.
#' @export
new_scientific_concordance_record <- function(
    dataset_id, analysis, reference_tool, reference_version, command, result,
    tolerance_profile, environment = list(),
    approval = c("proposed", "approved"), approved_by = NULL, approved_at = NULL) {
  scalar <- function(x, label) {
    if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x)))
      stop(label, " must be one non-empty string", call. = FALSE)
    trimws(x)
  }
  if (!inherits(result, "PopgenVCFExternalReferenceResult"))
    stop("result must be a PopgenVCFExternalReferenceResult", call. = FALSE)
  if (!is.list(tolerance_profile) || is.null(names(tolerance_profile)))
    stop("tolerance_profile must be a named list", call. = FALSE)
  if (!is.list(environment) || (length(environment) && is.null(names(environment))))
    stop("environment must be a named list", call. = FALSE)
  approval <- match.arg(approval)
  if (approval == "approved") {
    approved_by <- scalar(approved_by, "approved_by")
    approved_at <- scalar(approved_at, "approved_at")
    if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", approved_at))
      stop("approved_at must be an ISO-8601 date", call. = FALSE)
  } else if (!is.null(approved_by) || !is.null(approved_at)) {
    stop("proposed records cannot contain approval metadata", call. = FALSE)
  }
  x <- structure(list(
    schema_version = "1.0",
    dataset_id = tolower(scalar(dataset_id, "dataset_id")),
    analysis = tolower(scalar(analysis, "analysis")),
    reference_tool = scalar(reference_tool, "reference_tool"),
    reference_version = scalar(reference_version, "reference_version"),
    command = scalar(command, "command"),
    status = result$status,
    role = result$role,
    mode = result$mode,
    passed = identical(result$status, "passed"),
    comparisons = external_reference_table(result),
    tolerance_profile = tolerance_profile[sort(names(tolerance_profile))],
    environment = environment[sort(names(environment))],
    interpretation = result$interpretation,
    citations = sort(unique(result$citations)),
    approval = approval,
    approved_by = approved_by,
    approved_at = approved_at
  ), class = "PopgenVCFScientificConcordanceRecord")
  validate_scientific_concordance_record(x)
}

#' Validate a scientific concordance record
#' @param record Concordance record.
#' @param require_approved Require explicit approval.
#' @return `record`, invisibly.
#' @export
validate_scientific_concordance_record <- function(record, require_approved = FALSE) {
  if (!inherits(record, "PopgenVCFScientificConcordanceRecord"))
    stop("record must be a PopgenVCFScientificConcordanceRecord", call. = FALSE)
  required <- c("schema_version", "dataset_id", "analysis", "reference_tool",
    "reference_version", "command", "status", "role", "mode", "passed",
    "comparisons", "tolerance_profile", "environment", "interpretation",
    "citations", "approval", "approved_by", "approved_at")
  if (!all(required %in% names(record)) || !identical(record$schema_version, "1.0"))
    stop("invalid scientific concordance record schema", call. = FALSE)
  if (!record$status %in% c("passed", "failed", "skipped", "error"))
    stop("invalid scientific concordance status", call. = FALSE)
  if (record$role == "equivalence" && !identical(record$passed, record$status == "passed"))
    stop("equivalence pass state is inconsistent", call. = FALSE)
  if (isTRUE(require_approved) && !identical(record$approval, "approved"))
    stop("scientific concordance record is not approved", call. = FALSE)
  invisible(record)
}

#' Assemble a canonical scientific concordance suite
#' @param records List of concordance records.
#' @param require_complete Fail when required tools or analyses are absent.
#' @param required_tools,required_analyses Required inventory.
#' @return A `PopgenVCFScientificConcordanceSuite`.
#' @export
new_scientific_concordance_suite <- function(records, require_complete = TRUE,
    required_tools = character(), required_analyses = character()) {
  if (!is.list(records) || !length(records)) stop("records must be a non-empty list", call. = FALSE)
  lapply(records, validate_scientific_concordance_record)
  key <- vapply(records, function(x) paste(x$analysis, x$reference_tool, sep = "::"), character(1))
  if (anyDuplicated(key)) stop("concordance analysis/tool pairs must be unique", call. = FALSE)
  records <- records[order(key)]
  missing_tools <- setdiff(required_tools, vapply(records, `[[`, character(1), "reference_tool"))
  missing_analyses <- setdiff(required_analyses, vapply(records, `[[`, character(1), "analysis"))
  if (isTRUE(require_complete) && (length(missing_tools) || length(missing_analyses)))
    stop("scientific concordance inventory is incomplete", call. = FALSE)
  structure(list(schema_version = "1.0", records = records,
    required_tools = sort(unique(required_tools)), required_analyses = sort(unique(required_analyses)),
    missing_tools = sort(missing_tools), missing_analyses = sort(missing_analyses),
    release_ready = all(vapply(records, function(x) x$role == "diagnostic" ||
      (x$passed && x$approval == "approved"), logical(1)))),
    class = "PopgenVCFScientificConcordanceSuite")
}

#' Return the scientific concordance summary table
#' @param suite Concordance suite.
#' @return Deterministically ordered data frame.
#' @export
scientific_concordance_table <- function(suite) {
  if (!inherits(suite, "PopgenVCFScientificConcordanceSuite"))
    stop("suite must be a PopgenVCFScientificConcordanceSuite", call. = FALSE)
  do.call(rbind, lapply(suite$records, function(x) data.frame(
    dataset_id = x$dataset_id, analysis = x$analysis,
    reference_tool = x$reference_tool, reference_version = x$reference_version,
    role = x$role, mode = x$mode, status = x$status, passed = x$passed,
    approval = x$approval, command = x$command, stringsAsFactors = FALSE)))
}

#' Write deterministic scientific concordance evidence
#' @param suite Concordance suite.
#' @param output_dir Evidence directory.
#' @param require_release_ready Refuse evidence finalization unless ready.
#' @return Named normalized evidence paths.
#' @export
write_scientific_concordance_evidence <- function(suite, output_dir,
    require_release_ready = FALSE) {
  if (!inherits(suite, "PopgenVCFScientificConcordanceSuite"))
    stop("suite must be a PopgenVCFScientificConcordanceSuite", call. = FALSE)
  if (isTRUE(require_release_ready) && !isTRUE(suite$release_ready))
    stop("scientific concordance suite is not release ready", call. = FALSE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  summary <- scientific_concordance_table(suite)
  tsv <- file.path(output_dir, "scientific_concordance.tsv")
  json <- file.path(output_dir, "scientific_concordance.json")
  methods <- file.path(output_dir, "scientific_concordance_methods.md")
  data.table::fwrite(summary, tsv, sep = "\t", quote = FALSE, na = "NA")
  payload <- list(schema_version = "1.0", release_ready = suite$release_ready,
    required_tools = suite$required_tools, required_analyses = suite$required_analyses,
    records = lapply(suite$records, unclass))
  jsonlite::write_json(payload, json, auto_unbox = TRUE, pretty = TRUE, na = "null", digits = 17)
  lines <- c("# Scientific concordance methods", "",
    paste("Canonical comparisons:", nrow(summary)),
    paste("Release ready:", suite$release_ready), "",
    "Each record preserves the external implementation version, exact command,",
    "tolerance profile, environment provenance, comparison table, citations, and approval state.")
  writeLines(lines, methods, useBytes = TRUE)
  c(tsv = normalizePath(tsv), json = normalizePath(json), methods = normalizePath(methods))
}
