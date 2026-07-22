scientific_concordance_scalar <- function(x, label, lower = FALSE) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    stop(label, " must be one non-empty string", call. = FALSE)
  }
  value <- trimws(x)
  if (isTRUE(lower)) tolower(value) else value
}

scientific_concordance_tool_id <- function(x) {
  tolower(trimws(as.character(x)))
}

scientific_concordance_record_key <- function(record) {
  paste(
    record$dataset_id,
    record$analysis,
    scientific_concordance_tool_id(record$reference_tool),
    record$reference_version,
    sep = "::"
  )
}

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
  if (!inherits(result, "PopgenVCFExternalReferenceResult")) {
    stop("result must be a PopgenVCFExternalReferenceResult", call. = FALSE)
  }
  if (!is.list(tolerance_profile) || is.null(names(tolerance_profile)) ||
      anyNA(names(tolerance_profile)) || any(!nzchar(names(tolerance_profile)))) {
    stop("tolerance_profile must be a named list", call. = FALSE)
  }
  if (!is.list(environment) || (length(environment) &&
      (is.null(names(environment)) || anyNA(names(environment)) || any(!nzchar(names(environment)))))) {
    stop("environment must be a named list", call. = FALSE)
  }
  approval <- match.arg(approval)
  if (approval == "approved") {
    approved_by <- scientific_concordance_scalar(approved_by, "approved_by")
    approved_at <- scientific_concordance_scalar(approved_at, "approved_at")
    if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", approved_at)) {
      stop("approved_at must be an ISO-8601 date", call. = FALSE)
    }
  } else if (!is.null(approved_by) || !is.null(approved_at)) {
    stop("proposed records cannot contain approval metadata", call. = FALSE)
  }
  x <- structure(list(
    schema_version = "1.1",
    dataset_id = scientific_concordance_scalar(dataset_id, "dataset_id", lower = TRUE),
    analysis = scientific_concordance_scalar(analysis, "analysis", lower = TRUE),
    reference_tool = scientific_concordance_scalar(reference_tool, "reference_tool"),
    reference_version = scientific_concordance_scalar(reference_version, "reference_version"),
    command = scientific_concordance_scalar(command, "command"),
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
  x
}

#' Validate a scientific concordance record
#' @param record Concordance record.
#' @param require_approved Require explicit approval.
#' @return `record`, invisibly.
#' @export
validate_scientific_concordance_record <- function(record, require_approved = FALSE) {
  if (!inherits(record, "PopgenVCFScientificConcordanceRecord")) {
    stop("record must be a PopgenVCFScientificConcordanceRecord", call. = FALSE)
  }
  required <- c("schema_version", "dataset_id", "analysis", "reference_tool",
    "reference_version", "command", "status", "role", "mode", "passed",
    "comparisons", "tolerance_profile", "environment", "interpretation",
    "citations", "approval", "approved_by", "approved_at")
  if (!all(required %in% names(record)) || !record$schema_version %in% c("1.0", "1.1")) {
    stop("invalid scientific concordance record schema", call. = FALSE)
  }
  for (field in c("dataset_id", "analysis", "reference_tool", "reference_version", "command")) {
    scientific_concordance_scalar(record[[field]], field)
  }
  if (!record$status %in% c("passed", "failed", "skipped", "error")) {
    stop("invalid scientific concordance status", call. = FALSE)
  }
  if (!record$role %in% c("equivalence", "diagnostic")) {
    stop("invalid scientific concordance role", call. = FALSE)
  }
  if (!is.logical(record$passed) || length(record$passed) != 1L || is.na(record$passed) ||
      !identical(record$passed, record$status == "passed")) {
    stop("scientific concordance pass state is inconsistent", call. = FALSE)
  }
  if (!is.data.frame(record$comparisons)) {
    stop("comparisons must be a data frame", call. = FALSE)
  }
  if (!is.list(record$tolerance_profile) || is.null(names(record$tolerance_profile)) ||
      !is.list(record$environment) || (length(record$environment) && is.null(names(record$environment)))) {
    stop("invalid concordance tolerance or environment metadata", call. = FALSE)
  }
  if (!record$approval %in% c("proposed", "approved")) {
    stop("invalid scientific concordance approval state", call. = FALSE)
  }
  if (record$approval == "approved") {
    scientific_concordance_scalar(record$approved_by, "approved_by")
    if (!is.character(record$approved_at) || length(record$approved_at) != 1L ||
        is.na(record$approved_at) || !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", record$approved_at)) {
      stop("approved records require valid approval metadata", call. = FALSE)
    }
  } else if (!is.null(record$approved_by) || !is.null(record$approved_at)) {
    stop("proposed records cannot contain approval metadata", call. = FALSE)
  }
  if (isTRUE(require_approved) && !identical(record$approval, "approved")) {
    stop("scientific concordance record is not approved", call. = FALSE)
  }
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
  if (!is.list(records) || !length(records)) {
    stop("records must be a non-empty list", call. = FALSE)
  }
  lapply(records, validate_scientific_concordance_record)
  keys <- vapply(records, scientific_concordance_record_key, character(1))
  if (anyDuplicated(keys)) {
    stop("concordance dataset/analysis/tool/version records must be unique", call. = FALSE)
  }
  records <- records[order(keys)]

  required_tools <- trimws(as.character(required_tools))
  required_tools <- required_tools[nzchar(required_tools)]
  required_tools <- required_tools[!duplicated(scientific_concordance_tool_id(required_tools))]
  required_analyses <- sort(unique(tolower(trimws(as.character(required_analyses)))))
  required_analyses <- required_analyses[nzchar(required_analyses)]

  observed_tool_ids <- scientific_concordance_tool_id(
    vapply(records, `[[`, character(1), "reference_tool")
  )
  missing_tools <- required_tools[
    !scientific_concordance_tool_id(required_tools) %in% observed_tool_ids
  ]
  observed_analyses <- vapply(records, `[[`, character(1), "analysis")
  missing_analyses <- setdiff(required_analyses, observed_analyses)
  inventory_complete <- !length(missing_tools) && !length(missing_analyses)
  if (isTRUE(require_complete) && !inventory_complete) {
    stop("scientific concordance inventory is incomplete", call. = FALSE)
  }
  equivalence_ready <- all(vapply(records, function(x) {
    x$role == "diagnostic" || (x$passed && x$approval == "approved")
  }, logical(1)))

  suite <- structure(list(
    schema_version = "1.1",
    records = records,
    required_tools = sort(required_tools),
    required_analyses = required_analyses,
    missing_tools = sort(missing_tools),
    missing_analyses = sort(missing_analyses),
    inventory_complete = inventory_complete,
    release_ready = inventory_complete && equivalence_ready
  ), class = "PopgenVCFScientificConcordanceSuite")
  validate_scientific_concordance_suite(suite)
  suite
}

#' Validate a scientific concordance suite
#' @param suite Concordance suite.
#' @return `suite`, invisibly.
#' @export
validate_scientific_concordance_suite <- function(suite) {
  if (!inherits(suite, "PopgenVCFScientificConcordanceSuite")) {
    stop("suite must be a PopgenVCFScientificConcordanceSuite", call. = FALSE)
  }
  required <- c("schema_version", "records", "required_tools", "required_analyses",
    "missing_tools", "missing_analyses", "inventory_complete", "release_ready")
  if (!all(required %in% names(suite)) || !identical(suite$schema_version, "1.1") ||
      !is.list(suite$records) || !length(suite$records)) {
    stop("invalid scientific concordance suite schema", call. = FALSE)
  }
  lapply(suite$records, validate_scientific_concordance_record)
  keys <- vapply(suite$records, scientific_concordance_record_key, character(1))
  if (anyDuplicated(keys) || !identical(keys, sort(keys))) {
    stop("scientific concordance records are duplicated or not deterministic", call. = FALSE)
  }
  observed_tool_ids <- scientific_concordance_tool_id(
    vapply(suite$records, `[[`, character(1), "reference_tool")
  )
  expected_missing_tools <- suite$required_tools[
    !scientific_concordance_tool_id(suite$required_tools) %in% observed_tool_ids
  ]
  observed_analyses <- vapply(suite$records, `[[`, character(1), "analysis")
  expected_missing_analyses <- setdiff(suite$required_analyses, observed_analyses)
  inventory_complete <- !length(expected_missing_tools) && !length(expected_missing_analyses)
  equivalence_ready <- all(vapply(suite$records, function(x) {
    x$role == "diagnostic" || (x$passed && x$approval == "approved")
  }, logical(1)))
  if (!identical(sort(suite$missing_tools), sort(expected_missing_tools)) ||
      !identical(sort(suite$missing_analyses), sort(expected_missing_analyses)) ||
      !identical(suite$inventory_complete, inventory_complete) ||
      !identical(suite$release_ready, inventory_complete && equivalence_ready)) {
    stop("scientific concordance suite state is inconsistent", call. = FALSE)
  }
  invisible(suite)
}

#' Return the scientific concordance summary table
#' @param suite Concordance suite.
#' @return Deterministically ordered data frame.
#' @export
scientific_concordance_table <- function(suite) {
  validate_scientific_concordance_suite(suite)
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
  validate_scientific_concordance_suite(suite)
  if (isTRUE(require_release_ready) && !isTRUE(suite$release_ready)) {
    stop("scientific concordance suite is not release ready", call. = FALSE)
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  summary <- scientific_concordance_table(suite)
  tsv <- file.path(output_dir, "scientific_concordance.tsv")
  json <- file.path(output_dir, "scientific_concordance.json")
  methods <- file.path(output_dir, "scientific_concordance_methods.md")
  data.table::fwrite(summary, tsv, sep = "\t", quote = FALSE, na = "NA")
  payload <- list(
    schema_version = "1.1",
    release_ready = suite$release_ready,
    inventory_complete = suite$inventory_complete,
    required_tools = suite$required_tools,
    required_analyses = suite$required_analyses,
    missing_tools = suite$missing_tools,
    missing_analyses = suite$missing_analyses,
    records = lapply(suite$records, unclass)
  )
  jsonlite::write_json(payload, json, auto_unbox = TRUE, pretty = TRUE,
    na = "null", digits = 17)
  lines <- c(
    "# Scientific concordance methods", "",
    paste("Canonical comparisons:", nrow(summary)),
    paste("Inventory complete:", suite$inventory_complete),
    paste("Release ready:", suite$release_ready), "",
    "Each record preserves the external implementation version, exact command,",
    "tolerance profile, environment provenance, comparison table, citations, and approval state."
  )
  writeLines(lines, methods, useBytes = TRUE)
  c(tsv = normalizePath(tsv), json = normalizePath(json), methods = normalizePath(methods))
}
