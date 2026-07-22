#' Create a canonical scientific change request
#'
#' @param id Stable request identifier.
#' @param metric_ids Affected canonical metric identifiers.
#' @param dataset_ids Optional dataset scope.
#' @param expected_classifications Named character vector keyed by metric id.
#' @param justification Scientific justification.
#' @param status One of pending, approved, rejected, or superseded.
#' @param requested_by Requestor identifier.
#' @param decided_by Optional approver/rejector identifier.
#' @param decided_at Optional ISO-8601 decision timestamp.
#' @param supersedes Optional request identifier superseded by this request.
#' @param provenance Optional named provenance metadata.
#' @return A validated canonical change request.
#' @export
new_canonical_change_request <- function(id, metric_ids, expected_classifications,
  justification, status = "pending", dataset_ids = character(), requested_by,
  decided_by = NULL, decided_at = NULL, supersedes = NULL, provenance = list()) {
  scalar <- function(x, label, optional = FALSE) {
    if (optional && is.null(x)) return(NULL)
    if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x)))
      stop(label, " must be one non-empty string", call. = FALSE)
    trimws(x)
  }
  metric_ids <- sort(unique(tolower(as.character(metric_ids))))
  if (!length(metric_ids) || anyNA(metric_ids) || any(!nzchar(metric_ids)))
    stop("metric_ids must contain non-empty identifiers", call. = FALSE)
  allowed_classes <- c("stable", "minor", "moderate", "major", "breaking")
  if (is.null(names(expected_classifications)) ||
      !setequal(names(expected_classifications), metric_ids) ||
      any(!expected_classifications %in% allowed_classes))
    stop("expected_classifications must be named for every metric id", call. = FALSE)
  expected_classifications <- expected_classifications[metric_ids]
  status <- match.arg(status, c("pending", "approved", "rejected", "superseded"))
  if (status %in% c("approved", "rejected", "superseded") &&
      (is.null(decided_by) || is.null(decided_at)))
    stop("decided_by and decided_at are required for decided requests", call. = FALSE)
  if (!is.list(provenance) || (length(provenance) && is.null(names(provenance))))
    stop("provenance must be a named list", call. = FALSE)
  structure(list(schema_version = "1.0", id = tolower(scalar(id, "id")),
    metric_ids = metric_ids, dataset_ids = sort(unique(tolower(as.character(dataset_ids)))),
    expected_classifications = expected_classifications,
    justification = scalar(justification, "justification"), status = status,
    requested_by = scalar(requested_by, "requested_by"),
    decided_by = scalar(decided_by, "decided_by", TRUE),
    decided_at = scalar(decided_at, "decided_at", TRUE),
    supersedes = scalar(supersedes, "supersedes", TRUE), provenance = provenance),
    class = "PopgenVCFCanonicalChangeRequest")
}

#' Create a canonical change approval registry
#' @param requests Optional list of change requests.
#' @return A validated registry.
#' @export
new_canonical_change_registry <- function(requests = list()) {
  registry <- structure(list(schema_version = "1.0", requests = list()),
    class = "PopgenVCFCanonicalChangeRegistry")
  for (request in requests) registry <- register_canonical_change_request(registry, request)
  registry
}

#' Register a canonical change request
#' @param registry Change registry.
#' @param request Change request.
#' @param replace Permit replacement.
#' @return Updated registry.
#' @export
register_canonical_change_request <- function(registry, request, replace = FALSE) {
  if (!inherits(registry, "PopgenVCFCanonicalChangeRegistry"))
    stop("registry must be a canonical change registry", call. = FALSE)
  if (!inherits(request, "PopgenVCFCanonicalChangeRequest"))
    stop("request must be a canonical change request", call. = FALSE)
  if (request$id %in% names(registry$requests) && !isTRUE(replace))
    stop("change request is already registered: ", request$id, call. = FALSE)
  registry$requests[[request$id]] <- request
  registry$requests <- registry$requests[sort(names(registry$requests))]
  registry
}

#' Update canonical change request status
#' @param registry Change registry.
#' @param id Request identifier.
#' @param status New status.
#' @param decided_by Decision maker.
#' @param decided_at ISO-8601 decision timestamp.
#' @return Updated registry.
#' @export
set_canonical_change_status <- function(registry, id, status, decided_by, decided_at) {
  id <- tolower(as.character(id)[1L])
  if (!id %in% names(registry$requests)) stop("unknown change request: ", id, call. = FALSE)
  status <- match.arg(status, c("approved", "rejected", "superseded"))
  request <- registry$requests[[id]]
  request$status <- status
  request$decided_by <- as.character(decided_by)[1L]
  request$decided_at <- as.character(decided_at)[1L]
  registry$requests[[id]] <- request
  registry
}

.severity_rank <- c(stable = 0L, minor = 1L, moderate = 2L, major = 3L, breaking = 4L)

#' Reconcile canonical drift with scientific change approvals
#' @param assessment Canonical drift assessment.
#' @param registry Canonical change registry.
#' @return A canonical change reconciliation result.
#' @export
reconcile_canonical_changes <- function(assessment, registry) {
  drift <- canonical_drift_table(assessment)
  if (!inherits(registry, "PopgenVCFCanonicalChangeRegistry"))
    stop("registry must be a canonical change registry", call. = FALSE)
  approved <- Filter(function(x) identical(x$status, "approved"), registry$requests)
  approvals <- list()
  for (request in approved) {
    for (metric_id in request$metric_ids) {
      approvals[[metric_id]] <- c(approvals[[metric_id]], list(request))
    }
  }
  rows <- lapply(seq_len(nrow(drift)), function(i) {
    row <- drift[i, , drop = FALSE]
    candidates <- approvals[[row$metric_id]]
    if (is.null(candidates)) {
      outcome <- if (row$classification == "stable") "no_change" else "unexpected_change"
      return(cbind(row, request_id = NA_character_, expected_classification = NA_character_,
        reconciliation = outcome, justification = NA_character_, stringsAsFactors = FALSE))
    }
    request <- candidates[[length(candidates)]]
    expected <- unname(request$expected_classifications[[row$metric_id]])
    within <- .severity_rank[[row$classification]] <= .severity_rank[[expected]]
    outcome <- if (within) "approved_change" else "exceeds_approval"
    cbind(row, request_id = request$id, expected_classification = expected,
      reconciliation = outcome, justification = request$justification,
      stringsAsFactors = FALSE)
  })
  table <- if (length(rows)) do.call(rbind, rows) else data.frame()
  expected_ids <- unique(unlist(lapply(approved, `[[`, "metric_ids"), use.names = FALSE))
  changed_ids <- drift$metric_id[drift$classification != "stable"]
  missing <- sort(setdiff(expected_ids, changed_ids))
  missing_table <- data.frame(metric_id = missing,
    reconciliation = rep("missing_expected_change", length(missing)), stringsAsFactors = FALSE)
  release_ready <- !any(table$reconciliation %in% c("unexpected_change", "exceeds_approval")) &&
    nrow(missing_table) == 0L
  structure(list(schema_version = "1.0", table = table,
    missing_expected = missing_table, release_ready = release_ready),
    class = "PopgenVCFCanonicalChangeReconciliation")
}

#' Summarize canonical change reconciliation
#' @param reconciliation Reconciliation result.
#' @return One-row release summary.
#' @export
canonical_change_summary <- function(reconciliation) {
  if (!inherits(reconciliation, "PopgenVCFCanonicalChangeReconciliation"))
    stop("reconciliation must be a canonical change reconciliation", call. = FALSE)
  states <- c("approved_change", "no_change", "unexpected_change", "exceeds_approval")
  counts <- vapply(states, function(x) sum(reconciliation$table$reconciliation == x), integer(1))
  data.frame(metrics = nrow(reconciliation$table), approved_change = counts[[1L]],
    no_change = counts[[2L]], unexpected_change = counts[[3L]],
    exceeds_approval = counts[[4L]], missing_expected_change = nrow(reconciliation$missing_expected),
    release_ready = reconciliation$release_ready, stringsAsFactors = FALSE)
}

#' Write canonical scientific change approval evidence
#' @param reconciliation Reconciliation result.
#' @param output_dir Evidence directory.
#' @return Named normalized paths.
#' @export
write_canonical_change_evidence <- function(reconciliation, output_dir) {
  summary <- canonical_change_summary(reconciliation)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  paths <- c(metrics = file.path(output_dir, "canonical_change_reconciliation.tsv"),
    missing = file.path(output_dir, "canonical_change_missing_expected.tsv"),
    summary = file.path(output_dir, "canonical_change_summary.tsv"),
    json = file.path(output_dir, "canonical_change_reconciliation.json"),
    methods = file.path(output_dir, "canonical_change_methods.md"))
  data.table::fwrite(reconciliation$table, paths[["metrics"]], sep = "\t", quote = FALSE, na = "NA")
  data.table::fwrite(reconciliation$missing_expected, paths[["missing"]], sep = "\t", quote = FALSE, na = "NA")
  data.table::fwrite(summary, paths[["summary"]], sep = "\t", quote = FALSE, na = "NA")
  jsonlite::write_json(list(schema_version = "1.0", release_ready = reconciliation$release_ready,
    metrics = reconciliation$table, missing_expected = reconciliation$missing_expected,
    summary = summary), paths[["json"]], auto_unbox = TRUE, pretty = TRUE, na = "null")
  writeLines(paste0("Canonical scientific change reconciliation evaluated ",
    nrow(reconciliation$table), " observed metric(s) and ",
    nrow(reconciliation$missing_expected), " missing expected change(s). Release ready: ",
    reconciliation$release_ready, "."), paths[["methods"]], useBytes = TRUE)
  vapply(paths, normalizePath, character(1))
}
