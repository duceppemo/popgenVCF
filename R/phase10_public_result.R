# Phase 10.1.4 — public result inspection adapter

.phase10_public_result_table <- function(result) {
  if (inherits(result, "PopgenVCFCoreResult")) {
    validate_core_result(result)
    return(as.data.frame(core_result_table(result), stringsAsFactors = FALSE))
  }
  if (inherits(result, "PopgenVCFAncestryReplicate")) {
    validate_ancestry_replicate(result)
    table <- as.data.frame(ancestry_result_table(result), stringsAsFactors = FALSE)
    return(table[, setdiff(names(table), c("seed", "runtime_seconds")), drop = FALSE])
  }
  if (inherits(result, "PopgenVCFAncestryResult")) {
    validate_ancestry_result(result)
    table <- as.data.frame(ancestry_result_table(result), stringsAsFactors = FALSE)
    return(table[, setdiff(names(table), c("seed", "runtime_seconds")), drop = FALSE])
  }
  stop("unsupported canonical result object", call. = FALSE)
}

.phase10_public_result_analysis <- function(result) {
  if (inherits(result, "PopgenVCFCoreResult")) return(result$analysis)
  if (inherits(result, c("PopgenVCFAncestryResult", "PopgenVCFAncestryReplicate"))) {
    return("ancestry")
  }
  stop("unsupported canonical result object", call. = FALSE)
}

.phase10_public_result_validation <- function(result) {
  if (!inherits(result, "PopgenVCFCoreResult")) {
    return(data.frame(check = "object_schema", passed = TRUE, stringsAsFactors = FALSE))
  }
  validation <- as.data.frame(result$validation, stringsAsFactors = FALSE)
  validation <- validation[, c("check", "passed"), drop = FALSE]
  validation <- validation[order(validation$check, method = "radix"), , drop = FALSE]
  rownames(validation) <- NULL
  validation
}

.phase10_public_result_artifact_ids <- function(result) {
  if (!inherits(result, "PopgenVCFCoreResult")) {
    return(stats::setNames(character(), character()))
  }
  validate_artifact_manifest(result$artifacts, must_exist = FALSE)
  if (!length(result$artifacts)) return(stats::setNames(character(), character()))
  ids <- vapply(result$artifacts, .phase10_public_artifact_id, character(1L))
  ids <- sort(ids, method = "radix")
  stats::setNames(ids, ids)
}

.phase10_public_result_projection <- function(result) {
  table <- .phase10_public_result_table(result)
  rownames(table) <- NULL
  list(
    analysis = .phase10_public_result_analysis(result),
    result_class = class(result)[1L],
    table = table,
    validation = .phase10_public_result_validation(result)
  )
}

#' Inspect a canonical public scientific result
#'
#' Validates an existing canonical result and translates its stable scientific
#' content into the Phase 10 public API. Parameters, raw provenance, metadata,
#' filesystem paths, software details, seeds, runtimes, and mutable execution
#' fields remain internal.
#'
#' @param request A canonical public request for `result.inspect`.
#' @param result A canonical core or ancestry result object.
#' @return A validated `PopgenVCFPublicAPIResponse`.
#' @export
inspect_public_result <- function(request, result) {
  validate_public_analysis_request(request)
  if (!identical(request$operation_id, "result.inspect")) {
    return(.phase10_public_failure(
      request, "unsupported_operation",
      "This adapter accepts only result.inspect requests."
    ))
  }

  projection <- tryCatch(
    .phase10_public_result_projection(result),
    error = function(e) e
  )
  if (inherits(projection, "error")) {
    return(.phase10_public_failure(
      request, "invalid_result_object", conditionMessage(projection)
    ))
  }

  result_id <- paste0(
    "result::", projection$analysis, "::",
    phase10_public_fingerprint(projection)
  )

  new_public_analysis_response(
    request = request,
    status = "completed",
    scientific_values = list(
      analysis = projection$analysis,
      primary_table = projection$table,
      result_class = projection$result_class,
      result_id = result_id,
      validation = projection$validation
    ),
    artifact_ids = .phase10_public_result_artifact_ids(result),
    provenance_ids = stats::setNames(result_id, "result")
  )
}
