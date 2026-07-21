# Phase 10.2.1 — deterministic public API compatibility contracts

#' Compare two stable public API descriptors
#'
#' @param baseline Baseline `PopgenVCFPublicAPIDescriptor`.
#' @param candidate Candidate `PopgenVCFPublicAPIDescriptor`.
#' @return A deterministic compatibility record.
#' @export
compare_phase10_api_descriptors <- function(baseline, candidate) {
  validate_phase10_api_descriptor(baseline)
  validate_phase10_api_descriptor(candidate)

  old <- phase10_api_operations(baseline)
  new <- phase10_api_operations(candidate)
  ids <- sort(unique(c(old$operation_id, new$operation_id)))

  changes <- do.call(rbind, lapply(ids, function(id) {
    before <- old[old$operation_id == id, , drop = FALSE]
    after <- new[new$operation_id == id, , drop = FALSE]
    .phase10_compare_operation(id, before, after)
  }))
  rownames(changes) <- NULL

  rank <- c(compatible = 1L, additive = 2L, deprecated = 3L, breaking = 4L)
  overall <- names(rank)[which.max(rank[changes$classification])]
  record <- list(
    record_type = "popgenvcf_public_api_compatibility",
    schema_version = "1.0.0",
    baseline_api_version = baseline$api_version,
    candidate_api_version = candidate$api_version,
    baseline_fingerprint = baseline$fingerprint,
    candidate_fingerprint = candidate$fingerprint,
    classification = overall,
    release_compatible = !identical(overall, "breaking"),
    changes = changes
  )
  record$fingerprint <- phase10_public_fingerprint(record)
  class(record) <- c("PopgenVCFPublicAPICompatibility", "list")
  record
}

#' Validate a public API compatibility record
#'
#' @param compatibility Compatibility record.
#' @param allow_breaking Whether an explicitly reviewed breaking change is allowed.
#' @return `TRUE`, invisibly.
#' @export
validate_phase10_api_compatibility <- function(compatibility, allow_breaking = FALSE) {
  if (!inherits(compatibility, "PopgenVCFPublicAPICompatibility")) {
    stop("compatibility must be a public API compatibility record.", call. = FALSE)
  }
  required <- c(
    "operation_id", "classification", "reason", "request_change",
    "response_change", "lifecycle_change"
  )
  if (!is.data.frame(compatibility$changes) ||
      !identical(names(compatibility$changes), required) ||
      any(!compatibility$changes$classification %in%
          c("compatible", "additive", "deprecated", "breaking"))) {
    stop("Malformed public API compatibility changes.", call. = FALSE)
  }
  if (!is.logical(allow_breaking) || length(allow_breaking) != 1L || is.na(allow_breaking)) {
    stop("allow_breaking must be TRUE or FALSE.", call. = FALSE)
  }
  expected <- phase10_public_fingerprint(compatibility)
  if (!identical(compatibility$fingerprint, expected)) {
    stop("Public API compatibility fingerprint verification failed.", call. = FALSE)
  }
  if (identical(compatibility$classification, "breaking") && !allow_breaking) {
    stop("Breaking public API drift requires explicit approval.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Render a deterministic public API compatibility report
#'
#' @param compatibility Compatibility record.
#' @return Character vector containing Markdown report lines.
#' @export
phase10_api_compatibility_report <- function(compatibility) {
  validate_phase10_api_compatibility(compatibility, allow_breaking = TRUE)
  rows <- apply(compatibility$changes, 1L, function(x) {
    sprintf("- `%s`: **%s** — %s", x[["operation_id"]], x[["classification"]], x[["reason"]])
  })
  c(
    "# Phase 10 public API compatibility report",
    "",
    sprintf("Baseline API: `%s`", compatibility$baseline_api_version),
    sprintf("Candidate API: `%s`", compatibility$candidate_api_version),
    sprintf("Classification: **%s**", compatibility$classification),
    sprintf("Release compatible: `%s`", tolower(as.character(compatibility$release_compatible))),
    sprintf("Fingerprint: `%s`", compatibility$fingerprint),
    "",
    "## Operation changes",
    "",
    rows
  )
}

.phase10_compare_operation <- function(id, before, after) {
  if (!nrow(before)) {
    return(.phase10_change_row(id, "additive", "New public operation.", "added", "added", "added"))
  }
  if (!nrow(after)) {
    return(.phase10_change_row(id, "breaking", "Stable public operation removed.", "removed", "removed", "removed"))
  }

  request_change <- .phase10_schema_change(before$request_schema, after$request_schema)
  response_change <- .phase10_schema_change(before$response_schema, after$response_schema)
  lifecycle_change <- if (identical(before$lifecycle, after$lifecycle)) {
    "unchanged"
  } else {
    paste(before$lifecycle, after$lifecycle, sep = "->")
  }

  if (identical(before$lifecycle, "stable") && identical(after$lifecycle, "deprecated") &&
      !identical(request_change, "breaking") && !identical(response_change, "breaking")) {
    classification <- "deprecated"
    reason <- "Stable operation entered explicit deprecation lifecycle."
  } else if (identical(request_change, "breaking") || identical(response_change, "breaking") ||
             identical(before$lifecycle, "deprecated") && identical(after$lifecycle, "stable")) {
    classification <- "breaking"
    reason <- "Operation contract contains incompatible schema or lifecycle drift."
  } else if (request_change == "additive" || response_change == "additive") {
    classification <- "additive"
    reason <- "Operation schema advanced compatibly within its major version."
  } else {
    classification <- "compatible"
    reason <- "Operation contract is unchanged."
  }
  .phase10_change_row(id, classification, reason, request_change, response_change, lifecycle_change)
}

.phase10_schema_change <- function(before, after) {
  if (identical(before, after)) return("unchanged")
  old <- .phase10_schema_semver(before)
  new <- .phase10_schema_semver(after)
  if (!identical(old$name, new$name) || new$major != old$major ||
      new$minor < old$minor || (new$minor == old$minor && new$patch < old$patch)) {
    return("breaking")
  }
  "additive"
}

.phase10_schema_semver <- function(x) {
  match <- regexec("^(.+)/([0-9]+)\\.([0-9]+)\\.([0-9]+)$", x)
  parts <- regmatches(x, match)[[1L]]
  if (length(parts) != 5L) {
    stop(sprintf("Invalid public schema identifier: %s", x), call. = FALSE)
  }
  list(name = parts[[2L]], major = as.integer(parts[[3L]]),
       minor = as.integer(parts[[4L]]), patch = as.integer(parts[[5L]]))
}

.phase10_change_row <- function(id, classification, reason, request, response, lifecycle) {
  data.frame(
    operation_id = id,
    classification = classification,
    reason = reason,
    request_change = request,
    response_change = response,
    lifecycle_change = lifecycle,
    stringsAsFactors = FALSE
  )
}
