# Phase 10.1 — canonical public analysis and artifact API

.phase10_public_operations <- data.frame(
  operation_id = c(
    "analysis.execute",
    "result.inspect",
    "artifact.list",
    "provenance.inspect",
    "report.render"
  ),
  request_schema = c(
    "popgenvcf.public.analysis-request/1.0.0",
    "popgenvcf.public.result-inspection-request/1.0.0",
    "popgenvcf.public.artifact-list-request/1.0.0",
    "popgenvcf.public.provenance-inspection-request/1.0.0",
    "popgenvcf.public.report-render-request/1.0.0"
  ),
  response_schema = c(
    "popgenvcf.public.analysis-response/1.0.0",
    "popgenvcf.public.result-inspection-response/1.0.0",
    "popgenvcf.public.artifact-list-response/1.0.0",
    "popgenvcf.public.provenance-inspection-response/1.0.0",
    "popgenvcf.public.report-render-response/1.0.0"
  ),
  lifecycle = rep("stable", 5L),
  stringsAsFactors = FALSE
)

.phase10_internal_fields <- c(
  "executor", "scheduler", "worker", "process_handle", "retry_state",
  "checkpoint_payload", "migration_record", "deprecation_record"
)

#' Describe the stable popgenVCF public API
#'
#' @param api_version Public API semantic version.
#' @return A deterministic public API descriptor.
#' @export
phase10_api_descriptor <- function(api_version = "1.0.0") {
  .phase10_validate_semver(api_version, "api_version")

  descriptor <- list(
    record_type = "popgenvcf_public_api_descriptor",
    schema_version = "1.0.0",
    api_version = api_version,
    lifecycle = "stable",
    operations = .phase10_public_operations,
    compatibility = list(
      minimum_supported = "1.0.0",
      maximum_supported_major = 1L,
      future_major = "reject",
      unknown_operation = "reject"
    )
  )
  descriptor$fingerprint <- phase10_public_fingerprint(descriptor)
  class(descriptor) <- c("PopgenVCFPublicAPIDescriptor", "list")
  descriptor
}

#' List stable public API operations
#'
#' @param descriptor A public API descriptor.
#' @return A data frame ordered by operation identifier.
#' @export
phase10_api_operations <- function(descriptor = phase10_api_descriptor()) {
  validate_phase10_api_descriptor(descriptor)
  descriptor$operations[order(descriptor$operations$operation_id), , drop = FALSE]
}

#' Create a canonical public API request
#'
#' @param operation_id Stable operation identifier.
#' @param analysis_id Stable analysis or request identity.
#' @param parameters Named public parameters.
#' @param input_ids Named immutable input identities.
#' @param api_version Requested public API version.
#' @return A validated request envelope.
#' @export
new_public_analysis_request <- function(
    operation_id,
    analysis_id,
    parameters = list(),
    input_ids = character(),
    api_version = "1.0.0") {
  descriptor <- phase10_api_descriptor(api_version)
  operations <- phase10_api_operations(descriptor)
  .phase10_scalar_string(operation_id, "operation_id")
  .phase10_scalar_string(analysis_id, "analysis_id")

  if (!operation_id %in% operations$operation_id) {
    stop(sprintf("Unsupported public operation: %s", operation_id), call. = FALSE)
  }
  parameters <- .phase10_canonical_named_list(parameters, "parameters")
  input_ids <- .phase10_canonical_named_character(input_ids, "input_ids")
  .phase10_reject_internal_fields(parameters)

  request_schema <- operations$request_schema[
    match(operation_id, operations$operation_id)
  ]
  request <- list(
    record_type = "popgenvcf_public_api_request",
    schema_version = request_schema,
    api_version = api_version,
    operation_id = operation_id,
    analysis_id = analysis_id,
    parameters = parameters,
    input_ids = input_ids
  )
  request$fingerprint <- phase10_public_fingerprint(request)
  class(request) <- c("PopgenVCFPublicAPIRequest", "list")
  request
}

#' Create a canonical public API response
#'
#' @param request A validated public API request.
#' @param status One of `completed`, `failed`, `cancelled`, `rejected`, or `cached`.
#' @param scientific_values Named scientific result values or immutable references.
#' @param artifact_ids Named artifact identities.
#' @param provenance_ids Named provenance identities.
#' @param warnings Character warnings.
#' @param error Optional stable public error record.
#' @return A validated response envelope.
#' @export
new_public_analysis_response <- function(
    request,
    status,
    scientific_values = list(),
    artifact_ids = character(),
    provenance_ids = character(),
    warnings = character(),
    error = NULL) {
  validate_public_analysis_request(request)
  statuses <- c("completed", "failed", "cancelled", "rejected", "cached")
  .phase10_scalar_string(status, "status")
  if (!status %in% statuses) {
    stop("Unsupported public response status.", call. = FALSE)
  }

  scientific_values <- .phase10_canonical_named_list(
    scientific_values, "scientific_values"
  )
  artifact_ids <- .phase10_canonical_named_character(
    artifact_ids, "artifact_ids"
  )
  provenance_ids <- .phase10_canonical_named_character(
    provenance_ids, "provenance_ids"
  )
  warnings <- sort(unique(as.character(warnings)))
  .phase10_reject_internal_fields(scientific_values)

  if (status %in% c("completed", "cached") && !is.null(error)) {
    stop("Successful public responses cannot contain an error.", call. = FALSE)
  }
  if (status %in% c("failed", "rejected") && is.null(error)) {
    stop("Failed or rejected public responses require an error record.", call. = FALSE)
  }
  error <- .phase10_public_error(error)

  operations <- phase10_api_operations(phase10_api_descriptor(request$api_version))
  response_schema <- operations$response_schema[
    match(request$operation_id, operations$operation_id)
  ]
  response <- list(
    record_type = "popgenvcf_public_api_response",
    schema_version = response_schema,
    api_version = request$api_version,
    operation_id = request$operation_id,
    request_fingerprint = request$fingerprint,
    analysis_id = request$analysis_id,
    status = status,
    scientific_values = scientific_values,
    artifact_ids = artifact_ids,
    provenance_ids = provenance_ids,
    warnings = warnings,
    error = error
  )
  response$fingerprint <- phase10_public_fingerprint(response)
  class(response) <- c("PopgenVCFPublicAPIResponse", "list")
  response
}

#' Validate a Phase 10 public API descriptor
#' @param descriptor A descriptor object.
#' @return `TRUE`, invisibly.
#' @export
validate_phase10_api_descriptor <- function(descriptor) {
  if (!inherits(descriptor, "PopgenVCFPublicAPIDescriptor")) {
    stop("descriptor must be a public API descriptor.", call. = FALSE)
  }
  .phase10_validate_semver(descriptor$api_version, "api_version")
  operations <- descriptor$operations
  required <- c(
    "operation_id", "request_schema", "response_schema", "lifecycle"
  )
  if (!is.data.frame(operations) || !identical(names(operations), required)) {
    stop("Malformed public operation registry.", call. = FALSE)
  }
  if (anyDuplicated(operations$operation_id) ||
      any(!operations$lifecycle %in% c("stable", "deprecated"))) {
    stop("Invalid or duplicate public operations.", call. = FALSE)
  }
  .phase10_verify_fingerprint(descriptor)
  invisible(TRUE)
}

#' Validate a public API request
#' @param request A request envelope.
#' @return `TRUE`, invisibly.
#' @export
validate_public_analysis_request <- function(request) {
  if (!inherits(request, "PopgenVCFPublicAPIRequest")) {
    stop("request must be a public API request.", call. = FALSE)
  }
  descriptor <- phase10_api_descriptor(request$api_version)
  operations <- phase10_api_operations(descriptor)
  if (!request$operation_id %in% operations$operation_id) {
    stop("Unknown public operation.", call. = FALSE)
  }
  expected_schema <- operations$request_schema[
    match(request$operation_id, operations$operation_id)
  ]
  if (!identical(request$schema_version, expected_schema)) {
    stop("Incompatible public request schema.", call. = FALSE)
  }
  .phase10_reject_internal_fields(request$parameters)
  .phase10_verify_fingerprint(request)
  invisible(TRUE)
}

#' Validate a public API response
#' @param response A response envelope.
#' @param request Optional originating request.
#' @return `TRUE`, invisibly.
#' @export
validate_public_analysis_response <- function(response, request = NULL) {
  if (!inherits(response, "PopgenVCFPublicAPIResponse")) {
    stop("response must be a public API response.", call. = FALSE)
  }
  descriptor <- phase10_api_descriptor(response$api_version)
  operations <- phase10_api_operations(descriptor)
  expected_schema <- operations$response_schema[
    match(response$operation_id, operations$operation_id)
  ]
  if (length(expected_schema) != 1L || is.na(expected_schema) ||
      !identical(response$schema_version, expected_schema)) {
    stop("Incompatible public response schema.", call. = FALSE)
  }
  if (!is.null(request)) {
    validate_public_analysis_request(request)
    if (!identical(response$request_fingerprint, request$fingerprint) ||
        !identical(response$analysis_id, request$analysis_id)) {
      stop("Public response does not belong to the supplied request.", call. = FALSE)
    }
  }
  .phase10_reject_internal_fields(response$scientific_values)
  .phase10_verify_fingerprint(response)
  invisible(TRUE)
}

#' Inspect a public API response without exposing runtime internals
#'
#' @param response A validated response.
#' @return A stable one-row summary data frame.
#' @export
inspect_public_analysis_response <- function(response) {
  validate_public_analysis_response(response)
  data.frame(
    analysis_id = response$analysis_id,
    operation_id = response$operation_id,
    status = response$status,
    scientific_value_count = length(response$scientific_values),
    artifact_count = length(response$artifact_ids),
    provenance_count = length(response$provenance_ids),
    warning_count = length(response$warnings),
    fingerprint = response$fingerprint,
    stringsAsFactors = FALSE
  )
}

#' Write a deterministic public API record
#'
#' @param x A public API descriptor, request, or response.
#' @param path Output JSON path.
#' @return The normalized path, invisibly.
#' @noRd
write_public_api_record <- function(x, path) {
  .phase10_validate_public_record(x)
  json <- jsonlite::toJSON(
    unclass(x), auto_unbox = TRUE, null = "null", digits = NA,
    pretty = TRUE, dataframe = "rows"
  )
  writeLines(json, path, useBytes = TRUE)
  invisible(normalizePath(path, winslash = "/", mustWork = FALSE))
}

#' Read and validate a deterministic public API record
#'
#' @param path Input JSON path.
#' @return A validated public API object.
#' @noRd
read_public_api_record <- function(path) {
  x <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  class_name <- switch(
    x$record_type,
    popgenvcf_public_api_descriptor = "PopgenVCFPublicAPIDescriptor",
    popgenvcf_public_api_request = "PopgenVCFPublicAPIRequest",
    popgenvcf_public_api_response = "PopgenVCFPublicAPIResponse",
    stop("Unsupported public API record type.", call. = FALSE)
  )
  class(x) <- c(class_name, "list")
  .phase10_validate_public_record(x)
  x
}

#' Compute a deterministic SHA-256 fingerprint for a public record
#' @param x Record content.
#' @return A lowercase SHA-256 digest.
#' @export
phase10_public_fingerprint <- function(x) {
  payload <- unclass(x)
  payload$fingerprint <- NULL
  raw <- serialize(payload, NULL, version = 3L)
  as.character(openssl::sha256(raw))
}

.phase10_validate_public_record <- function(x) {
  if (inherits(x, "PopgenVCFPublicAPIDescriptor")) {
    validate_phase10_api_descriptor(x)
  } else if (inherits(x, "PopgenVCFPublicAPIRequest")) {
    validate_public_analysis_request(x)
  } else if (inherits(x, "PopgenVCFPublicAPIResponse")) {
    validate_public_analysis_response(x)
  } else {
    stop("Unsupported public API record.", call. = FALSE)
  }
}

.phase10_verify_fingerprint <- function(x) {
  if (!is.character(x$fingerprint) || length(x$fingerprint) != 1L ||
      !grepl("^[0-9a-f]{64}$", x$fingerprint) ||
      !identical(x$fingerprint, phase10_public_fingerprint(x))) {
    stop("Public API record fingerprint verification failed.", call. = FALSE)
  }
}

.phase10_validate_semver <- function(x, field) {
  .phase10_scalar_string(x, field)
  if (!grepl("^[0-9]+\\.[0-9]+\\.[0-9]+$", x)) {
    stop(sprintf("%s must be a semantic version.", field), call. = FALSE)
  }
  major <- as.integer(strsplit(x, ".", fixed = TRUE)[[1L]][1L])
  if (major != 1L) {
    stop("Unsupported public API major version.", call. = FALSE)
  }
}

.phase10_scalar_string <- function(x, field) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop(sprintf("%s must be one non-empty string.", field), call. = FALSE)
  }
}

.phase10_canonical_named_list <- function(x, field) {
  if (!is.list(x)) {
    stop(sprintf("%s must be a named list.", field), call. = FALSE)
  }
  if (length(x) == 0L) return(x)
  if (is.null(names(x)) || any(!nzchar(names(x))) || anyDuplicated(names(x))) {
    stop(sprintf("%s must have unique non-empty names.", field), call. = FALSE)
  }
  x[order(names(x))]
}

.phase10_canonical_named_character <- function(x, field) {
  if (length(x) == 0L) return(character())
  if (!is.character(x) || is.null(names(x)) || any(!nzchar(names(x))) ||
      anyDuplicated(names(x)) || any(!nzchar(x))) {
    stop(sprintf("%s must be named, unique, non-empty identities.", field),
         call. = FALSE)
  }
  x[order(names(x))]
}

.phase10_reject_internal_fields <- function(x) {
  fields <- names(x)
  if (length(fields) && any(fields %in% .phase10_internal_fields)) {
    stop("Internal runtime fields are not part of the public API.", call. = FALSE)
  }
}

.phase10_public_error <- function(error) {
  if (is.null(error)) return(NULL)
  if (!is.list(error) || !identical(sort(names(error)), c("code", "message"))) {
    stop("error must contain exactly code and message.", call. = FALSE)
  }
  .phase10_scalar_string(error$code, "error code")
  .phase10_scalar_string(error$message, "error message")
  list(code = error$code, message = error$message)
}
