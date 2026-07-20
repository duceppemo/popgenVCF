# Phase 10.1 — canonical public analysis and artifact API

.phase10_api_version <- "1.0.0"
.phase10_internal_fields <- c(
  "executor", "scheduler", "worker", "retry", "checkpoint", "process",
  "migration", "deprecation", "runtime_state", "execution_handle"
)

.phase10_operations <- data.frame(
  operation_id = sort(c(
    "analysis.execute", "result.inspect", "artifact.list",
    "provenance.inspect", "report.render"
  )),
  lifecycle = "stable",
  request_schema = "1.0.0",
  response_schema = "1.0.0",
  stringsAsFactors = FALSE
)

phase10_api_descriptor <- function(api_version = .phase10_api_version) {
  .phase10_validate_semver(api_version, "api_version")
  if (!identical(api_version, .phase10_api_version)) {
    stop("Unsupported public API version.", call. = FALSE)
  }
  x <- list(
    record_type = "popgenvcf_public_api_descriptor",
    api_version = api_version,
    lifecycle = "stable",
    compatibility = list(minimum = "1.0.0", maximum = "1.x"),
    operations = .phase10_operations
  )
  x$fingerprint <- phase10_public_fingerprint(x)
  class(x) <- c("PopgenVCFPublicAPIDescriptor", "list")
  x
}

phase10_api_operations <- function(descriptor = phase10_api_descriptor()) {
  validate_phase10_api_descriptor(descriptor)
  descriptor$operations[order(descriptor$operations$operation_id), , drop = FALSE]
}

validate_phase10_api_descriptor <- function(x) {
  if (!inherits(x, "PopgenVCFPublicAPIDescriptor")) {
    stop("Invalid public API descriptor.", call. = FALSE)
  }
  .phase10_validate_semver(x$api_version, "api_version")
  if (!identical(x$api_version, .phase10_api_version)) {
    stop("Unsupported public API version.", call. = FALSE)
  }
  ops <- x$operations
  required <- c("operation_id", "lifecycle", "request_schema", "response_schema")
  if (!is.data.frame(ops) || !all(required %in% names(ops)) ||
      anyDuplicated(ops$operation_id) ||
      !identical(ops$operation_id, sort(ops$operation_id)) ||
      any(!ops$operation_id %in% .phase10_operations$operation_id) ||
      any(ops$lifecycle != "stable")) {
    stop("Malformed public API operation descriptor.", call. = FALSE)
  }
  .phase10_verify_fingerprint(x)
  TRUE
}

new_public_analysis_request <- function(operation_id, analysis_id,
                                        parameters = list(), input_ids = character(),
                                        api_version = .phase10_api_version) {
  descriptor <- phase10_api_descriptor(api_version)
  operations <- phase10_api_operations(descriptor)
  .phase10_scalar_string(operation_id, "operation_id")
  .phase10_scalar_string(analysis_id, "analysis_id")
  if (!operation_id %in% operations$operation_id) {
    stop("Unsupported public operation.", call. = FALSE)
  }
  parameters <- .phase10_canonical_named_list(parameters, "parameters")
  input_ids <- .phase10_canonical_named_character(input_ids, "input_ids")
  .phase10_reject_internal_fields(parameters)
  schema <- operations$request_schema[match(operation_id, operations$operation_id)]
  x <- list(
    record_type = "popgenvcf_public_api_request",
    api_version = api_version,
    schema_version = schema,
    operation_id = operation_id,
    analysis_id = analysis_id,
    parameters = parameters,
    input_ids = input_ids
  )
  x$fingerprint <- phase10_public_fingerprint(x)
  class(x) <- c("PopgenVCFPublicAPIRequest", "list")
  x
}

validate_public_analysis_request <- function(request) {
  if (!inherits(request, "PopgenVCFPublicAPIRequest")) {
    stop("Invalid public analysis request.", call. = FALSE)
  }
  descriptor <- phase10_api_descriptor(request$api_version)
  operations <- phase10_api_operations(descriptor)
  .phase10_scalar_string(request$operation_id, "operation_id")
  .phase10_scalar_string(request$analysis_id, "analysis_id")
  idx <- match(request$operation_id, operations$operation_id)
  if (is.na(idx)) stop("Unsupported public operation.", call. = FALSE)
  if (!identical(request$schema_version, operations$request_schema[idx])) {
    stop("Incompatible public request schema.", call. = FALSE)
  }
  .phase10_reject_internal_fields(request$parameters)
  .phase10_verify_fingerprint(request)
  TRUE
}

new_public_analysis_response <- function(request, status,
                                         scientific_values = list(),
                                         artifact_ids = character(),
                                         provenance_ids = character(),
                                         warnings = character(), error = NULL) {
  validate_public_analysis_request(request)
  status <- match.arg(status, c("completed", "failed", "rejected"))
  if (status != "completed" && is.null(error)) {
    stop("Failed or rejected responses require an error record.", call. = FALSE)
  }
  if (!is.null(error)) {
    if (!is.list(error) || is.null(error$code) || is.null(error$message)) {
      stop("error must contain code and message.", call. = FALSE)
    }
    error <- list(code = as.character(error$code), message = as.character(error$message))
  }
  scientific_values <- .phase10_canonical_named_list(scientific_values, "scientific_values")
  .phase10_reject_internal_fields(scientific_values)
  artifact_ids <- .phase10_canonical_named_character(artifact_ids, "artifact_ids")
  provenance_ids <- .phase10_canonical_named_character(provenance_ids, "provenance_ids")
  warnings <- sort(unique(as.character(warnings)))
  operations <- phase10_api_operations(phase10_api_descriptor(request$api_version))
  schema <- operations$response_schema[match(request$operation_id, operations$operation_id)]
  x <- list(
    record_type = "popgenvcf_public_api_response",
    api_version = request$api_version,
    schema_version = schema,
    operation_id = request$operation_id,
    analysis_id = request$analysis_id,
    request_fingerprint = request$fingerprint,
    status = status,
    scientific_values = scientific_values,
    artifact_ids = artifact_ids,
    provenance_ids = provenance_ids,
    warnings = warnings,
    error = error
  )
  x$fingerprint <- phase10_public_fingerprint(x)
  class(x) <- c("PopgenVCFPublicAPIResponse", "list")
  x
}

validate_public_analysis_response <- function(response, request = NULL) {
  if (!inherits(response, "PopgenVCFPublicAPIResponse")) {
    stop("Invalid public analysis response.", call. = FALSE)
  }
  descriptor <- phase10_api_descriptor(response$api_version)
  operations <- phase10_api_operations(descriptor)
  idx <- match(response$operation_id, operations$operation_id)
  if (is.na(idx) || !identical(response$schema_version, operations$response_schema[idx])) {
    stop("Incompatible public response schema.", call. = FALSE)
  }
  if (!response$status %in% c("completed", "failed", "rejected")) {
    stop("Unsupported public response status.", call. = FALSE)
  }
  if (response$status != "completed" && is.null(response$error)) {
    stop("Failed or rejected responses require an error record.", call. = FALSE)
  }
  if (!is.null(request)) {
    validate_public_analysis_request(request)
    if (!identical(response$request_fingerprint, request$fingerprint) ||
        !identical(response$analysis_id, request$analysis_id) ||
        !identical(response$operation_id, request$operation_id)) {
      stop("Public response does not belong to the supplied request.", call. = FALSE)
    }
  }
  .phase10_reject_internal_fields(response$scientific_values)
  .phase10_verify_fingerprint(response)
  TRUE
}

inspect_public_analysis_response <- function(response) {
  validate_public_analysis_response(response)
  data.frame(
    analysis_id = response$analysis_id,
    operation_id = response$operation_id,
    status = response$status,
    scientific_value_count = as.integer(length(response$scientific_values)),
    artifact_count = as.integer(length(response$artifact_ids)),
    provenance_count = as.integer(length(response$provenance_ids)),
    warning_count = as.integer(length(response$warnings)),
    fingerprint = response$fingerprint,
    stringsAsFactors = FALSE
  )
}

write_public_api_record <- function(x, path) {
  .phase10_validate_public_record(x)
  payload <- unclass(x)
  payload$.r_classes <- class(x)
  saveRDS(payload, path, version = 3)
  invisible(normalizePath(path, winslash = "/", mustWork = FALSE))
}

read_public_api_record <- function(path) {
  x <- readRDS(path)
  classes <- x$.r_classes
  x$.r_classes <- NULL
  if (!is.character(classes) || !length(classes)) {
    stop("Unsupported public API record type.", call. = FALSE)
  }
  class(x) <- classes
  .phase10_validate_public_record(x)
  x
}

phase10_public_fingerprint <- function(x) {
  payload <- unclass(x)
  payload$fingerprint <- NULL
  payload$.r_classes <- NULL
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
}

.phase10_scalar_string <- function(x, field) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop(sprintf("%s must be a non-empty scalar string.", field), call. = FALSE)
  }
}

.phase10_canonical_named_list <- function(x, field) {
  if (!is.list(x)) stop(sprintf("%s must be a list.", field), call. = FALSE)
  if (!length(x)) return(list())
  if (is.null(names(x)) || any(!nzchar(names(x))) || anyDuplicated(names(x))) {
    stop(sprintf("%s must have unique non-empty names.", field), call. = FALSE)
  }
  x[order(names(x))]
}

.phase10_canonical_named_character <- function(x, field) {
  if (!is.character(x)) stop(sprintf("%s must be character.", field), call. = FALSE)
  if (!length(x)) return(setNames(character(), character()))
  if (is.null(names(x)) || any(!nzchar(names(x))) || anyDuplicated(names(x)) || any(!nzchar(x))) {
    stop(sprintf("%s must have unique non-empty names and values.", field), call. = FALSE)
  }
  x[order(names(x))]
}

.phase10_reject_internal_fields <- function(x) {
  nms <- names(x)
  if (!is.null(nms) && any(nms %in% .phase10_internal_fields)) {
    stop("Internal runtime fields are not permitted in public API records.", call. = FALSE)
  }
  invisible(TRUE)
}
