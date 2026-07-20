# Phase 9 executable vertical-slice contracts
#
# These records define the deterministic boundary between the Phase 9 module
# contracts and the existing Phase 8 runtime. They intentionally do not
# implement a scheduler, executor, retry loop, or persistence layer.

.phase9_adapter_scalar_text <- function(x, field) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", field, "` must be one non-empty string.", call. = FALSE)
  }
  invisible(x)
}

.phase9_adapter_sha256 <- function(x) {
  digest::digest(x, algo = "sha256", serialize = TRUE)
}

.phase9_adapter_named_character <- function(x, field, allow_empty = FALSE) {
  if (!is.character(x) || anyNA(x) || any(!nzchar(x))) {
    stop("`", field, "` must contain non-empty strings.", call. = FALSE)
  }
  if (!length(x)) {
    if (allow_empty) {
      return(x)
    }
    stop("`", field, "` must not be empty.", call. = FALSE)
  }
  if (is.null(names(x)) || anyNA(names(x)) || any(!nzchar(names(x))) || anyDuplicated(names(x))) {
    stop("`", field, "` must be uniquely named.", call. = FALSE)
  }
  x[order(names(x), method = "radix")]
}

.phase9_adapter_canonical_list <- function(x, field) {
  if (!is.list(x)) {
    stop("`", field, "` must be a list.", call. = FALSE)
  }
  if (!length(x)) {
    return(x)
  }
  if (is.null(names(x)) || anyNA(names(x)) || any(!nzchar(names(x))) || anyDuplicated(names(x))) {
    stop("`", field, "` must be uniquely named.", call. = FALSE)
  }
  x[order(names(x), method = "radix")]
}

#' Construct a Phase 9 to Phase 8 runtime adapter record
#'
#' The adapter records one immutable mapping from a validated Phase 9
#' integration request and execution plan to a Phase 8 runtime plan. It does
#' not execute the plan itself.
#'
#' @param integration_request_id Phase 9 integration request identifier.
#' @param integration_request_fingerprint SHA-256 request fingerprint.
#' @param phase9_plan_id Phase 9 execution-plan identifier.
#' @param phase9_plan_fingerprint SHA-256 Phase 9 plan fingerprint.
#' @param phase8_plan_fingerprint SHA-256 fingerprint of the mapped Phase 8
#'   runtime plan.
#' @param module_mapping Named character vector mapping Phase 9 module IDs to
#'   Phase 8 runtime module IDs.
#' @param schema_resolutions Named character vector mapping declared schema
#'   roles to resolved schema identifiers.
#' @param input_fingerprints Named character vector of canonical scientific
#'   input fingerprints.
#' @param environment_fingerprint Runtime-environment fingerprint.
#' @param resource_fingerprint Resource-policy fingerprint.
#' @param adapter_version Adapter contract version.
#'
#' @return A validated `popgen_phase9_runtime_adapter` record.
#' @export
new_phase9_runtime_adapter <- function(
    integration_request_id,
    integration_request_fingerprint,
    phase9_plan_id,
    phase9_plan_fingerprint,
    phase8_plan_fingerprint,
    module_mapping,
    schema_resolutions,
    input_fingerprints,
    environment_fingerprint,
    resource_fingerprint,
    adapter_version = 1L) {
  module_mapping <- .phase9_adapter_named_character(module_mapping, "module_mapping")
  schema_resolutions <- .phase9_adapter_named_character(schema_resolutions, "schema_resolutions")
  input_fingerprints <- .phase9_adapter_named_character(input_fingerprints, "input_fingerprints")

  content <- list(
    adapter_version = adapter_version,
    integration_request_id = integration_request_id,
    integration_request_fingerprint = integration_request_fingerprint,
    phase9_plan_id = phase9_plan_id,
    phase9_plan_fingerprint = phase9_plan_fingerprint,
    phase8_plan_fingerprint = phase8_plan_fingerprint,
    module_mapping = module_mapping,
    schema_resolutions = schema_resolutions,
    input_fingerprints = input_fingerprints,
    environment_fingerprint = environment_fingerprint,
    resource_fingerprint = resource_fingerprint
  )

  adapter <- c(
    content,
    list(adapter_fingerprint = .phase9_adapter_sha256(content))
  )
  class(adapter) <- c("popgen_phase9_runtime_adapter", "list")
  validate_phase9_runtime_adapter(adapter)
  adapter
}

#' Validate a Phase 9 runtime adapter record
#'
#' @param x Candidate adapter record.
#' @return `x`, invisibly, when valid.
#' @export
validate_phase9_runtime_adapter <- function(x) {
  required <- c(
    "adapter_version", "integration_request_id",
    "integration_request_fingerprint", "phase9_plan_id",
    "phase9_plan_fingerprint", "phase8_plan_fingerprint", "module_mapping",
    "schema_resolutions", "input_fingerprints", "environment_fingerprint",
    "resource_fingerprint", "adapter_fingerprint"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Runtime adapter is missing fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!identical(x$adapter_version, 1L)) {
    stop("Unsupported Phase 9 runtime adapter version.", call. = FALSE)
  }

  scalar_fields <- c(
    "integration_request_id", "integration_request_fingerprint",
    "phase9_plan_id", "phase9_plan_fingerprint", "phase8_plan_fingerprint",
    "environment_fingerprint", "resource_fingerprint", "adapter_fingerprint"
  )
  for (field in scalar_fields) {
    .phase9_adapter_scalar_text(x[[field]], field)
  }
  for (field in c(
    "integration_request_fingerprint", "phase9_plan_fingerprint",
    "phase8_plan_fingerprint", "environment_fingerprint",
    "resource_fingerprint", "adapter_fingerprint"
  )) {
    if (!grepl("^[0-9a-f]{64}$", x[[field]])) {
      stop("`", field, "` must be a lower-case SHA-256 digest.", call. = FALSE)
    }
  }

  module_mapping <- .phase9_adapter_named_character(x$module_mapping, "module_mapping")
  schema_resolutions <- .phase9_adapter_named_character(x$schema_resolutions, "schema_resolutions")
  input_fingerprints <- .phase9_adapter_named_character(x$input_fingerprints, "input_fingerprints")
  if (!identical(module_mapping, x$module_mapping) ||
      !identical(schema_resolutions, x$schema_resolutions) ||
      !identical(input_fingerprints, x$input_fingerprints)) {
    stop("Runtime adapter mappings must use canonical name ordering.", call. = FALSE)
  }
  if (any(!grepl("^[0-9a-f]{64}$", input_fingerprints))) {
    stop("Every scientific input fingerprint must be a lower-case SHA-256 digest.", call. = FALSE)
  }

  content <- x[setdiff(required, "adapter_fingerprint")]
  expected <- .phase9_adapter_sha256(content)
  if (!identical(x$adapter_fingerprint, expected)) {
    stop("Runtime adapter content does not match its fingerprint.", call. = FALSE)
  }
  invisible(x)
}

#' Construct a deterministic Phase 9 cache decision
#'
#' @param adapter_fingerprint Runtime adapter fingerprint.
#' @param cache_key_fingerprint Cache-key fingerprint.
#' @param action One of `hit`, `miss`, `bypass`, or `reject`.
#' @param publish_intent Whether successful runtime output may be published to
#'   the cache.
#' @param manifest_fingerprint Existing cache-manifest fingerprint for a hit,
#'   otherwise `NA_character_`.
#' @param reason Stable machine-readable decision reason.
#'
#' @return A validated `popgen_phase9_cache_decision`.
#' @export
new_phase9_cache_decision <- function(
    adapter_fingerprint,
    cache_key_fingerprint,
    action,
    publish_intent = identical(action, "miss"),
    manifest_fingerprint = NA_character_,
    reason) {
  decision <- list(
    schema_version = 1L,
    adapter_fingerprint = adapter_fingerprint,
    cache_key_fingerprint = cache_key_fingerprint,
    action = action,
    publish_intent = publish_intent,
    manifest_fingerprint = manifest_fingerprint,
    reason = reason
  )
  decision$decision_fingerprint <- .phase9_adapter_sha256(decision)
  class(decision) <- c("popgen_phase9_cache_decision", "list")
  validate_phase9_cache_decision(decision)
  decision
}

#' Validate a deterministic Phase 9 cache decision
#'
#' @param x Candidate cache decision.
#' @return `x`, invisibly, when valid.
#' @export
validate_phase9_cache_decision <- function(x) {
  required <- c(
    "schema_version", "adapter_fingerprint", "cache_key_fingerprint",
    "action", "publish_intent", "manifest_fingerprint", "reason",
    "decision_fingerprint"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Cache decision is missing fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!identical(x$schema_version, 1L)) {
    stop("Unsupported Phase 9 cache-decision version.", call. = FALSE)
  }
  for (field in c("adapter_fingerprint", "cache_key_fingerprint", "action", "reason", "decision_fingerprint")) {
    .phase9_adapter_scalar_text(x[[field]], field)
  }
  if (!x$action %in% c("hit", "miss", "bypass", "reject")) {
    stop("Unsupported Phase 9 cache action.", call. = FALSE)
  }
  if (!is.logical(x$publish_intent) || length(x$publish_intent) != 1L || is.na(x$publish_intent)) {
    stop("`publish_intent` must be one non-missing logical value.", call. = FALSE)
  }
  if (identical(x$action, "hit") &&
      (!is.character(x$manifest_fingerprint) || length(x$manifest_fingerprint) != 1L ||
       is.na(x$manifest_fingerprint) || !grepl("^[0-9a-f]{64}$", x$manifest_fingerprint))) {
    stop("A cache hit requires one valid manifest fingerprint.", call. = FALSE)
  }
  if (!identical(x$action, "hit") && !is.na(x$manifest_fingerprint)) {
    stop("Only a cache hit may carry a manifest fingerprint.", call. = FALSE)
  }
  if (x$publish_intent && !identical(x$action, "miss")) {
    stop("Only a cache miss may request cache publication.", call. = FALSE)
  }
  for (field in c("adapter_fingerprint", "cache_key_fingerprint", "decision_fingerprint")) {
    if (!grepl("^[0-9a-f]{64}$", x[[field]])) {
      stop("`", field, "` must be a lower-case SHA-256 digest.", call. = FALSE)
    }
  }
  content <- x[setdiff(required, "decision_fingerprint")]
  if (!identical(x$decision_fingerprint, .phase9_adapter_sha256(content))) {
    stop("Cache decision content does not match its fingerprint.", call. = FALSE)
  }
  invisible(x)
}

#' Construct a Phase 9 executable vertical-slice record
#'
#' @param adapter_fingerprint Runtime adapter fingerprint.
#' @param cache_decision_fingerprint Cache-decision fingerprint.
#' @param runtime_status Terminal Phase 8 runtime status.
#' @param runtime_execution_fingerprint Phase 8 execution fingerprint.
#' @param result_fingerprint Canonical Phase 9 result fingerprint, or
#'   `NA_character_` for unsuccessful execution.
#' @param linked_records Named character vector of ledger, scheduler,
#'   checkpoint, recovery, provenance, validation, and publication identities.
#' @param diagnostics Named list of structured diagnostic identities.
#'
#' @return A validated `popgen_phase9_vertical_slice` record.
#' @export
new_phase9_vertical_slice <- function(
    adapter_fingerprint,
    cache_decision_fingerprint,
    runtime_status,
    runtime_execution_fingerprint,
    result_fingerprint = NA_character_,
    linked_records = character(),
    diagnostics = list()) {
  linked_records <- .phase9_adapter_named_character(
    linked_records,
    "linked_records",
    allow_empty = TRUE
  )
  diagnostics <- .phase9_adapter_canonical_list(diagnostics, "diagnostics")
  content <- list(
    schema_version = 1L,
    adapter_fingerprint = adapter_fingerprint,
    cache_decision_fingerprint = cache_decision_fingerprint,
    runtime_status = runtime_status,
    runtime_execution_fingerprint = runtime_execution_fingerprint,
    result_fingerprint = result_fingerprint,
    linked_records = linked_records,
    diagnostics = diagnostics
  )
  record <- c(content, list(vertical_slice_fingerprint = .phase9_adapter_sha256(content)))
  class(record) <- c("popgen_phase9_vertical_slice", "list")
  validate_phase9_vertical_slice(record)
  record
}

#' Validate a Phase 9 executable vertical-slice record
#'
#' @param x Candidate vertical-slice record.
#' @return `x`, invisibly, when valid.
#' @export
validate_phase9_vertical_slice <- function(x) {
  required <- c(
    "schema_version", "adapter_fingerprint", "cache_decision_fingerprint",
    "runtime_status", "runtime_execution_fingerprint", "result_fingerprint",
    "linked_records", "diagnostics", "vertical_slice_fingerprint"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Vertical-slice record is missing fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!identical(x$schema_version, 1L)) {
    stop("Unsupported Phase 9 vertical-slice version.", call. = FALSE)
  }
  for (field in c(
    "adapter_fingerprint", "cache_decision_fingerprint", "runtime_status",
    "runtime_execution_fingerprint", "vertical_slice_fingerprint"
  )) {
    .phase9_adapter_scalar_text(x[[field]], field)
  }
  if (!x$runtime_status %in% c("success", "failed", "blocked", "cancelled", "skipped")) {
    stop("Vertical-slice runtime status must be terminal.", call. = FALSE)
  }
  successful <- identical(x$runtime_status, "success")
  if (successful) {
    if (!is.character(x$result_fingerprint) || length(x$result_fingerprint) != 1L ||
        is.na(x$result_fingerprint) || !grepl("^[0-9a-f]{64}$", x$result_fingerprint)) {
      stop("Successful execution requires a canonical result fingerprint.", call. = FALSE)
    }
  } else if (!is.na(x$result_fingerprint)) {
    stop("Unsuccessful execution must not fabricate a result fingerprint.", call. = FALSE)
  }
  for (field in c(
    "adapter_fingerprint", "cache_decision_fingerprint",
    "runtime_execution_fingerprint", "vertical_slice_fingerprint"
  )) {
    if (!grepl("^[0-9a-f]{64}$", x[[field]])) {
      stop("`", field, "` must be a lower-case SHA-256 digest.", call. = FALSE)
    }
  }
  linked_records <- .phase9_adapter_named_character(
    x$linked_records,
    "linked_records",
    allow_empty = TRUE
  )
  if (!identical(linked_records, x$linked_records)) {
    stop("Linked runtime records must use canonical name ordering.", call. = FALSE)
  }
  if (length(linked_records) && any(!grepl("^[0-9a-f]{64}$", linked_records))) {
    stop("Linked runtime record identities must be SHA-256 digests.", call. = FALSE)
  }
  diagnostics <- .phase9_adapter_canonical_list(x$diagnostics, "diagnostics")
  if (!identical(diagnostics, x$diagnostics)) {
    stop("Diagnostics must use canonical name ordering.", call. = FALSE)
  }
  content <- x[setdiff(required, "vertical_slice_fingerprint")]
  if (!identical(x$vertical_slice_fingerprint, .phase9_adapter_sha256(content))) {
    stop("Vertical-slice content does not match its fingerprint.", call. = FALSE)
  }
  invisible(x)
}
