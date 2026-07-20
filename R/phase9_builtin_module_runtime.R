# Phase 9 first built-in module runtime integration
#
# These records describe the first deterministic built-in module execution
# through the existing Phase 8 runtime. They intentionally delegate execution,
# retry, cancellation, supervision, checkpointing, and persistence to Phase 8.

.phase9_builtin_scalar_text <- function(x, field) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", field, "` must be one non-empty string.", call. = FALSE)
  }
  invisible(x)
}

.phase9_builtin_sha256 <- function(x) {
  digest::digest(x, algo = "sha256", serialize = TRUE)
}

.phase9_builtin_named_character <- function(x, field, allow_empty = FALSE) {
  if (!is.character(x) || anyNA(x) || any(!nzchar(x))) {
    stop("`", field, "` must contain non-empty strings.", call. = FALSE)
  }
  if (!length(x)) {
    if (allow_empty) return(x)
    stop("`", field, "` must not be empty.", call. = FALSE)
  }
  if (is.null(names(x)) || anyNA(names(x)) || any(!nzchar(names(x))) || anyDuplicated(names(x))) {
    stop("`", field, "` must be uniquely named.", call. = FALSE)
  }
  x[order(names(x), method = "radix")]
}

.phase9_builtin_named_list <- function(x, field, allow_empty = FALSE) {
  if (!is.list(x)) {
    stop("`", field, "` must be a list.", call. = FALSE)
  }
  if (!length(x)) {
    if (allow_empty) return(x)
    stop("`", field, "` must not be empty.", call. = FALSE)
  }
  if (is.null(names(x)) || anyNA(names(x)) || any(!nzchar(names(x))) || anyDuplicated(names(x))) {
    stop("`", field, "` must be uniquely named.", call. = FALSE)
  }
  x[order(names(x), method = "radix")]
}

.phase9_builtin_sha_field <- function(x, field, allow_na = FALSE) {
  if (allow_na && is.character(x) && length(x) == 1L && is.na(x)) {
    return(invisible(x))
  }
  .phase9_builtin_scalar_text(x, field)
  if (!grepl("^[0-9a-f]{64}$", x)) {
    stop("`", field, "` must be a lower-case SHA-256 digest.", call. = FALSE)
  }
  invisible(x)
}

#' Construct a deterministic built-in Phase 9 module fixture
#'
#' @param module_id Stable built-in module identifier.
#' @param module_version Stable module implementation version.
#' @param plugin_fingerprint Plugin descriptor fingerprint.
#' @param schema_resolutions Named input/output schema fingerprints.
#' @param normalized_parameters Canonically named normalized parameters.
#' @param resource_fingerprint Resource-policy fingerprint.
#' @param environment_fingerprint Runtime-environment fingerprint.
#' @param implementation_fingerprint Built-in implementation fingerprint.
#'
#' @return A validated `popgen_phase9_builtin_fixture` record.
#' @export
new_phase9_builtin_fixture <- function(
    module_id,
    module_version,
    plugin_fingerprint,
    schema_resolutions,
    normalized_parameters,
    resource_fingerprint,
    environment_fingerprint,
    implementation_fingerprint) {
  schema_resolutions <- .phase9_builtin_named_character(schema_resolutions, "schema_resolutions")
  normalized_parameters <- .phase9_builtin_named_list(
    normalized_parameters,
    "normalized_parameters",
    allow_empty = TRUE
  )

  content <- list(
    schema_version = 1L,
    module_id = module_id,
    module_version = module_version,
    plugin_fingerprint = plugin_fingerprint,
    schema_resolutions = schema_resolutions,
    normalized_parameters = normalized_parameters,
    resource_fingerprint = resource_fingerprint,
    environment_fingerprint = environment_fingerprint,
    implementation_fingerprint = implementation_fingerprint
  )
  fixture <- c(content, list(fixture_fingerprint = .phase9_builtin_sha256(content)))
  class(fixture) <- c("popgen_phase9_builtin_fixture", "list")
  validate_phase9_builtin_fixture(fixture)
  fixture
}

#' Validate a deterministic built-in Phase 9 module fixture
#'
#' @param x Candidate fixture.
#' @return `x`, invisibly, when valid.
#' @export
validate_phase9_builtin_fixture <- function(x) {
  required <- c(
    "schema_version", "module_id", "module_version", "plugin_fingerprint",
    "schema_resolutions", "normalized_parameters", "resource_fingerprint",
    "environment_fingerprint", "implementation_fingerprint", "fixture_fingerprint"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Built-in fixture is missing fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!identical(x$schema_version, 1L)) {
    stop("Unsupported built-in fixture version.", call. = FALSE)
  }
  for (field in c("module_id", "module_version")) {
    .phase9_builtin_scalar_text(x[[field]], field)
  }
  for (field in c(
    "plugin_fingerprint", "resource_fingerprint", "environment_fingerprint",
    "implementation_fingerprint", "fixture_fingerprint"
  )) {
    .phase9_builtin_sha_field(x[[field]], field)
  }
  schemas <- .phase9_builtin_named_character(x$schema_resolutions, "schema_resolutions")
  parameters <- .phase9_builtin_named_list(x$normalized_parameters, "normalized_parameters", allow_empty = TRUE)
  if (!identical(schemas, x$schema_resolutions) || !identical(parameters, x$normalized_parameters)) {
    stop("Fixture schemas and parameters must use canonical name ordering.", call. = FALSE)
  }
  if (any(!grepl("^[0-9a-f]{64}$", schemas))) {
    stop("Every resolved schema identity must be a lower-case SHA-256 digest.", call. = FALSE)
  }
  content <- x[setdiff(required, "fixture_fingerprint")]
  if (!identical(x$fixture_fingerprint, .phase9_builtin_sha256(content))) {
    stop("Built-in fixture content does not match its fingerprint.", call. = FALSE)
  }
  invisible(x)
}

#' Construct a first built-in Phase 9 runtime run record
#'
#' @param fixture_fingerprint Built-in fixture fingerprint.
#' @param integration_request_fingerprint Phase 9 integration-request fingerprint.
#' @param phase9_plan_fingerprint Phase 9 execution-plan fingerprint.
#' @param adapter_fingerprint Phase 9-to-Phase 8 adapter fingerprint.
#' @param cache_decision_fingerprint Cache-decision fingerprint.
#' @param phase8_execution_fingerprint Authoritative Phase 8 execution fingerprint.
#' @param runtime_status Terminal Phase 8 runtime state.
#' @param result_fingerprint Canonical Phase 9 result fingerprint, or `NA`.
#' @param linked_records Named runtime, recovery, provenance, validation, and publication identities.
#' @param failure_stage Stable failure stage, or `NA` for success.
#' @param failure_code Stable failure code, or `NA` for success.
#'
#' @return A validated `popgen_phase9_builtin_run` record.
#' @export
new_phase9_builtin_run <- function(
    fixture_fingerprint,
    integration_request_fingerprint,
    phase9_plan_fingerprint,
    adapter_fingerprint,
    cache_decision_fingerprint,
    phase8_execution_fingerprint,
    runtime_status,
    result_fingerprint = NA_character_,
    linked_records = character(),
    failure_stage = NA_character_,
    failure_code = NA_character_) {
  linked_records <- .phase9_builtin_named_character(
    linked_records,
    "linked_records",
    allow_empty = TRUE
  )
  content <- list(
    schema_version = 1L,
    fixture_fingerprint = fixture_fingerprint,
    integration_request_fingerprint = integration_request_fingerprint,
    phase9_plan_fingerprint = phase9_plan_fingerprint,
    adapter_fingerprint = adapter_fingerprint,
    cache_decision_fingerprint = cache_decision_fingerprint,
    phase8_execution_fingerprint = phase8_execution_fingerprint,
    runtime_status = runtime_status,
    result_fingerprint = result_fingerprint,
    linked_records = linked_records,
    failure_stage = failure_stage,
    failure_code = failure_code
  )
  run <- c(content, list(run_fingerprint = .phase9_builtin_sha256(content)))
  class(run) <- c("popgen_phase9_builtin_run", "list")
  validate_phase9_builtin_run(run)
  run
}

#' Validate a first built-in Phase 9 runtime run record
#'
#' @param x Candidate run record.
#' @return `x`, invisibly, when valid.
#' @export
validate_phase9_builtin_run <- function(x) {
  required <- c(
    "schema_version", "fixture_fingerprint", "integration_request_fingerprint",
    "phase9_plan_fingerprint", "adapter_fingerprint", "cache_decision_fingerprint",
    "phase8_execution_fingerprint", "runtime_status", "result_fingerprint",
    "linked_records", "failure_stage", "failure_code", "run_fingerprint"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Built-in run is missing fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!identical(x$schema_version, 1L)) {
    stop("Unsupported built-in run version.", call. = FALSE)
  }
  for (field in c(
    "fixture_fingerprint", "integration_request_fingerprint", "phase9_plan_fingerprint",
    "adapter_fingerprint", "cache_decision_fingerprint", "phase8_execution_fingerprint",
    "run_fingerprint"
  )) {
    .phase9_builtin_sha_field(x[[field]], field)
  }
  .phase9_builtin_sha_field(x$result_fingerprint, "result_fingerprint", allow_na = TRUE)
  .phase9_builtin_scalar_text(x$runtime_status, "runtime_status")
  terminal <- c("succeeded", "failed", "cancelled", "rejected", "cached")
  if (!x$runtime_status %in% terminal) {
    stop("Built-in runs require a terminal runtime status.", call. = FALSE)
  }
  linked <- .phase9_builtin_named_character(x$linked_records, "linked_records", allow_empty = TRUE)
  if (!identical(linked, x$linked_records)) {
    stop("Linked records must use canonical name ordering.", call. = FALSE)
  }
  success <- x$runtime_status %in% c("succeeded", "cached")
  if (success && is.na(x$result_fingerprint)) {
    stop("Successful or cached execution requires a canonical result fingerprint.", call. = FALSE)
  }
  if (!success && !is.na(x$result_fingerprint)) {
    stop("Unsuccessful execution must not carry a successful result fingerprint.", call. = FALSE)
  }
  if (success) {
    if (!is.na(x$failure_stage) || !is.na(x$failure_code)) {
      stop("Successful execution must not carry failure metadata.", call. = FALSE)
    }
  } else {
    for (field in c("failure_stage", "failure_code")) {
      .phase9_builtin_scalar_text(x[[field]], field)
    }
    allowed_stages <- c(
      "preflight", "schema_resolution", "planning", "cache_lookup", "dispatch",
      "runtime", "result_validation", "cache_publication", "recovery"
    )
    if (!x$failure_stage %in% allowed_stages) {
      stop("Unsupported built-in failure stage.", call. = FALSE)
    }
  }
  content <- x[setdiff(required, "run_fingerprint")]
  if (!identical(x$run_fingerprint, .phase9_builtin_sha256(content))) {
    stop("Built-in run content does not match its fingerprint.", call. = FALSE)
  }
  invisible(x)
}

#' Construct deterministic equivalence verification for built-in executions
#'
#' @param reference_run_fingerprint Reference run fingerprint.
#' @param candidate_run_fingerprint Candidate run fingerprint.
#' @param reference_result_fingerprint Reference result fingerprint.
#' @param candidate_result_fingerprint Candidate result fingerprint.
#' @param execution_mode One of `replay`, `checkpoint_resume`, or `cache_hit`.
#' @param equivalent Whether the canonical scientific results are equivalent.
#' @param reason Stable machine-readable verification reason.
#'
#' @return A validated `popgen_phase9_builtin_verification` record.
#' @export
new_phase9_builtin_verification <- function(
    reference_run_fingerprint,
    candidate_run_fingerprint,
    reference_result_fingerprint,
    candidate_result_fingerprint,
    execution_mode,
    equivalent,
    reason) {
  content <- list(
    schema_version = 1L,
    reference_run_fingerprint = reference_run_fingerprint,
    candidate_run_fingerprint = candidate_run_fingerprint,
    reference_result_fingerprint = reference_result_fingerprint,
    candidate_result_fingerprint = candidate_result_fingerprint,
    execution_mode = execution_mode,
    equivalent = equivalent,
    reason = reason
  )
  verification <- c(content, list(verification_fingerprint = .phase9_builtin_sha256(content)))
  class(verification) <- c("popgen_phase9_builtin_verification", "list")
  validate_phase9_builtin_verification(verification)
  verification
}

#' Validate built-in execution equivalence verification
#'
#' @param x Candidate verification record.
#' @return `x`, invisibly, when valid.
#' @export
validate_phase9_builtin_verification <- function(x) {
  required <- c(
    "schema_version", "reference_run_fingerprint", "candidate_run_fingerprint",
    "reference_result_fingerprint", "candidate_result_fingerprint", "execution_mode",
    "equivalent", "reason", "verification_fingerprint"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Built-in verification is missing fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!identical(x$schema_version, 1L)) {
    stop("Unsupported built-in verification version.", call. = FALSE)
  }
  for (field in c(
    "reference_run_fingerprint", "candidate_run_fingerprint",
    "reference_result_fingerprint", "candidate_result_fingerprint",
    "verification_fingerprint"
  )) {
    .phase9_builtin_sha_field(x[[field]], field)
  }
  .phase9_builtin_scalar_text(x$execution_mode, "execution_mode")
  .phase9_builtin_scalar_text(x$reason, "reason")
  if (!x$execution_mode %in% c("replay", "checkpoint_resume", "cache_hit")) {
    stop("Unsupported built-in verification mode.", call. = FALSE)
  }
  if (!is.logical(x$equivalent) || length(x$equivalent) != 1L || is.na(x$equivalent)) {
    stop("`equivalent` must be one non-missing logical value.", call. = FALSE)
  }
  identities_match <- identical(x$reference_result_fingerprint, x$candidate_result_fingerprint)
  if (!identical(x$equivalent, identities_match)) {
    stop("Equivalence must agree with canonical result identity.", call. = FALSE)
  }
  content <- x[setdiff(required, "verification_fingerprint")]
  if (!identical(x$verification_fingerprint, .phase9_builtin_sha256(content))) {
    stop("Built-in verification content does not match its fingerprint.", call. = FALSE)
  }
  invisible(x)
}
