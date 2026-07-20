# Phase 9.13 — legacy runtime deprecation and unified release readiness

.phase9_deprecation_stages <- c(
  "warn_only",
  "unified_opt_in",
  "unified_default",
  "legacy_disabled",
  "retired"
)

phase9_legacy_runtime_deprecation <- function(
    module_id,
    migration_registry_id,
    legacy_entrypoint_id,
    unified_entrypoint_id,
    stage,
    replacement_verification_id,
    rollback_identity = NULL,
    compatibility_notice_id = NULL,
    effective_at = NULL,
    fingerprint = NULL) {
  stopifnot(
    is.character(module_id), length(module_id) == 1L, nzchar(module_id),
    is.character(migration_registry_id), length(migration_registry_id) == 1L,
    is.character(legacy_entrypoint_id), length(legacy_entrypoint_id) == 1L,
    is.character(unified_entrypoint_id), length(unified_entrypoint_id) == 1L,
    is.character(stage), length(stage) == 1L,
    stage %in% .phase9_deprecation_stages,
    is.character(replacement_verification_id),
    length(replacement_verification_id) == 1L,
    nzchar(replacement_verification_id)
  )

  if (stage %in% c("legacy_disabled", "retired") &&
      (is.null(rollback_identity) || !nzchar(rollback_identity))) {
    stop("A validated rollback identity is required before legacy disablement.",
         call. = FALSE)
  }

  record <- list(
    record_type = "phase9_legacy_runtime_deprecation",
    schema_version = "1.0.0",
    module_id = module_id,
    migration_registry_id = migration_registry_id,
    legacy_entrypoint_id = legacy_entrypoint_id,
    unified_entrypoint_id = unified_entrypoint_id,
    stage = stage,
    replacement_verification_id = replacement_verification_id,
    rollback_identity = rollback_identity,
    compatibility_notice_id = compatibility_notice_id,
    effective_at = effective_at
  )

  record$fingerprint <- fingerprint %||%
    phase9_deprecation_fingerprint(record)
  class(record) <- c("phase9_legacy_runtime_deprecation", "list")
  record
}

phase9_release_readiness <- function(
    migration_registry_id,
    required_module_ids,
    ready_module_ids,
    blocked_module_ids = character(),
    schema_evidence_id,
    runtime_evidence_id,
    cache_evidence_id,
    recovery_evidence_id,
    provenance_evidence_id,
    publication_evidence_id,
    validation_evidence_id,
    performance_evidence_id,
    documentation_evidence_id,
    release_ready = FALSE,
    fingerprint = NULL) {
  required_module_ids <- sort(unique(required_module_ids))
  ready_module_ids <- sort(unique(ready_module_ids))
  blocked_module_ids <- sort(unique(blocked_module_ids))

  missing_ready <- setdiff(required_module_ids, ready_module_ids)
  false_claim <- isTRUE(release_ready) &&
    (length(missing_ready) > 0L || length(blocked_module_ids) > 0L)
  if (false_claim) {
    stop("Release readiness cannot pass with missing or blocked modules.",
         call. = FALSE)
  }

  evidence <- c(
    schema_evidence_id,
    runtime_evidence_id,
    cache_evidence_id,
    recovery_evidence_id,
    provenance_evidence_id,
    publication_evidence_id,
    validation_evidence_id,
    performance_evidence_id,
    documentation_evidence_id
  )
  if (any(!nzchar(evidence))) {
    stop("All release-readiness evidence identities are required.",
         call. = FALSE)
  }

  record <- list(
    record_type = "phase9_release_readiness",
    schema_version = "1.0.0",
    migration_registry_id = migration_registry_id,
    required_module_ids = required_module_ids,
    ready_module_ids = ready_module_ids,
    blocked_module_ids = blocked_module_ids,
    missing_module_ids = missing_ready,
    evidence_ids = sort(evidence),
    release_ready = isTRUE(release_ready)
  )

  record$fingerprint <- fingerprint %||%
    phase9_deprecation_fingerprint(record)
  class(record) <- c("phase9_release_readiness", "list")
  record
}

phase9_validate_deprecation_transition <- function(from, to) {
  transitions <- list(
    warn_only = c("unified_opt_in"),
    unified_opt_in = c("unified_default", "warn_only"),
    unified_default = c("legacy_disabled", "unified_opt_in"),
    legacy_disabled = c("retired", "unified_default"),
    retired = character()
  )

  if (!from %in% names(transitions) || !to %in% .phase9_deprecation_stages) {
    stop("Unsupported deprecation stage.", call. = FALSE)
  }
  if (!to %in% transitions[[from]]) {
    stop(sprintf("Invalid deprecation transition: %s -> %s", from, to),
         call. = FALSE)
  }
  invisible(TRUE)
}

phase9_deprecation_fingerprint <- function(x) {
  payload <- x
  payload$fingerprint <- NULL
  raw <- serialize(payload, NULL, version = 3L)
  if (requireNamespace("openssl", quietly = TRUE)) {
    return(as.character(openssl::sha256(raw)))
  }
  stop("Package 'openssl' is required for deterministic SHA-256 fingerprints.",
       call. = FALSE)
}

`%||%` <- function(x, y) if (is.null(x)) y else x
