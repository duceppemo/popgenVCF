# Phase 9 production-module migration registry
#
# These records generalize the first production-module migration into a
# deterministic portfolio registry and staged cutover model. They do not
# execute modules or replace the authoritative Phase 8 runtime.

.phase9_migration_registry_sha256 <- function(x) {
  digest::digest(x, algo = "sha256", serialize = TRUE)
}

.phase9_migration_registry_scalar <- function(x, field) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", field, "` must be one non-empty string.", call. = FALSE)
  }
  invisible(x)
}

.phase9_migration_registry_named <- function(x, field, allow_empty = FALSE) {
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

.phase9_migration_states <- c(
  "discovered", "planned", "shadow", "dual-run", "verified",
  "default-unified", "legacy-disabled", "retired"
)

.phase9_migration_transitions <- list(
  discovered = c("planned"),
  planned = c("shadow"),
  shadow = c("dual-run", "planned"),
  `dual-run` = c("verified", "shadow"),
  verified = c("default-unified", "dual-run"),
  `default-unified` = c("legacy-disabled", "verified"),
  `legacy-disabled` = c("retired", "default-unified"),
  retired = character()
)

#' Construct a production-module migration registry entry
#'
#' @param module_id Stable production-module identifier.
#' @param descriptor_fingerprint Phase 9 plugin-descriptor fingerprint.
#' @param legacy_entrypoint Legacy public entrypoint identity.
#' @param unified_entrypoint Unified Phase 8/Phase 9 entrypoint identity.
#' @param input_schema_fingerprint Canonical input-schema fingerprint.
#' @param output_schema_fingerprint Canonical output-schema fingerprint.
#' @param implementation_fingerprint Production implementation fingerprint.
#' @param compatibility_policy_fingerprint Compatibility-policy fingerprint.
#' @param state Current migration lifecycle state.
#' @param verification_records Named verification-record identities.
#' @param rollback_identity Explicit rollback-plan identity.
#'
#' @return A validated `popgen_phase9_migration_registry_entry`.
#' @export
new_phase9_migration_registry_entry <- function(
    module_id,
    descriptor_fingerprint,
    legacy_entrypoint,
    unified_entrypoint,
    input_schema_fingerprint,
    output_schema_fingerprint,
    implementation_fingerprint,
    compatibility_policy_fingerprint,
    state = "discovered",
    verification_records = character(),
    rollback_identity) {
  verification_records <- .phase9_migration_registry_named(
    verification_records, "verification_records", allow_empty = TRUE
  )
  content <- list(
    schema_version = 1L,
    module_id = module_id,
    descriptor_fingerprint = descriptor_fingerprint,
    legacy_entrypoint = legacy_entrypoint,
    unified_entrypoint = unified_entrypoint,
    input_schema_fingerprint = input_schema_fingerprint,
    output_schema_fingerprint = output_schema_fingerprint,
    implementation_fingerprint = implementation_fingerprint,
    compatibility_policy_fingerprint = compatibility_policy_fingerprint,
    state = state,
    verification_records = verification_records,
    rollback_identity = rollback_identity
  )
  entry <- c(content, list(entry_fingerprint = .phase9_migration_registry_sha256(content)))
  class(entry) <- c("popgen_phase9_migration_registry_entry", "list")
  validate_phase9_migration_registry_entry(entry)
  entry
}

#' Validate a production-module migration registry entry
#'
#' @param x Candidate registry entry.
#' @return `x`, invisibly, when valid.
#' @export
validate_phase9_migration_registry_entry <- function(x) {
  required <- c(
    "schema_version", "module_id", "descriptor_fingerprint",
    "legacy_entrypoint", "unified_entrypoint", "input_schema_fingerprint",
    "output_schema_fingerprint", "implementation_fingerprint",
    "compatibility_policy_fingerprint", "state", "verification_records",
    "rollback_identity", "entry_fingerprint"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Migration registry entry is missing fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!identical(x$schema_version, 1L)) {
    stop("Unsupported migration registry entry version.", call. = FALSE)
  }
  for (field in c("module_id", "legacy_entrypoint", "unified_entrypoint", "state", "rollback_identity")) {
    .phase9_migration_registry_scalar(x[[field]], field)
  }
  if (!x$state %in% .phase9_migration_states) {
    stop("Unsupported migration lifecycle state.", call. = FALSE)
  }
  digest_fields <- c(
    "descriptor_fingerprint", "input_schema_fingerprint",
    "output_schema_fingerprint", "implementation_fingerprint",
    "compatibility_policy_fingerprint", "entry_fingerprint"
  )
  for (field in digest_fields) {
    .phase9_migration_registry_scalar(x[[field]], field)
    if (!grepl("^[0-9a-f]{64}$", x[[field]])) {
      stop("`", field, "` must be a lower-case SHA-256 digest.", call. = FALSE)
    }
  }
  verification_records <- .phase9_migration_registry_named(
    x$verification_records, "verification_records", allow_empty = TRUE
  )
  if (!identical(verification_records, x$verification_records)) {
    stop("Verification records must use canonical name ordering.", call. = FALSE)
  }
  if (x$state %in% c("verified", "default-unified", "legacy-disabled", "retired") &&
      !length(x$verification_records)) {
    stop("Verified or later migration states require verification evidence.", call. = FALSE)
  }
  content <- x[setdiff(required, "entry_fingerprint")]
  if (!identical(x$entry_fingerprint, .phase9_migration_registry_sha256(content))) {
    stop("Migration registry entry content does not match its fingerprint.", call. = FALSE)
  }
  invisible(x)
}

#' Validate a migration lifecycle transition
#'
#' @param from Current lifecycle state.
#' @param to Requested lifecycle state.
#' @return `TRUE`, invisibly, for an allowed transition.
#' @export
validate_phase9_migration_transition <- function(from, to) {
  .phase9_migration_registry_scalar(from, "from")
  .phase9_migration_registry_scalar(to, "to")
  if (!from %in% .phase9_migration_states || !to %in% .phase9_migration_states) {
    stop("Unsupported migration lifecycle state.", call. = FALSE)
  }
  if (!to %in% .phase9_migration_transitions[[from]]) {
    stop("Invalid migration transition from `", from, "` to `", to, "`.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Construct a deterministic production-module migration registry
#'
#' @param entries Named list of validated registry entries keyed by module ID.
#' @param registry_version Registry contract version.
#'
#' @return A validated `popgen_phase9_module_migration_registry`.
#' @export
new_phase9_module_migration_registry <- function(entries, registry_version = 1L) {
  if (!is.list(entries) || !length(entries)) {
    stop("`entries` must be a non-empty named list.", call. = FALSE)
  }
  if (is.null(names(entries)) || anyNA(names(entries)) || any(!nzchar(names(entries))) || anyDuplicated(names(entries))) {
    stop("`entries` must be uniquely named.", call. = FALSE)
  }
  entries <- entries[order(names(entries), method = "radix")]
  content <- list(registry_version = registry_version, entries = entries)
  registry <- c(content, list(registry_fingerprint = .phase9_migration_registry_sha256(content)))
  class(registry) <- c("popgen_phase9_module_migration_registry", "list")
  validate_phase9_module_migration_registry(registry)
  registry
}

#' Validate a production-module migration registry
#'
#' @param x Candidate registry.
#' @return `x`, invisibly, when valid.
#' @export
validate_phase9_module_migration_registry <- function(x) {
  required <- c("registry_version", "entries", "registry_fingerprint")
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Migration registry is missing fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!identical(x$registry_version, 1L)) {
    stop("Unsupported migration registry version.", call. = FALSE)
  }
  if (!is.list(x$entries) || !length(x$entries) || is.null(names(x$entries)) || anyDuplicated(names(x$entries))) {
    stop("Registry entries must be a non-empty uniquely named list.", call. = FALSE)
  }
  canonical <- x$entries[order(names(x$entries), method = "radix")]
  if (!identical(canonical, x$entries)) {
    stop("Registry entries must use canonical module ordering.", call. = FALSE)
  }
  for (module_id in names(x$entries)) {
    entry <- x$entries[[module_id]]
    validate_phase9_migration_registry_entry(entry)
    if (!identical(entry$module_id, module_id)) {
      stop("Registry key does not match entry module identity.", call. = FALSE)
    }
  }
  .phase9_migration_registry_scalar(x$registry_fingerprint, "registry_fingerprint")
  if (!grepl("^[0-9a-f]{64}$", x$registry_fingerprint)) {
    stop("`registry_fingerprint` must be a lower-case SHA-256 digest.", call. = FALSE)
  }
  content <- x[setdiff(required, "registry_fingerprint")]
  if (!identical(x$registry_fingerprint, .phase9_migration_registry_sha256(content))) {
    stop("Migration registry content does not match its fingerprint.", call. = FALSE)
  }
  invisible(x)
}

#' Construct a portfolio cutover-readiness record
#'
#' @param registry_fingerprint Migration registry fingerprint.
#' @param required_modules Canonically ordered required module IDs.
#' @param ready_modules Canonically ordered modules passing all cutover gates.
#' @param blocked_modules Named blocking-reason identities.
#' @param complete Whether the production portfolio is fully cut over.
#'
#' @return A validated `popgen_phase9_cutover_readiness` record.
#' @export
new_phase9_cutover_readiness <- function(
    registry_fingerprint,
    required_modules,
    ready_modules = character(),
    blocked_modules = character(),
    complete = FALSE) {
  required_modules <- sort(unique(required_modules), method = "radix")
  ready_modules <- sort(unique(ready_modules), method = "radix")
  blocked_modules <- .phase9_migration_registry_named(
    blocked_modules, "blocked_modules", allow_empty = TRUE
  )
  content <- list(
    schema_version = 1L,
    registry_fingerprint = registry_fingerprint,
    required_modules = required_modules,
    ready_modules = ready_modules,
    blocked_modules = blocked_modules,
    complete = complete
  )
  record <- c(content, list(readiness_fingerprint = .phase9_migration_registry_sha256(content)))
  class(record) <- c("popgen_phase9_cutover_readiness", "list")
  validate_phase9_cutover_readiness(record)
  record
}

#' Validate a portfolio cutover-readiness record
#'
#' @param x Candidate readiness record.
#' @return `x`, invisibly, when valid.
#' @export
validate_phase9_cutover_readiness <- function(x) {
  required <- c(
    "schema_version", "registry_fingerprint", "required_modules",
    "ready_modules", "blocked_modules", "complete", "readiness_fingerprint"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Cutover readiness record is missing fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!identical(x$schema_version, 1L)) {
    stop("Unsupported cutover-readiness version.", call. = FALSE)
  }
  for (field in c("registry_fingerprint", "readiness_fingerprint")) {
    .phase9_migration_registry_scalar(x[[field]], field)
    if (!grepl("^[0-9a-f]{64}$", x[[field]])) {
      stop("`", field, "` must be a lower-case SHA-256 digest.", call. = FALSE)
    }
  }
  if (!is.character(x$required_modules) || !length(x$required_modules) || anyNA(x$required_modules) || any(!nzchar(x$required_modules))) {
    stop("`required_modules` must contain non-empty module IDs.", call. = FALSE)
  }
  if (!identical(x$required_modules, sort(unique(x$required_modules), method = "radix")) ||
      !identical(x$ready_modules, sort(unique(x$ready_modules), method = "radix"))) {
    stop("Module identities must be unique and canonically ordered.", call. = FALSE)
  }
  if (length(setdiff(x$ready_modules, x$required_modules))) {
    stop("Ready modules must be required modules.", call. = FALSE)
  }
  blocked <- .phase9_migration_registry_named(x$blocked_modules, "blocked_modules", allow_empty = TRUE)
  if (!identical(blocked, x$blocked_modules) || length(setdiff(names(blocked), x$required_modules))) {
    stop("Blocked modules must be canonically named required modules.", call. = FALSE)
  }
  if (!is.logical(x$complete) || length(x$complete) != 1L || is.na(x$complete)) {
    stop("`complete` must be one non-missing logical value.", call. = FALSE)
  }
  expected_complete <- setequal(x$required_modules, x$ready_modules) && !length(x$blocked_modules)
  if (!identical(x$complete, expected_complete)) {
    stop("Portfolio completion claim does not match module readiness evidence.", call. = FALSE)
  }
  content <- x[setdiff(required, "readiness_fingerprint")]
  if (!identical(x$readiness_fingerprint, .phase9_migration_registry_sha256(content))) {
    stop("Cutover readiness content does not match its fingerprint.", call. = FALSE)
  }
  invisible(x)
}
