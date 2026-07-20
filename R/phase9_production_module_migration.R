# Phase 9 production-module migration contracts
#
# These records describe and verify the migration of one established built-in
# analysis module into the unified Phase 8/Phase 9 execution path. They do not
# replace the authoritative Phase 8 executor or the module implementation.

.phase9_migration_scalar_text <- function(x, field) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", field, "` must be one non-empty string.", call. = FALSE)
  }
  invisible(x)
}

.phase9_migration_sha256 <- function(x) {
  digest::digest(x, algo = "sha256", serialize = TRUE)
}

.phase9_migration_digest <- function(x, field, allow_na = FALSE) {
  if (allow_na && is.character(x) && length(x) == 1L && is.na(x)) {
    return(invisible(x))
  }
  .phase9_migration_scalar_text(x, field)
  if (!grepl("^[0-9a-f]{64}$", x)) {
    stop("`", field, "` must be a lower-case SHA-256 digest.", call. = FALSE)
  }
  invisible(x)
}

.phase9_migration_named_character <- function(x, field, allow_empty = FALSE) {
  if (!is.character(x) || anyNA(x) || any(!nzchar(x))) {
    stop("`", field, "` must contain non-empty strings.", call. = FALSE)
  }
  if (!length(x)) {
    if (allow_empty) {
      return(x)
    }
    stop("`", field, "` must not be empty.", call. = FALSE)
  }
  if (is.null(names(x)) || anyNA(names(x)) || any(!nzchar(names(x))) ||
      anyDuplicated(names(x))) {
    stop("`", field, "` must be uniquely named.", call. = FALSE)
  }
  x[order(names(x), method = "radix")]
}

#' Construct a Phase 9 production-module migration record
#'
#' @param module_id Stable production module identifier.
#' @param module_version Production module version.
#' @param plugin_descriptor_fingerprint Phase 9 plugin descriptor fingerprint.
#' @param legacy_entrypoint_fingerprint Legacy execution entrypoint fingerprint.
#' @param unified_entrypoint_fingerprint Unified execution entrypoint fingerprint.
#' @param input_schema_fingerprints Named input-schema fingerprints.
#' @param output_schema_fingerprints Named output-schema fingerprints.
#' @param compatibility_policy_fingerprint Compatibility-policy fingerprint.
#' @param implementation_fingerprint Module implementation fingerprint.
#' @param migration_version Migration contract version.
#'
#' @return A validated `popgen_phase9_module_migration` record.
#' @export
new_phase9_module_migration <- function(
    module_id,
    module_version,
    plugin_descriptor_fingerprint,
    legacy_entrypoint_fingerprint,
    unified_entrypoint_fingerprint,
    input_schema_fingerprints,
    output_schema_fingerprints,
    compatibility_policy_fingerprint,
    implementation_fingerprint,
    migration_version = 1L) {
  input_schema_fingerprints <- .phase9_migration_named_character(
    input_schema_fingerprints,
    "input_schema_fingerprints"
  )
  output_schema_fingerprints <- .phase9_migration_named_character(
    output_schema_fingerprints,
    "output_schema_fingerprints"
  )

  content <- list(
    migration_version = migration_version,
    module_id = module_id,
    module_version = module_version,
    plugin_descriptor_fingerprint = plugin_descriptor_fingerprint,
    legacy_entrypoint_fingerprint = legacy_entrypoint_fingerprint,
    unified_entrypoint_fingerprint = unified_entrypoint_fingerprint,
    input_schema_fingerprints = input_schema_fingerprints,
    output_schema_fingerprints = output_schema_fingerprints,
    compatibility_policy_fingerprint = compatibility_policy_fingerprint,
    implementation_fingerprint = implementation_fingerprint
  )
  record <- c(
    content,
    list(migration_fingerprint = .phase9_migration_sha256(content))
  )
  class(record) <- c("popgen_phase9_module_migration", "list")
  validate_phase9_module_migration(record)
  record
}

#' Validate a Phase 9 production-module migration record
#'
#' @param x Candidate migration record.
#' @return `x`, invisibly, when valid.
#' @export
validate_phase9_module_migration <- function(x) {
  required <- c(
    "migration_version", "module_id", "module_version",
    "plugin_descriptor_fingerprint", "legacy_entrypoint_fingerprint",
    "unified_entrypoint_fingerprint", "input_schema_fingerprints",
    "output_schema_fingerprints", "compatibility_policy_fingerprint",
    "implementation_fingerprint", "migration_fingerprint"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "Module migration is missing fields: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  if (!identical(x$migration_version, 1L)) {
    stop("Unsupported module-migration version.", call. = FALSE)
  }
  .phase9_migration_scalar_text(x$module_id, "module_id")
  .phase9_migration_scalar_text(x$module_version, "module_version")

  digest_fields <- c(
    "plugin_descriptor_fingerprint", "legacy_entrypoint_fingerprint",
    "unified_entrypoint_fingerprint", "compatibility_policy_fingerprint",
    "implementation_fingerprint", "migration_fingerprint"
  )
  for (field in digest_fields) {
    .phase9_migration_digest(x[[field]], field)
  }

  inputs <- .phase9_migration_named_character(
    x$input_schema_fingerprints,
    "input_schema_fingerprints"
  )
  outputs <- .phase9_migration_named_character(
    x$output_schema_fingerprints,
    "output_schema_fingerprints"
  )
  if (!identical(inputs, x$input_schema_fingerprints) ||
      !identical(outputs, x$output_schema_fingerprints)) {
    stop("Schema fingerprints must use canonical name ordering.", call. = FALSE)
  }
  if (any(!grepl("^[0-9a-f]{64}$", c(inputs, outputs)))) {
    stop("Every schema fingerprint must be a lower-case SHA-256 digest.", call. = FALSE)
  }

  content <- x[setdiff(required, "migration_fingerprint")]
  if (!identical(
    x$migration_fingerprint,
    .phase9_migration_sha256(content)
  )) {
    stop("Module migration content does not match its fingerprint.", call. = FALSE)
  }
  invisible(x)
}

#' Construct a production-module migration verification record
#'
#' @param migration_fingerprint Module-migration fingerprint.
#' @param legacy_result_fingerprint Canonical legacy result fingerprint.
#' @param unified_result_fingerprint Canonical unified result fingerprint.
#' @param scientific_equivalence_fingerprint Scientific-equivalence report fingerprint.
#' @param execution_mode One of `executed`, `cache_hit`, `replayed`, or `resumed`.
#' @param status One of `equivalent`, `different`, or `rejected`.
#' @param linked_records Named fingerprints for runtime, cache, recovery,
#'   provenance, validation, and publication records.
#' @param reason Stable machine-readable verification reason.
#'
#' @return A validated `popgen_phase9_migration_verification` record.
#' @export
new_phase9_migration_verification <- function(
    migration_fingerprint,
    legacy_result_fingerprint,
    unified_result_fingerprint,
    scientific_equivalence_fingerprint,
    execution_mode,
    status,
    linked_records = character(),
    reason) {
  linked_records <- .phase9_migration_named_character(
    linked_records,
    "linked_records",
    allow_empty = TRUE
  )
  content <- list(
    schema_version = 1L,
    migration_fingerprint = migration_fingerprint,
    legacy_result_fingerprint = legacy_result_fingerprint,
    unified_result_fingerprint = unified_result_fingerprint,
    scientific_equivalence_fingerprint = scientific_equivalence_fingerprint,
    execution_mode = execution_mode,
    status = status,
    linked_records = linked_records,
    reason = reason
  )
  record <- c(
    content,
    list(verification_fingerprint = .phase9_migration_sha256(content))
  )
  class(record) <- c("popgen_phase9_migration_verification", "list")
  validate_phase9_migration_verification(record)
  record
}

#' Validate a production-module migration verification record
#'
#' @param x Candidate verification record.
#' @return `x`, invisibly, when valid.
#' @export
validate_phase9_migration_verification <- function(x) {
  required <- c(
    "schema_version", "migration_fingerprint",
    "legacy_result_fingerprint", "unified_result_fingerprint",
    "scientific_equivalence_fingerprint", "execution_mode", "status",
    "linked_records", "reason", "verification_fingerprint"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "Migration verification is missing fields: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  if (!identical(x$schema_version, 1L)) {
    stop("Unsupported migration-verification version.", call. = FALSE)
  }

  for (field in c(
    "migration_fingerprint", "legacy_result_fingerprint",
    "unified_result_fingerprint", "scientific_equivalence_fingerprint",
    "verification_fingerprint"
  )) {
    .phase9_migration_digest(x[[field]], field)
  }
  .phase9_migration_scalar_text(x$execution_mode, "execution_mode")
  .phase9_migration_scalar_text(x$status, "status")
  .phase9_migration_scalar_text(x$reason, "reason")

  if (!x$execution_mode %in% c("executed", "cache_hit", "replayed", "resumed")) {
    stop("Unsupported migration verification execution mode.", call. = FALSE)
  }
  if (!x$status %in% c("equivalent", "different", "rejected")) {
    stop("Unsupported migration verification status.", call. = FALSE)
  }
  linked <- .phase9_migration_named_character(
    x$linked_records,
    "linked_records",
    allow_empty = TRUE
  )
  if (!identical(linked, x$linked_records)) {
    stop("Linked records must use canonical name ordering.", call. = FALSE)
  }
  if (length(linked) && any(!grepl("^[0-9a-f]{64}$", linked))) {
    stop("Every linked record must be a lower-case SHA-256 digest.", call. = FALSE)
  }
  if (identical(x$status, "equivalent") &&
      !identical(x$legacy_result_fingerprint, x$unified_result_fingerprint)) {
    stop(
      "Equivalent migrations require identical canonical result fingerprints.",
      call. = FALSE
    )
  }

  content <- x[setdiff(required, "verification_fingerprint")]
  if (!identical(
    x$verification_fingerprint,
    .phase9_migration_sha256(content)
  )) {
    stop(
      "Migration verification content does not match its fingerprint.",
      call. = FALSE
    )
  }
  invisible(x)
}
