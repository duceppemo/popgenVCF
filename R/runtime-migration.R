# Runtime migration -----------------------------------------------------------

#' Create an empty runtime migration registry
#'
#' @return A `PopgenVCFRuntimeMigrationRegistry` object.
#' @export
new_runtime_migration_registry <- function() {
  structure(list(entries = list()), class = "PopgenVCFRuntimeMigrationRegistry")
}

migration_registry_key <- function(kind, from_version, to_version) {
  paste(kind, as.integer(from_version), as.integer(to_version), sep = ":")
}

#' Register one adjacent runtime schema migration
#'
#' @param registry A runtime migration registry.
#' @param kind Registered runtime artifact kind.
#' @param from_version Source schema version.
#' @param to_version Target schema version. Must equal `from_version + 1`.
#' @param migrate Deterministic payload transformation function.
#' @param id Stable migration step identifier.
#' @return An updated registry.
#' @export
register_runtime_migration <- function(
    registry, kind, from_version, to_version, migrate, id) {
  if (!inherits(registry, "PopgenVCFRuntimeMigrationRegistry") ||
      !is.list(registry$entries)) {
    stop("registry must be a PopgenVCFRuntimeMigrationRegistry", call. = FALSE)
  }
  kind <- as.character(kind)[1]
  if (is.na(kind) || !kind %in% names(runtime_schema_versions())) {
    stop("unknown runtime schema kind: ", kind, call. = FALSE)
  }
  from_version <- as.integer(from_version)[1]
  to_version <- as.integer(to_version)[1]
  if (is.na(from_version) || from_version < 1L || is.na(to_version) ||
      !identical(to_version, from_version + 1L)) {
    stop("runtime migrations must connect adjacent positive schema versions",
         call. = FALSE)
  }
  if (!is.function(migrate)) {
    stop("runtime migration must be a function", call. = FALSE)
  }
  id <- as.character(id)[1]
  if (is.na(id) || !nzchar(id)) {
    stop("runtime migration id must be a non-empty string", call. = FALSE)
  }
  key <- migration_registry_key(kind, from_version, to_version)
  if (!is.null(registry$entries[[key]])) {
    stop("runtime migration is already registered: ", key, call. = FALSE)
  }
  registry$entries[[key]] <- list(
    id = id,
    kind = kind,
    from_version = from_version,
    to_version = to_version,
    migrate = migrate
  )
  registry
}

#' Resolve a contiguous runtime migration path
#'
#' @param registry A runtime migration registry.
#' @param kind Registered runtime artifact kind.
#' @param from_version Source schema version.
#' @param to_version Target schema version. Defaults to the current version.
#' @return An ordered list of migration entries.
#' @export
runtime_migration_path <- function(
    registry, kind, from_version,
    to_version = unname(runtime_schema_versions()[[kind]])) {
  if (!inherits(registry, "PopgenVCFRuntimeMigrationRegistry") ||
      !is.list(registry$entries)) {
    stop("registry must be a PopgenVCFRuntimeMigrationRegistry", call. = FALSE)
  }
  kind <- as.character(kind)[1]
  current <- runtime_schema_versions()
  if (is.na(kind) || !kind %in% names(current)) {
    stop("unknown runtime schema kind: ", kind, call. = FALSE)
  }
  from_version <- as.integer(from_version)[1]
  to_version <- as.integer(to_version)[1]
  if (is.na(from_version) || from_version < 1L ||
      is.na(to_version) || to_version < 1L) {
    stop("runtime schema versions must be positive integers", call. = FALSE)
  }
  if (from_version > to_version) {
    stop("runtime migrations cannot downgrade schemas", call. = FALSE)
  }
  if (to_version > unname(current[[kind]])) {
    stop("runtime migration target is an unsupported future schema", call. = FALSE)
  }
  if (from_version == to_version) return(list())

  path <- vector("list", to_version - from_version)
  for (index in seq_along(path)) {
    source <- from_version + index - 1L
    target <- source + 1L
    key <- migration_registry_key(kind, source, target)
    entry <- registry$entries[[key]]
    if (is.null(entry)) {
      stop("no registered runtime migration path for ", kind, " from ",
           from_version, " to ", to_version, call. = FALSE)
    }
    path[[index]] <- entry
  }
  path
}

#' Migrate a runtime integrity envelope explicitly
#'
#' @param envelope A current or legacy runtime integrity envelope.
#' @param registry A runtime migration registry.
#' @return A `PopgenVCFRuntimeMigrationResult` containing the current envelope
#'   and a deterministic migration record.
#' @export
migrate_runtime_integrity_envelope <- function(envelope, registry) {
  validate_runtime_integrity_envelope(envelope, allow_legacy = TRUE)
  kind <- as.character(envelope$kind)[1]
  source_version <- as.integer(envelope$schema$version)[1]
  target_version <- unname(runtime_schema_versions()[[kind]])
  classification <- classify_runtime_schema(kind, source_version)
  if (identical(classification, "unsupported_future")) {
    stop("unsupported future runtime schemas cannot be migrated", call. = FALSE)
  }

  path <- runtime_migration_path(
    registry, kind, source_version, target_version
  )
  payload <- envelope$payload
  before_digest <- runtime_payload_digest(payload)
  steps <- vector("list", length(path))

  for (index in seq_along(path)) {
    entry <- path[[index]]
    first <- entry$migrate(payload)
    second <- entry$migrate(payload)
    first_digest <- runtime_payload_digest(first)
    second_digest <- runtime_payload_digest(second)
    if (!identical(first_digest, second_digest)) {
      stop("runtime migration is nondeterministic: ", entry$id, call. = FALSE)
    }
    if (inherits(first, "PopgenVCFRuntimeEnvelope")) {
      stop("runtime migration steps must return payloads, not envelopes",
           call. = FALSE)
    }
    steps[[index]] <- list(
      id = entry$id,
      from_version = entry$from_version,
      to_version = entry$to_version,
      before_digest = runtime_payload_digest(payload),
      after_digest = first_digest
    )
    payload <- first
  }

  migrated <- new_runtime_integrity_envelope(kind, payload)
  validate_runtime_integrity_envelope(migrated)
  record <- structure(
    list(
      kind = kind,
      source_version = source_version,
      target_version = target_version,
      source_payload_digest = before_digest,
      target_payload_digest = runtime_payload_digest(payload),
      steps = steps,
      migration_fingerprint = runtime_payload_digest(list(
        kind = kind,
        source_version = source_version,
        target_version = target_version,
        source_payload_digest = before_digest,
        target_payload_digest = runtime_payload_digest(payload),
        steps = steps
      ))
    ),
    class = "PopgenVCFRuntimeMigrationRecord"
  )
  structure(
    list(envelope = migrated, record = record),
    class = "PopgenVCFRuntimeMigrationResult"
  )
}
