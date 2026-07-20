#' Construct a canonical scientific cache key
#'
#' Builds a deterministic cache-key record from canonical scientific-object
#' fingerprints, schema identities, module contracts, normalized parameters,
#' and dependency fingerprints. The initial contract is intentionally strict:
#' missing or ambiguous identities fail before any cache lookup is attempted.
#'
#' @param object_fingerprints Named character vector of scientific-object
#'   fingerprints.
#' @param schema_id Canonical schema identifier.
#' @param schema_version Canonical schema version.
#' @param module_id Canonical producer module identifier.
#' @param module_version Producer module version.
#' @param parameters Named list of normalized execution parameters.
#' @param dependency_fingerprints Named character vector of dependency
#'   fingerprints.
#'
#' @return A `popgen_cache_key` object.
#' @export
new_scientific_cache_key <- function(
    object_fingerprints,
    schema_id,
    schema_version,
    module_id,
    module_version,
    parameters = list(),
    dependency_fingerprints = character()
) {
  object_fingerprints <- .validate_named_fingerprints(
    object_fingerprints,
    "object_fingerprints"
  )
  dependency_fingerprints <- .validate_named_fingerprints(
    dependency_fingerprints,
    "dependency_fingerprints",
    allow_empty = TRUE
  )

  fields <- list(
    schema_id = .scalar_cache_string(schema_id, "schema_id"),
    schema_version = .scalar_cache_string(schema_version, "schema_version"),
    module_id = .scalar_cache_string(module_id, "module_id"),
    module_version = .scalar_cache_string(module_version, "module_version"),
    object_fingerprints = object_fingerprints[order(names(object_fingerprints))],
    parameters = .canonicalize_cache_value(parameters),
    dependency_fingerprints = dependency_fingerprints[
      order(names(dependency_fingerprints))
    ]
  )

  structure(
    c(fields, list(fingerprint = .cache_contract_fingerprint(fields))),
    class = "popgen_cache_key"
  )
}

#' Construct an immutable scientific cache manifest
#'
#' @param key A `popgen_cache_key` object.
#' @param payload_checksum Checksum of the serialized cache payload.
#' @param payload_format Stable payload format identifier.
#' @param provenance_fingerprint Fingerprint of the canonical provenance record.
#' @param created_by Producer identity.
#' @param dependencies Optional dependency manifest records.
#'
#' @return A `popgen_cache_manifest` object.
#' @export
new_scientific_cache_manifest <- function(
    key,
    payload_checksum,
    payload_format,
    provenance_fingerprint,
    created_by,
    dependencies = list()
) {
  if (!inherits(key, "popgen_cache_key")) {
    stop("`key` must inherit from `popgen_cache_key`.", call. = FALSE)
  }

  manifest <- list(
    contract = "popgenVCF.scientific-cache-manifest",
    contract_version = "1.0.0",
    cache_key = unclass(key),
    payload_checksum = .scalar_cache_string(
      payload_checksum,
      "payload_checksum"
    ),
    payload_format = .scalar_cache_string(payload_format, "payload_format"),
    provenance_fingerprint = .scalar_cache_string(
      provenance_fingerprint,
      "provenance_fingerprint"
    ),
    created_by = .scalar_cache_string(created_by, "created_by"),
    dependencies = .canonicalize_cache_value(dependencies)
  )

  structure(
    c(manifest, list(manifest_fingerprint = .cache_contract_fingerprint(manifest))),
    class = "popgen_cache_manifest"
  )
}

#' Validate a scientific cache manifest
#'
#' Returns a structured, fail-closed validation report. Cache reuse is allowed
#' only when the report is valid and the expected key, payload checksum, schema,
#' module contract, dependencies, and provenance all match.
#'
#' @param manifest A cache manifest.
#' @param expected_key Optional expected `popgen_cache_key`.
#' @param payload_checksum Optional observed payload checksum.
#'
#' @return A `popgen_cache_validation` object.
#' @export
validate_scientific_cache_manifest <- function(
    manifest,
    expected_key = NULL,
    payload_checksum = NULL
) {
  errors <- character()

  if (!inherits(manifest, "popgen_cache_manifest")) {
    errors <- c(errors, "manifest_class_invalid")
  } else {
    required <- c(
      "contract",
      "contract_version",
      "cache_key",
      "payload_checksum",
      "payload_format",
      "provenance_fingerprint",
      "created_by",
      "dependencies",
      "manifest_fingerprint"
    )
    missing_fields <- setdiff(required, names(manifest))
    if (length(missing_fields)) {
      errors <- c(errors, paste0("missing_field:", missing_fields))
    }

    manifest_body <- manifest[setdiff(names(manifest), "manifest_fingerprint")]
    observed_manifest_fingerprint <- .cache_contract_fingerprint(manifest_body)
    if (!identical(observed_manifest_fingerprint, manifest$manifest_fingerprint)) {
      errors <- c(errors, "manifest_fingerprint_mismatch")
    }

    if (!is.null(expected_key)) {
      if (!inherits(expected_key, "popgen_cache_key")) {
        errors <- c(errors, "expected_key_class_invalid")
      } else if (!identical(
        manifest$cache_key$fingerprint,
        expected_key$fingerprint
      )) {
        errors <- c(errors, "cache_key_mismatch")
      }
    }

    if (!is.null(payload_checksum) && !identical(
      manifest$payload_checksum,
      payload_checksum
    )) {
      errors <- c(errors, "payload_checksum_mismatch")
    }
  }

  errors <- sort(unique(errors))
  structure(
    list(
      valid = length(errors) == 0L,
      decision = if (length(errors)) "reject" else "accept",
      errors = errors
    ),
    class = "popgen_cache_validation"
  )
}

.scalar_cache_string <- function(value, field) {
  if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)) {
    stop(sprintf("`%s` must be one non-empty string.", field), call. = FALSE)
  }
  value
}

.validate_named_fingerprints <- function(value, field, allow_empty = FALSE) {
  if (!is.character(value) || anyNA(value) || any(!nzchar(value))) {
    stop(sprintf("`%s` must contain non-empty fingerprints.", field), call. = FALSE)
  }
  if (!length(value) && allow_empty) {
    return(value)
  }
  if (!length(value) || is.null(names(value)) || any(!nzchar(names(value)))) {
    stop(sprintf("`%s` must be a named character vector.", field), call. = FALSE)
  }
  if (anyDuplicated(names(value))) {
    stop(sprintf("`%s` contains duplicate identities.", field), call. = FALSE)
  }
  value
}

.canonicalize_cache_value <- function(value) {
  if (is.list(value)) {
    if (!is.null(names(value))) {
      if (anyDuplicated(names(value))) {
        stop("Canonical cache lists cannot contain duplicate names.", call. = FALSE)
      }
      value <- value[order(names(value))]
    }
    return(lapply(value, .canonicalize_cache_value))
  }
  if (is.factor(value)) {
    return(as.character(value))
  }
  value
}

.cache_contract_fingerprint <- function(value) {
  bytes <- serialize(value, NULL, version = 3L)
  paste0("r-serialize-v3:", paste(sprintf("%02x", as.integer(bytes)), collapse = ""))
}
