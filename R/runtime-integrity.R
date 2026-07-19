#' Compute a deterministic runtime payload digest
#'
#' @param payload Runtime payload to fingerprint.
#' @return A lowercase SHA-256 digest.
#' @export
runtime_payload_digest <- function(payload) {
  digest::digest(payload, algo = "sha256", serialize = TRUE)
}

#' Create a versioned runtime integrity envelope
#'
#' @param kind Registered runtime artifact kind.
#' @param payload Runtime payload.
#' @return A validated `PopgenVCFRuntimeEnvelope`.
#' @export
new_runtime_integrity_envelope <- function(kind, payload) {
  schema <- new_runtime_schema_metadata(kind)
  structure(
    list(
      kind = kind,
      schema = schema,
      digest_algorithm = "sha256",
      digest = runtime_payload_digest(payload),
      payload = payload
    ),
    class = "PopgenVCFRuntimeEnvelope"
  )
}

#' Validate a runtime integrity envelope
#'
#' Validation is fail closed: malformed envelopes, unsupported schemas, unknown
#' digest algorithms, and payload digest mismatches are rejected.
#'
#' @param envelope A runtime integrity envelope.
#' @param allow_legacy Whether explicitly supported legacy schemas may proceed
#'   to a later migration step.
#' @return `envelope`, invisibly.
#' @export
validate_runtime_integrity_envelope <- function(envelope, allow_legacy = FALSE) {
  if (!is.list(envelope)) {
    stop("runtime integrity envelope must be a list", call. = FALSE)
  }
  required <- c("kind", "schema", "digest_algorithm", "digest", "payload")
  missing <- setdiff(required, names(envelope))
  if (length(missing)) {
    stop("runtime integrity envelope is missing field(s): ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!is.list(envelope$schema) ||
      !all(c("kind", "version") %in% names(envelope$schema))) {
    stop("runtime integrity envelope has malformed schema metadata", call. = FALSE)
  }
  if (!identical(envelope$digest_algorithm, "sha256")) {
    stop("unsupported runtime integrity digest algorithm", call. = FALSE)
  }
  validate_runtime_schema(
    envelope$schema$kind,
    envelope$schema$version,
    allow_legacy = allow_legacy
  )
  if (!identical(envelope$kind, envelope$schema$kind)) {
    stop("runtime integrity envelope kind does not match schema kind", call. = FALSE)
  }
  expected <- runtime_payload_digest(envelope$payload)
  if (!identical(envelope$digest, expected)) {
    stop("runtime integrity digest mismatch", call. = FALSE)
  }
  invisible(envelope)
}

#' Extract a validated runtime payload
#'
#' @param envelope A runtime integrity envelope.
#' @param allow_legacy Whether explicitly supported legacy schemas may proceed
#'   to a later migration step.
#' @return The validated payload.
#' @export
runtime_integrity_payload <- function(envelope, allow_legacy = FALSE) {
  validate_runtime_integrity_envelope(envelope, allow_legacy = allow_legacy)
  envelope$payload
}
