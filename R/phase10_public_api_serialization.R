# Deterministic, type-preserving serialization for Phase 10 public records.
#
# The JSON envelope is intentionally small. The canonical R record is serialized
# with R serialization version 3 and base64 encoded so named vectors, data-frame
# schemas, integer types, classes, and fingerprints survive an exact round trip.

#' Write a deterministic public API record
#'
#' @param x A public API descriptor, request, or response.
#' @param path Output JSON path.
#' @return The normalized path, invisibly.
#' @export
write_public_api_record <- function(x, path) {
  .phase10_validate_public_record(x)
  payload <- serialize(x, NULL, version = 3L)
  envelope <- list(
    envelope_type = "popgenvcf_public_api_record",
    envelope_version = "1.0.0",
    record_type = x$record_type,
    fingerprint = unclass(x$fingerprint),
    encoding = "r-serialize-v3+base64",
    payload = as.character(openssl::base64_encode(payload))
  )
  json <- jsonlite::toJSON(
    envelope, auto_unbox = TRUE, null = "null", pretty = TRUE
  )
  writeLines(json, path, useBytes = TRUE)
  invisible(normalizePath(path, winslash = "/", mustWork = FALSE))
}

#' Read and validate a deterministic public API record
#'
#' @param path Input JSON path.
#' @return A validated public API object.
#' @export
read_public_api_record <- function(path) {
  envelope <- jsonlite::fromJSON(path, simplifyVector = TRUE)
  required <- c(
    "envelope_type", "envelope_version", "record_type", "fingerprint",
    "encoding", "payload"
  )
  if (!is.list(envelope) || !all(required %in% names(envelope)) ||
      !identical(envelope$envelope_type, "popgenvcf_public_api_record") ||
      !identical(envelope$envelope_version, "1.0.0") ||
      !identical(envelope$encoding, "r-serialize-v3+base64")) {
    stop("Unsupported or malformed public API record envelope.", call. = FALSE)
  }

  x <- unserialize(openssl::base64_decode(envelope$payload))
  if (!is.list(x) || !identical(x$record_type, envelope$record_type) ||
      !identical(unclass(x$fingerprint), unclass(envelope$fingerprint))) {
    stop("Public API envelope identity verification failed.", call. = FALSE)
  }
  .phase10_validate_public_record(x)
  x
}
