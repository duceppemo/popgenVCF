# Runtime schema compatibility -------------------------------------------------

.runtime_schema_versions <- c(
  execution_plan = 1L,
  execution_ledger = 1L,
  attempt_ledger = 1L,
  checkpoint = 1L,
  scheduler_metadata = 1L,
  resource_policy = 1L,
  process_result = 1L,
  process_workspace = 1L,
  lifecycle_events = 1L
)

#' Return supported execution-runtime schema versions
#'
#' The returned named integer vector is the canonical compatibility contract for
#' persisted and externally inspected execution-runtime objects.
#'
#' @return A named integer vector of current schema versions.
#' @export
runtime_schema_versions <- function() {
  .runtime_schema_versions
}

#' Classify an execution-runtime schema version
#'
#' @param kind Runtime artifact kind.
#' @param version Positive integer schema version.
#' @return One of `"current"`, `"legacy"`, or `"unsupported_future"`.
#' @export
classify_runtime_schema <- function(kind, version) {
  kind <- as.character(kind)[1]
  if (is.na(kind) || !nzchar(kind) || !kind %in% names(.runtime_schema_versions)) {
    stop("unknown runtime schema kind: ", kind, call. = FALSE)
  }

  version <- as.integer(version)[1]
  if (is.na(version) || version < 1L) {
    stop("runtime schema version must be a positive integer", call. = FALSE)
  }

  current <- unname(.runtime_schema_versions[[kind]])
  if (version == current) return("current")
  if (version < current) return("legacy")
  "unsupported_future"
}

#' Validate an execution-runtime schema version
#'
#' Unknown kinds and unsupported future versions fail closed. Legacy versions
#' are accepted only when the caller explicitly permits them for migration.
#'
#' @param kind Runtime artifact kind.
#' @param version Positive integer schema version.
#' @param allow_legacy Whether older versions may pass validation.
#' @return The normalized schema version, invisibly.
#' @export
validate_runtime_schema <- function(kind, version, allow_legacy = FALSE) {
  classification <- classify_runtime_schema(kind, version)
  if (identical(classification, "unsupported_future")) {
    stop(
      "unsupported future runtime schema for '", kind, "': ", version,
      " (current: ", .runtime_schema_versions[[kind]], ")",
      call. = FALSE
    )
  }
  if (identical(classification, "legacy") && !isTRUE(allow_legacy)) {
    stop(
      "legacy runtime schema for '", kind, "' requires explicit migration: ",
      version, " (current: ", .runtime_schema_versions[[kind]], ")",
      call. = FALSE
    )
  }
  invisible(as.integer(version)[1])
}

#' Create canonical runtime schema metadata
#'
#' @param kind Runtime artifact kind.
#' @return A named list containing the artifact kind and current schema version.
#' @export
new_runtime_schema_metadata <- function(kind) {
  validate_runtime_schema(kind, .runtime_schema_versions[[kind]])
  list(kind = kind, version = unname(.runtime_schema_versions[[kind]]))
}
