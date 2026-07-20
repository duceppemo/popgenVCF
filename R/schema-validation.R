#' Create a canonical schema descriptor
#'
#' @param id Stable schema identifier.
#' @param version Semantic schema version.
#' @param lifecycle One of `stable`, `experimental`, or `deprecated`.
#' @param validator Validation function accepting one object.
#' @param fingerprint Optional deterministic schema fingerprint.
#' @return A canonical schema descriptor.
#' @export
new_schema_descriptor <- function(id,
                                  version,
                                  lifecycle = c("stable", "experimental", "deprecated"),
                                  validator,
                                  fingerprint = NULL) {
  lifecycle <- match.arg(lifecycle)

  stopifnot(
    is.character(id), length(id) == 1L, nzchar(id),
    grepl("^[a-z][a-z0-9._-]*$", id),
    is.character(version), length(version) == 1L,
    grepl("^[0-9]+\\.[0-9]+\\.[0-9]+$", version),
    is.function(validator)
  )

  structure(
    list(
      id = id,
      version = version,
      lifecycle = lifecycle,
      validator = validator,
      fingerprint = fingerprint
    ),
    class = c("popgen_schema_descriptor", "list")
  )
}

#' Create a canonical schema registry
#'
#' @param schemas Optional list of schema descriptors.
#' @return A schema registry.
#' @export
new_schema_registry <- function(schemas = list()) {
  registry <- structure(
    list(schemas = list(), migrations = list()),
    class = c("popgen_schema_registry", "list")
  )

  for (schema in schemas) {
    registry <- register_schema(registry, schema)
  }

  registry
}

#' Register a schema descriptor
#'
#' @param registry Schema registry.
#' @param schema Schema descriptor.
#' @return Updated schema registry.
#' @export
register_schema <- function(registry, schema) {
  stopifnot(
    inherits(registry, "popgen_schema_registry"),
    inherits(schema, "popgen_schema_descriptor")
  )

  key <- paste(schema$id, schema$version, sep = "@")
  if (!is.null(registry$schemas[[key]])) {
    stop("Schema already registered: ", key, call. = FALSE)
  }

  registry$schemas[[key]] <- schema
  registry$schemas <- registry$schemas[order(names(registry$schemas))]
  registry
}

#' Resolve a schema from the registry
#'
#' @param registry Schema registry.
#' @param id Schema identifier.
#' @param version Exact semantic version.
#' @param allow_deprecated Whether deprecated schemas may be resolved.
#' @return A registered schema descriptor.
#' @export
resolve_schema <- function(registry, id, version, allow_deprecated = FALSE) {
  stopifnot(inherits(registry, "popgen_schema_registry"))
  key <- paste(id, version, sep = "@")
  schema <- registry$schemas[[key]]

  if (is.null(schema)) {
    stop("Unknown schema: ", key, call. = FALSE)
  }
  if (identical(schema$lifecycle, "deprecated") && !isTRUE(allow_deprecated)) {
    stop("Deprecated schema requires explicit opt-in: ", key, call. = FALSE)
  }

  schema
}

#' Validate an object against a registered schema
#'
#' @param registry Schema registry.
#' @param object Object to validate.
#' @param id Schema identifier.
#' @param version Exact semantic version.
#' @param allow_deprecated Whether deprecated schemas may be used.
#' @return A structured validation report.
#' @export
validate_schema_object <- function(registry,
                                   object,
                                   id,
                                   version,
                                   allow_deprecated = FALSE) {
  schema <- resolve_schema(registry, id, version, allow_deprecated)

  result <- tryCatch(
    schema$validator(object),
    error = function(error) list(valid = FALSE, errors = conditionMessage(error))
  )

  if (isTRUE(result)) {
    result <- list(valid = TRUE, errors = character(), warnings = character())
  }
  if (!is.list(result) || is.null(result$valid) || length(result$valid) != 1L) {
    stop("Schema validator returned an invalid result for ", id, "@", version, call. = FALSE)
  }

  structure(
    list(
      schema_id = id,
      schema_version = version,
      valid = isTRUE(result$valid),
      errors = sort(unique(as.character(result$errors %||% character()))),
      warnings = sort(unique(as.character(result$warnings %||% character()))),
      schema_fingerprint = schema$fingerprint
    ),
    class = c("popgen_schema_validation_report", "list")
  )
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
