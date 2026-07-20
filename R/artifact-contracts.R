# Canonical analysis-result and publication-artifact contracts

.artifact_contract_schema_version <- "1.0.0"

.normalize_contract_id <- function(x, field) {
  if (!is.character(x) || length(x) != 1L || is.na(x) ||
      !grepl("^[a-z][a-z0-9_.-]*$", x)) {
    stop(sprintf("`%s` must be one canonical lowercase identifier.", field), call. = FALSE)
  }
  x
}

.normalize_named_character <- function(x, field, allow_empty = TRUE) {
  if (is.null(x) && allow_empty) return(character())
  if (!is.character(x) || anyNA(x) || is.null(names(x)) || any(names(x) == "") ||
      anyDuplicated(names(x))) {
    stop(sprintf("`%s` must be a uniquely named character vector.", field), call. = FALSE)
  }
  x[order(names(x), method = "radix")]
}

new_artifact_contract <- function(id, type, role, format, checksum,
                                  schema_version = .artifact_contract_schema_version,
                                  metadata = character()) {
  id <- .normalize_contract_id(id, "id")
  type <- match.arg(type, c("table", "figure", "methods", "caption",
                            "supplement", "validation"))
  role <- .normalize_contract_id(role, "role")
  if (!is.character(format) || length(format) != 1L || is.na(format) || !nzchar(format)) {
    stop("`format` must be one non-empty string.", call. = FALSE)
  }
  if (!is.character(checksum) || length(checksum) != 1L || is.na(checksum) ||
      !grepl("^[a-f0-9]{64}$", checksum)) {
    stop("`checksum` must be a lowercase SHA-256 digest.", call. = FALSE)
  }
  if (!identical(schema_version, .artifact_contract_schema_version)) {
    stop(sprintf("Unsupported artifact schema version: %s", schema_version), call. = FALSE)
  }

  structure(
    list(
      schema_version = schema_version,
      id = id,
      type = type,
      role = role,
      format = format,
      checksum = checksum,
      metadata = .normalize_named_character(metadata, "metadata")
    ),
    class = "popgen_artifact_contract"
  )
}

validate_artifact_contracts <- function(artifacts) {
  if (!is.list(artifacts)) stop("`artifacts` must be a list.", call. = FALSE)
  if (!length(artifacts)) return(invisible(TRUE))
  valid <- vapply(artifacts, inherits, logical(1), what = "popgen_artifact_contract")
  if (!all(valid)) stop("Every artifact must be a canonical artifact contract.", call. = FALSE)
  ids <- vapply(artifacts, `[[`, character(1), "id")
  if (anyDuplicated(ids)) stop("Artifact identifiers must be unique.", call. = FALSE)
  invisible(TRUE)
}

new_analysis_result_contract <- function(module_id, module_version, status,
                                         configuration_fingerprint,
                                         artifacts = list(), warnings = character(),
                                         schema_version = .artifact_contract_schema_version) {
  module_id <- .normalize_contract_id(module_id, "module_id")
  if (!is.character(module_version) || length(module_version) != 1L || is.na(module_version) ||
      !grepl("^[0-9]+\\.[0-9]+\\.[0-9]+(?:[-+][A-Za-z0-9.-]+)?$", module_version)) {
    stop("`module_version` must be a semantic version.", call. = FALSE)
  }
  status <- match.arg(status, c("success", "failed", "blocked", "cancelled", "timed_out"))
  if (!is.character(configuration_fingerprint) || length(configuration_fingerprint) != 1L ||
      is.na(configuration_fingerprint) || !grepl("^[a-f0-9]{64}$", configuration_fingerprint)) {
    stop("`configuration_fingerprint` must be a lowercase SHA-256 digest.", call. = FALSE)
  }
  if (!identical(schema_version, .artifact_contract_schema_version)) {
    stop(sprintf("Unsupported result schema version: %s", schema_version), call. = FALSE)
  }
  validate_artifact_contracts(artifacts)
  if (!is.character(warnings) || anyNA(warnings)) {
    stop("`warnings` must be a character vector without missing values.", call. = FALSE)
  }
  artifacts <- artifacts[order(vapply(artifacts, `[[`, character(1), "id"), method = "radix")]

  structure(
    list(
      schema_version = schema_version,
      module_id = module_id,
      module_version = module_version,
      status = status,
      configuration_fingerprint = configuration_fingerprint,
      artifacts = artifacts,
      warnings = sort(unique(warnings), method = "radix")
    ),
    class = "popgen_analysis_result_contract"
  )
}
