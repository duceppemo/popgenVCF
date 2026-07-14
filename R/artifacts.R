#' Create a canonical analysis artifact
#'
#' Artifacts describe files emitted by analysis modules. They are deliberately
#' separate from in-memory statistical results so reports and release tooling can
#' consume a stable manifest without understanding each module's implementation.
#'
#' @param module Analysis module name.
#' @param name Stable artifact identifier within the module.
#' @param type One of `table`, `figure`, `methods`, `caption`, `supplementary`,
#'   `validation`, `provenance`, `report`, or `data`.
#' @param path File path, preferably relative to the analysis output directory.
#' @param format File format such as `tsv`, `pdf`, `svg`, `png`, `md`, or `json`.
#' @param description Human-readable description.
#' @param required Whether absence of the artifact invalidates the module output.
#' @param metadata Optional named metadata list.
#' @return A `PopgenVCFArtifact` object.
#' @export
new_analysis_artifact <- function(module, name, type, path, format,
                                  description = "", required = TRUE,
                                  metadata = list()) {
  allowed <- c("table", "figure", "methods", "caption", "supplementary",
               "validation", "provenance", "report", "data")
  scalar_string <- function(x, label) {
    if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
      stop(label, " must be one non-empty string", call. = FALSE)
    }
    x
  }
  module <- scalar_string(module, "module")
  name <- scalar_string(name, "name")
  type <- scalar_string(type, "type")
  path <- scalar_string(path, "path")
  format <- tolower(scalar_string(format, "format"))
  if (!type %in% allowed) stop("unsupported artifact type: ", type, call. = FALSE)
  if (!is.logical(required) || length(required) != 1L || is.na(required)) {
    stop("required must be TRUE or FALSE", call. = FALSE)
  }
  if (!is.list(metadata) || (length(metadata) && is.null(names(metadata)))) {
    stop("metadata must be a named list", call. = FALSE)
  }
  structure(list(
    schema_version = "1.0",
    module = module,
    name = name,
    type = type,
    path = path,
    format = format,
    description = as.character(description)[1L],
    required = required,
    metadata = metadata
  ), class = "PopgenVCFArtifact")
}

#' Validate an analysis artifact
#' @param artifact A `PopgenVCFArtifact` object.
#' @param must_exist Require the declared file to exist.
#' @return `TRUE` invisibly, or an error.
#' @export
validate_analysis_artifact <- function(artifact, must_exist = FALSE) {
  if (!inherits(artifact, "PopgenVCFArtifact")) {
    stop("artifact must be a PopgenVCFArtifact", call. = FALSE)
  }
  required_fields <- c("schema_version", "module", "name", "type", "path",
                       "format", "description", "required", "metadata")
  missing <- setdiff(required_fields, names(artifact))
  if (length(missing)) stop("artifact is missing field(s): ", paste(missing, collapse = ", "), call. = FALSE)
  if (isTRUE(must_exist) && !file.exists(artifact$path)) {
    stop("artifact file does not exist: ", artifact$path, call. = FALSE)
  }
  invisible(TRUE)
}

#' Create an artifact manifest
#' @param artifacts A list of `PopgenVCFArtifact` objects.
#' @return A `PopgenVCFArtifactManifest` object.
#' @export
new_artifact_manifest <- function(artifacts = list()) {
  if (!is.list(artifacts)) stop("artifacts must be a list", call. = FALSE)
  manifest <- structure(artifacts, class = c("PopgenVCFArtifactManifest", "list"))
  validate_artifact_manifest(manifest)
  manifest
}

#' Add an artifact to a manifest
#' @param manifest A `PopgenVCFArtifactManifest`.
#' @param artifact A `PopgenVCFArtifact`.
#' @return Updated manifest.
#' @export
register_artifact <- function(manifest, artifact) {
  validate_artifact_manifest(manifest)
  validate_analysis_artifact(artifact)
  key <- paste(artifact$module, artifact$name, sep = "::")
  existing <- vapply(manifest, function(x) paste(x$module, x$name, sep = "::"), character(1))
  if (key %in% existing) stop("duplicate artifact identifier: ", key, call. = FALSE)
  manifest[[length(manifest) + 1L]] <- artifact
  class(manifest) <- c("PopgenVCFArtifactManifest", "list")
  manifest
}

#' Validate an artifact manifest
#' @param manifest A `PopgenVCFArtifactManifest`.
#' @param must_exist Require all required artifact files to exist.
#' @return `TRUE` invisibly, or an error.
#' @export
validate_artifact_manifest <- function(manifest, must_exist = FALSE) {
  if (!inherits(manifest, "PopgenVCFArtifactManifest")) {
    stop("manifest must be a PopgenVCFArtifactManifest", call. = FALSE)
  }
  invisible(lapply(manifest, validate_analysis_artifact,
                   must_exist = FALSE))
  if (isTRUE(must_exist)) {
    invisible(lapply(Filter(function(x) isTRUE(x$required), manifest),
                     validate_analysis_artifact, must_exist = TRUE))
  }
  keys <- vapply(manifest, function(x) paste(x$module, x$name, sep = "::"), character(1))
  if (anyDuplicated(keys)) stop("artifact manifest contains duplicate identifiers", call. = FALSE)
  invisible(TRUE)
}

#' Convert an artifact manifest to a data table
#' @param manifest A `PopgenVCFArtifactManifest`.
#' @return A data table with one row per artifact.
#' @export
artifact_manifest_table <- function(manifest) {
  validate_artifact_manifest(manifest)
  if (!length(manifest)) return(data.table::data.table())
  data.table::rbindlist(lapply(manifest, function(x) data.table::data.table(
    schema_version = x$schema_version,
    module = x$module,
    name = x$name,
    type = x$type,
    path = x$path,
    format = x$format,
    description = x$description,
    required = x$required
  )), use.names = TRUE)
}
