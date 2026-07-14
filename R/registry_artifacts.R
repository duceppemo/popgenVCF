normalize_module_artifacts <- function(artifacts) {
  if (is.null(artifacts)) return(character())
  if (!is.character(artifacts) || anyNA(artifacts) || any(!nzchar(artifacts))) {
    stop("artifacts must be a character vector of non-empty names", call. = FALSE)
  }
  unique(artifacts)
}

#' Declare required artifacts for a registered analysis
#'
#' Artifact declarations are opt-in so existing modules remain compatible. Once
#' declared, the registry requires the module runner to return an `artifacts`
#' manifest containing each named artifact under the module's own namespace.
#'
#' @param registry A `PopgenVCFRegistry` object.
#' @param name Name of an already registered module.
#' @param artifacts Character vector of required artifact names.
#' @param must_exist Require the declared files to exist immediately after the
#'   module runs. Use `FALSE` when a later publishing stage materializes files.
#' @return The updated registry.
#' @export
register_analysis_artifacts <- function(registry, name, artifacts,
                                        must_exist = FALSE) {
  if (!inherits(registry, "PopgenVCFRegistry")) {
    stop("registry must be a PopgenVCFRegistry", call. = FALSE)
  }
  if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(name)) {
    stop("module name must be one non-empty string", call. = FALSE)
  }
  if (is.null(registry$modules[[name]])) {
    stop("Unknown analysis module: ", name, call. = FALSE)
  }
  if (!is.logical(must_exist) || length(must_exist) != 1L || is.na(must_exist)) {
    stop("must_exist must be TRUE or FALSE", call. = FALSE)
  }
  registry$modules[[name]]$artifacts <- normalize_module_artifacts(artifacts)
  registry$modules[[name]]$artifacts_must_exist <- must_exist
  registry
}

module_artifact_manifest <- function(output) {
  manifest <- output$artifacts
  if (is.null(manifest)) return(new_artifact_manifest())
  validate_artifact_manifest(manifest)
  manifest
}

validate_module_artifacts <- function(module_name, declared, manifest,
                                      must_exist = FALSE) {
  declared <- normalize_module_artifacts(declared)
  validate_artifact_manifest(manifest, must_exist = FALSE)
  if (!length(declared)) return(invisible(TRUE))

  module_items <- Filter(function(x) identical(x$module, module_name), manifest)
  names_present <- vapply(module_items, `[[`, character(1), "name")
  missing <- setdiff(declared, names_present)
  if (length(missing)) {
    stop(
      "Module '", module_name, "' did not produce declared artifact(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  if (isTRUE(must_exist)) {
    required_items <- Filter(function(x) {
      identical(x$module, module_name) && x$name %in% declared
    }, manifest)
    invisible(lapply(required_items, validate_analysis_artifact, must_exist = TRUE))
  }
  invisible(TRUE)
}

append_artifact_manifest <- function(target, source) {
  validate_artifact_manifest(target)
  validate_artifact_manifest(source)
  for (artifact in source) target <- register_artifact(target, artifact)
  target
}
