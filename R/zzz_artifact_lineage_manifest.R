#' Convert an artifact manifest to immutable lineage
#'
#' @param manifest A `PopgenVCFArtifactManifest` whose files exist.
#' @param executions List of lineage execution records.
#' @param consumers Optional named list mapping `module::name` artifact IDs to
#'   consuming execution IDs.
#' @param execution_ids Optional named character vector mapping module names to
#'   execution IDs. Defaults to `exec:<module>`.
#' @return A validated `PopgenVCFArtifactLineage`.
#' @export
artifact_lineage_from_manifest <- function(
    manifest, executions, consumers = list(), execution_ids = NULL) {
  validate_artifact_manifest(manifest, must_exist = TRUE)
  if (!is.list(executions)) stop("executions must be a list", call. = FALSE)
  modules <- unique(vapply(manifest, `[[`, character(1L), "module"))
  if (is.null(execution_ids)) {
    execution_ids <- stats::setNames(paste0("exec:", modules), modules)
  }
  if (is.null(names(execution_ids)) || any(!modules %in% names(execution_ids))) {
    stop("execution_ids must map every artifact module", call. = FALSE)
  }
  execution_table <- if (length(executions)) {
    stats::setNames(vapply(executions, `[[`, character(1L), "id"),
                    vapply(executions, `[[`, character(1L), "module"))
  } else character()
  expected <- unname(execution_ids[modules])
  if (any(!expected %in% unname(execution_table))) {
    stop("executions do not contain every mapped producer", call. = FALSE)
  }
  artifacts <- lapply(manifest, function(x) {
    id <- paste(x$module, x$name, sep = "::")
    new_lineage_artifact(
      id = id, module = x$module, name = x$name, type = x$type,
      format = x$format, producer = unname(execution_ids[[x$module]]),
      consumers = as.character(consumers[[id]] %||% character()),
      path = x$path, metadata = x$metadata
    )
  })
  new_artifact_lineage(executions, artifacts)
}
