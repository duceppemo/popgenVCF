# Loaded after artifact_lineage.R to normalize empty edge collections.
artifact_lineage_dag <- function(lineage, validate = TRUE) {
  if (isTRUE(validate)) validate_artifact_lineage(lineage)
  execution_nodes <- lapply(lineage$executions, function(x) new_provenance_node(
    id = x$id, label = x$module, kind = "analysis", digest = x$digest,
    parameters = x$parameters, software = x$software, status = x$status,
    started_at = x$started_at, completed_at = x$completed_at))
  artifact_nodes <- lapply(lineage$artifacts, function(x) new_provenance_node(
    id = x$id, label = paste(x$module, x$name, sep = "::"), kind = "artifact",
    digest = x$sha256, parameters = x$metadata, status = "complete"))
  edges <- unlist(lapply(lineage$artifacts, function(x) {
    c(list(new_provenance_edge(x$producer, x$id, relation = "produces", artifact = x$id)),
      lapply(x$consumers, function(consumer) new_provenance_edge(
        x$id, consumer, relation = "consumes", artifact = x$id)))
  }), recursive = FALSE)
  if (is.null(edges)) edges <- list()
  new_provenance_dag(c(execution_nodes, artifact_nodes), edges)
}

#' Convert an artifact manifest to immutable lineage
#'
#' @param manifest A `PopgenVCFArtifactManifest` whose files exist.
#' @param executions List of lineage execution records.
#' @param consumers Optional named list mapping `module::name` artifact IDs to
#'   consuming execution IDs.
#' @param execution_ids Optional named character vector mapping module names to
#'   execution IDs. Defaults to `exec:<module>`.
#' @return A validated `PopgenVCFArtifactLineage`.
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
  execution_ids_present <- if (length(executions)) {
    vapply(executions, `[[`, character(1L), "id")
  } else character()
  expected <- unname(execution_ids[modules])
  if (any(!expected %in% execution_ids_present)) {
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
