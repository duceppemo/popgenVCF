# Phase 10.1.2 — public artifact and provenance adapters

.phase10_public_failure <- function(request, code, message, status = "rejected") {
  new_public_analysis_response(
    request = request,
    status = status,
    error = list(code = code, message = message)
  )
}

.phase10_public_artifact_id <- function(artifact) {
  paste(artifact$module, artifact$name, sep = "::")
}

.phase10_public_artifact_record <- function(artifact) {
  validate_analysis_artifact(artifact, must_exist = FALSE)
  list(
    schema_version = artifact$schema_version,
    module = artifact$module,
    name = artifact$name,
    type = artifact$type,
    format = artifact$format,
    description = artifact$description,
    required = isTRUE(artifact$required)
  )
}

#' List canonical public analysis artifacts
#'
#' Translates an existing artifact manifest into the stable Phase 10 public API
#' without exposing filesystem paths or implementation-specific metadata.
#'
#' @param request A canonical public request for `artifact.list`.
#' @param manifest An existing `PopgenVCFArtifactManifest`.
#' @return A validated `PopgenVCFPublicAPIResponse`.
#' @export
list_public_artifacts <- function(request, manifest) {
  request_valid <- tryCatch(
    {
      validate_public_analysis_request(request)
      TRUE
    },
    error = function(e) e
  )
  if (inherits(request_valid, "error")) stop(request_valid)
  if (!identical(request$operation_id, "artifact.list")) {
    return(.phase10_public_failure(
      request, "unsupported_operation",
      "This adapter accepts only artifact.list requests."
    ))
  }

  validated <- tryCatch(
    {
      validate_artifact_manifest(manifest, must_exist = FALSE)
      TRUE
    },
    error = function(e) e
  )
  if (inherits(validated, "error")) {
    return(.phase10_public_failure(
      request, "invalid_artifact_manifest", conditionMessage(validated)
    ))
  }

  if (!length(manifest)) {
    return(new_public_analysis_response(
      request = request,
      status = "completed",
      scientific_values = list(artifacts = list()),
      artifact_ids = setNames(character(), character())
    ))
  }

  ids <- vapply(manifest, .phase10_public_artifact_id, character(1))
  ord <- order(ids, method = "radix")
  ids <- ids[ord]
  records <- lapply(manifest[ord], .phase10_public_artifact_record)
  names(records) <- ids
  artifact_ids <- stats::setNames(ids, ids)

  new_public_analysis_response(
    request = request,
    status = "completed",
    scientific_values = list(artifacts = records),
    artifact_ids = artifact_ids
  )
}

.phase10_public_provenance_nodes <- function(dag) {
  nodes <- provenance_node_table(dag)
  if (!nrow(nodes)) return(list())
  nodes <- nodes[order(nodes$id, method = "radix"), , drop = FALSE]
  out <- lapply(seq_len(nrow(nodes)), function(i) {
    list(
      id = nodes$id[[i]],
      label = nodes$label[[i]],
      kind = nodes$kind[[i]],
      digest = nodes$digest[[i]],
      status = nodes$status[[i]]
    )
  })
  names(out) <- nodes$id
  out
}

.phase10_public_provenance_edges <- function(dag) {
  edges <- provenance_edge_table(dag)
  if (!nrow(edges)) return(list())
  keys <- paste(edges$from, edges$to, edges$relation, edges$artifact, sep = "|")
  ord <- order(keys, method = "radix")
  edges <- edges[ord, , drop = FALSE]
  keys <- keys[ord]
  out <- lapply(seq_len(nrow(edges)), function(i) {
    list(
      from = edges$from[[i]],
      to = edges$to[[i]],
      relation = edges$relation[[i]],
      artifact = edges$artifact[[i]]
    )
  })
  names(out) <- keys
  out
}

#' Inspect canonical public provenance
#'
#' Translates an existing provenance DAG into stable public node and edge
#' records. Timestamps, software details, parameters, and other mutable runtime
#' fields remain internal.
#'
#' @param request A canonical public request for `provenance.inspect`.
#' @param dag An existing `PopgenVCFProvenanceDAG`.
#' @return A validated `PopgenVCFPublicAPIResponse`.
#' @export
inspect_public_provenance <- function(request, dag) {
  request_valid <- tryCatch(
    {
      validate_public_analysis_request(request)
      TRUE
    },
    error = function(e) e
  )
  if (inherits(request_valid, "error")) stop(request_valid)
  if (!identical(request$operation_id, "provenance.inspect")) {
    return(.phase10_public_failure(
      request, "unsupported_operation",
      "This adapter accepts only provenance.inspect requests."
    ))
  }

  validated <- tryCatch(validate_provenance_dag(dag), error = function(e) e)
  if (inherits(validated, "error")) {
    return(.phase10_public_failure(
      request, "invalid_provenance_dag", conditionMessage(validated)
    ))
  }

  node_ids <- provenance_topological_order(dag)
  provenance_ids <- stats::setNames(node_ids, node_ids)
  values <- list(
    edges = .phase10_public_provenance_edges(dag),
    nodes = .phase10_public_provenance_nodes(dag),
    topological_order = node_ids
  )

  new_public_analysis_response(
    request = request,
    status = "completed",
    scientific_values = values,
    provenance_ids = provenance_ids
  )
}
