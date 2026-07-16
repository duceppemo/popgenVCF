provenance_scalar <- function(x, label) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop(label, " must be one non-empty string", call. = FALSE)
  }
  x
}

#' Create a provenance node
#'
#' @param id Stable node identifier.
#' @param label Human-readable label.
#' @param kind Node kind.
#' @param digest Optional SHA256 or canonical component digest.
#' @param parameters,software Named metadata lists.
#' @param status Execution status.
#' @param started_at,completed_at Optional timestamps.
#' @return A `PopgenVCFProvenanceNode`.
#' @export
new_provenance_node <- function(
    id, label = id,
    kind = c("input", "transformation", "analysis", "artifact", "report"),
    digest = NA_character_, parameters = list(), software = list(),
    status = c("complete", "planned", "running", "failed", "skipped"),
    started_at = NA_character_, completed_at = NA_character_) {
  id <- provenance_scalar(id, "id")
  label <- provenance_scalar(label, "label")
  kind <- match.arg(kind)
  status <- match.arg(status)
  if (!is.list(parameters) || !is.list(software)) {
    stop("parameters and software must be lists", call. = FALSE)
  }
  structure(list(
    schema_version = "1.0", id = id, label = label, kind = kind,
    digest = as.character(digest)[1L], parameters = parameters,
    software = software, status = status,
    started_at = as.character(started_at)[1L],
    completed_at = as.character(completed_at)[1L]
  ), class = "PopgenVCFProvenanceNode")
}

#' Create a provenance edge
#'
#' @param from,to Parent and child node identifiers.
#' @param relation Lineage relationship.
#' @param artifact Optional artifact identifier carried along the edge.
#' @return A `PopgenVCFProvenanceEdge`.
#' @export
new_provenance_edge <- function(
    from, to,
    relation = c("derived_from", "consumes", "produces", "documents"),
    artifact = NA_character_) {
  from <- provenance_scalar(from, "from")
  to <- provenance_scalar(to, "to")
  if (identical(from, to)) stop("provenance edges cannot be self-referential", call. = FALSE)
  relation <- match.arg(relation)
  structure(list(
    schema_version = "1.0", from = from, to = to,
    relation = relation, artifact = as.character(artifact)[1L]
  ), class = "PopgenVCFProvenanceEdge")
}

#' Create and validate a provenance DAG
#'
#' @param nodes List of provenance nodes.
#' @param edges List of provenance edges.
#' @return A validated `PopgenVCFProvenanceDAG`.
#' @export
new_provenance_dag <- function(nodes = list(), edges = list()) {
  graph <- structure(list(
    schema_version = "1.0", nodes = nodes, edges = edges
  ), class = "PopgenVCFProvenanceDAG")
  validate_provenance_dag(graph)
  graph
}

#' Add a node or edge to a provenance DAG
#' @param dag A provenance DAG.
#' @param node,edge Object to add.
#' @return An updated validated DAG.
#' @export
add_provenance_node <- function(dag, node) {
  validate_provenance_dag(dag)
  if (!inherits(node, "PopgenVCFProvenanceNode")) stop("node is invalid", call. = FALSE)
  dag$nodes[[length(dag$nodes) + 1L]] <- node
  validate_provenance_dag(dag)
  dag
}

#' @rdname add_provenance_node
#' @export
add_provenance_edge <- function(dag, edge) {
  validate_provenance_dag(dag)
  if (!inherits(edge, "PopgenVCFProvenanceEdge")) stop("edge is invalid", call. = FALSE)
  dag$edges[[length(dag$edges) + 1L]] <- edge
  validate_provenance_dag(dag)
  dag
}

#' Convert provenance objects to stable tables
#' @param dag A provenance DAG.
#' @return A data table.
#' @export
provenance_node_table <- function(dag) {
  if (!inherits(dag, "PopgenVCFProvenanceDAG")) stop("dag is invalid", call. = FALSE)
  if (!length(dag$nodes)) return(data.table::data.table(
    id = character(), label = character(), kind = character(), digest = character(),
    status = character(), started_at = character(), completed_at = character()
  ))
  data.table::rbindlist(lapply(dag$nodes, function(x) data.table::data.table(
    id = x$id, label = x$label, kind = x$kind, digest = x$digest,
    status = x$status, started_at = x$started_at, completed_at = x$completed_at
  )), fill = TRUE)
}

#' @rdname provenance_node_table
#' @export
provenance_edge_table <- function(dag) {
  if (!inherits(dag, "PopgenVCFProvenanceDAG")) stop("dag is invalid", call. = FALSE)
  if (!length(dag$edges)) return(data.table::data.table(
    from = character(), to = character(), relation = character(), artifact = character()
  ))
  data.table::rbindlist(lapply(dag$edges, function(x) data.table::data.table(
    from = x$from, to = x$to, relation = x$relation, artifact = x$artifact
  )), fill = TRUE)
}

#' Return deterministic topological order
#' @param dag A provenance DAG.
#' @return Character node identifiers.
#' @export
provenance_topological_order <- function(dag) {
  nodes <- provenance_node_table(dag)$id
  edges <- provenance_edge_table(dag)
  if (!length(nodes)) return(character())
  indegree <- stats::setNames(integer(length(nodes)), nodes)
  if (nrow(edges)) {
    counts <- table(edges$to)
    indegree[names(counts)] <- as.integer(counts)
  }
  ready <- sort(names(indegree)[indegree == 0L])
  order <- character()
  while (length(ready)) {
    current <- ready[[1L]]
    ready <- ready[-1L]
    order <- c(order, current)
    children <- sort(edges[from == current, to])
    for (child in children) {
      indegree[[child]] <- indegree[[child]] - 1L
      if (indegree[[child]] == 0L) ready <- sort(unique(c(ready, child)))
    }
  }
  if (length(order) != length(nodes)) stop("provenance graph contains a cycle", call. = FALSE)
  order
}

#' Validate a provenance DAG
#' @param dag A provenance DAG.
#' @return `dag`, invisibly.
#' @export
validate_provenance_dag <- function(dag) {
  if (!inherits(dag, "PopgenVCFProvenanceDAG")) stop("dag must be a PopgenVCFProvenanceDAG", call. = FALSE)
  if (!is.list(dag$nodes) || !is.list(dag$edges)) stop("dag nodes and edges must be lists", call. = FALSE)
  if (length(dag$nodes) && !all(vapply(dag$nodes, inherits, logical(1L), "PopgenVCFProvenanceNode"))) {
    stop("dag contains an invalid node", call. = FALSE)
  }
  if (length(dag$edges) && !all(vapply(dag$edges, inherits, logical(1L), "PopgenVCFProvenanceEdge"))) {
    stop("dag contains an invalid edge", call. = FALSE)
  }
  nodes <- provenance_node_table(dag)
  edges <- provenance_edge_table(dag)
  if (anyDuplicated(nodes$id)) stop("provenance node IDs must be unique", call. = FALSE)
  if (nrow(edges)) {
    if (any(!edges$from %in% nodes$id) || any(!edges$to %in% nodes$id)) {
      stop("provenance graph contains dangling edges", call. = FALSE)
    }
    edge_key <- paste(edges$from, edges$to, edges$relation, edges$artifact, sep = "|")
    if (anyDuplicated(edge_key)) stop("provenance edges must be unique", call. = FALSE)
  }
  provenance_topological_order(dag)
  invisible(dag)
}

provenance_reachable <- function(dag, id, direction = c("ancestors", "descendants")) {
  validate_provenance_dag(dag)
  id <- provenance_scalar(id, "id")
  nodes <- provenance_node_table(dag)$id
  if (!id %in% nodes) stop("unknown provenance node: ", id, call. = FALSE)
  direction <- match.arg(direction)
  edges <- provenance_edge_table(dag)
  seen <- character(); frontier <- id
  while (length(frontier)) {
    current <- frontier[[1L]]; frontier <- frontier[-1L]
    next_ids <- if (direction == "ancestors") edges[to == current, from] else edges[from == current, to]
    next_ids <- setdiff(next_ids, c(id, seen))
    seen <- unique(c(seen, next_ids)); frontier <- unique(c(frontier, next_ids))
  }
  sort(seen)
}

#' Trace provenance ancestors or descendants
#' @param dag A provenance DAG.
#' @param id Node identifier.
#' @return Character node identifiers.
#' @export
provenance_ancestors <- function(dag, id) provenance_reachable(dag, id, "ancestors")

#' @rdname provenance_ancestors
#' @export
provenance_descendants <- function(dag, id) provenance_reachable(dag, id, "descendants")
