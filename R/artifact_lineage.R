lineage_scalar <- function(x, label) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop(label, " must be one non-empty string", call. = FALSE)
  }
  x
}

lineage_named_list <- function(x, label) {
  if (!is.list(x) || (length(x) && (is.null(names(x)) || any(!nzchar(names(x)))))) {
    stop(label, " must be a named list", call. = FALSE)
  }
  x
}

lineage_hash_object <- function(x) {
  digest::digest(x, algo = "sha256", serialize = TRUE)
}

lineage_hash_file <- function(path) {
  if (!file.exists(path)) stop("artifact file does not exist: ", path, call. = FALSE)
  digest::digest(path, algo = "sha256", file = TRUE)
}

#' Create an immutable module-execution lineage record
#'
#' @param id Stable execution identifier.
#' @param module Analysis module name.
#' @param status Execution status.
#' @param parameters,software Named metadata lists.
#' @param started_at,completed_at Optional timestamps.
#' @return A `PopgenVCFLineageExecution`.
#' @export
new_lineage_execution <- function(
    id, module,
    status = c("complete", "planned", "running", "failed", "skipped"),
    parameters = list(), software = list(),
    started_at = NA_character_, completed_at = NA_character_) {
  id <- lineage_scalar(id, "id")
  module <- lineage_scalar(module, "module")
  status <- match.arg(status)
  parameters <- lineage_named_list(parameters, "parameters")
  software <- lineage_named_list(software, "software")
  payload <- list(module = module, status = status, parameters = parameters,
                  software = software, started_at = as.character(started_at)[1L],
                  completed_at = as.character(completed_at)[1L])
  structure(c(list(schema_version = "1.0", id = id), payload,
              list(digest = lineage_hash_object(payload))),
            class = "PopgenVCFLineageExecution")
}

#' Create an immutable artifact lineage record
#'
#' Exactly one of `path` or `object` must be supplied. File artifacts are hashed
#' from their bytes; in-memory artifacts are hashed from canonical R serialization.
#'
#' @param id Stable artifact identifier.
#' @param module,name,type,format Artifact metadata.
#' @param producer Producing execution identifier.
#' @param consumers Character vector of consuming execution identifiers.
#' @param path Existing file path.
#' @param object In-memory object when no file exists.
#' @param metadata Named metadata list.
#' @return A `PopgenVCFLineageArtifact`.
#' @export
new_lineage_artifact <- function(
    id, module, name, type, format, producer, consumers = character(),
    path = NULL, object = NULL, metadata = list()) {
  id <- lineage_scalar(id, "id")
  module <- lineage_scalar(module, "module")
  name <- lineage_scalar(name, "name")
  type <- lineage_scalar(type, "type")
  format <- tolower(lineage_scalar(format, "format"))
  producer <- lineage_scalar(producer, "producer")
  metadata <- lineage_named_list(metadata, "metadata")
  consumers <- unique(as.character(consumers))
  if (anyNA(consumers) || any(!nzchar(consumers))) stop("consumers must contain non-empty IDs", call. = FALSE)
  supplied <- c(!is.null(path), !is.null(object))
  if (sum(supplied) != 1L) stop("exactly one of path or object must be supplied", call. = FALSE)
  if (!is.null(path)) {
    path <- normalizePath(lineage_scalar(path, "path"), winslash = "/", mustWork = TRUE)
    digest <- lineage_hash_file(path)
    size_bytes <- unname(file.info(path)$size)
    source_type <- "file"
    object_class <- NA_character_
  } else {
    digest <- lineage_hash_object(object)
    size_bytes <- as.numeric(object.size(object))
    source_type <- "object"
    object_class <- paste(class(object), collapse = ",")
  }
  structure(list(
    schema_version = "1.0", id = id, module = module, name = name,
    type = type, format = format, producer = producer, consumers = consumers,
    source_type = source_type, path = if (is.null(path)) NA_character_ else path,
    object_class = object_class, sha256 = digest, size_bytes = size_bytes,
    metadata = metadata
  ), class = "PopgenVCFLineageArtifact")
}

#' Build immutable artifact lineage
#'
#' @param executions List of execution records.
#' @param artifacts List of artifact records.
#' @return A validated `PopgenVCFArtifactLineage`.
#' @export
new_artifact_lineage <- function(executions = list(), artifacts = list()) {
  lineage <- structure(list(schema_version = "1.0", executions = executions,
                            artifacts = artifacts),
                       class = "PopgenVCFArtifactLineage")
  validate_artifact_lineage(lineage)
  lineage$dag <- artifact_lineage_dag(lineage, validate = FALSE)
  lineage$digest <- lineage_hash_object(list(
    executions = lineage_execution_table(lineage),
    artifacts = lineage_artifact_table(lineage),
    edges = provenance_edge_table(lineage$dag)
  ))
  lineage
}

#' Validate immutable artifact lineage
#'
#' @param lineage A `PopgenVCFArtifactLineage`.
#' @return `lineage`, invisibly.
#' @export
validate_artifact_lineage <- function(lineage) {
  if (!inherits(lineage, "PopgenVCFArtifactLineage")) stop("lineage must be a PopgenVCFArtifactLineage", call. = FALSE)
  if (!is.list(lineage$executions) || !is.list(lineage$artifacts)) stop("lineage records must be lists", call. = FALSE)
  if (length(lineage$executions) && !all(vapply(lineage$executions, inherits, logical(1), "PopgenVCFLineageExecution"))) {
    stop("lineage contains an invalid execution", call. = FALSE)
  }
  if (length(lineage$artifacts) && !all(vapply(lineage$artifacts, inherits, logical(1), "PopgenVCFLineageArtifact"))) {
    stop("lineage contains an invalid artifact", call. = FALSE)
  }
  exec_ids <- vapply(lineage$executions, `[[`, character(1), "id")
  artifact_ids <- vapply(lineage$artifacts, `[[`, character(1), "id")
  if (anyDuplicated(exec_ids)) stop("execution IDs must be unique", call. = FALSE)
  if (anyDuplicated(artifact_ids)) stop("artifact IDs must be unique", call. = FALSE)
  if (length(intersect(exec_ids, artifact_ids))) stop("execution and artifact IDs must occupy separate namespaces", call. = FALSE)
  for (artifact in lineage$artifacts) {
    if (!artifact$producer %in% exec_ids) stop("unknown artifact producer: ", artifact$producer, call. = FALSE)
    unknown <- setdiff(artifact$consumers, exec_ids)
    if (length(unknown)) stop("unknown artifact consumer(s): ", paste(unknown, collapse = ", "), call. = FALSE)
    if (artifact$producer %in% artifact$consumers) stop("an execution cannot consume its own artifact", call. = FALSE)
    if (!grepl("^[0-9a-f]{64}$", artifact$sha256)) stop("artifact SHA256 is invalid", call. = FALSE)
  }
  invisible(lineage)
}

#' Convert lineage records to stable tables
#' @param lineage An artifact lineage object.
#' @return A data table.
#' @export
lineage_execution_table <- function(lineage) {
  validate_artifact_lineage(lineage)
  if (!length(lineage$executions)) return(data.table::data.table(
    id = character(), module = character(), status = character(), digest = character(),
    started_at = character(), completed_at = character()))
  data.table::rbindlist(lapply(lineage$executions, function(x) data.table::data.table(
    id = x$id, module = x$module, status = x$status, digest = x$digest,
    started_at = x$started_at, completed_at = x$completed_at)), fill = TRUE)
}

#' @rdname lineage_execution_table
#' @export
lineage_artifact_table <- function(lineage) {
  validate_artifact_lineage(lineage)
  if (!length(lineage$artifacts)) return(data.table::data.table(
    id = character(), module = character(), name = character(), type = character(),
    format = character(), producer = character(), consumers = character(),
    source_type = character(), path = character(), object_class = character(),
    sha256 = character(), size_bytes = numeric()))
  data.table::rbindlist(lapply(lineage$artifacts, function(x) data.table::data.table(
    id = x$id, module = x$module, name = x$name, type = x$type, format = x$format,
    producer = x$producer, consumers = paste(sort(x$consumers), collapse = ","),
    source_type = x$source_type, path = x$path, object_class = x$object_class,
    sha256 = x$sha256, size_bytes = x$size_bytes)), fill = TRUE)
}

#' Derive the provenance DAG from immutable lineage
#' @param lineage An artifact lineage object.
#' @param validate Validate the lineage before conversion.
#' @return A `PopgenVCFProvenanceDAG`.
#' @export
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
  new_provenance_dag(c(execution_nodes, artifact_nodes), edges)
}

#' Verify immutable artifact content
#'
#' File artifacts are rehashed from disk. Object artifacts require a named list of
#' current objects indexed by artifact ID.
#'
#' @param lineage An artifact lineage object.
#' @param objects Optional named list of current in-memory artifacts.
#' @return `TRUE`, or an error identifying changed content.
#' @export
verify_artifact_lineage <- function(lineage, objects = list()) {
  validate_artifact_lineage(lineage)
  for (artifact in lineage$artifacts) {
    actual <- if (artifact$source_type == "file") {
      lineage_hash_file(artifact$path)
    } else {
      if (is.null(objects[[artifact$id]])) stop("object artifact is unavailable for verification: ", artifact$id, call. = FALSE)
      lineage_hash_object(objects[[artifact$id]])
    }
    if (!identical(actual, artifact$sha256)) stop("artifact content changed: ", artifact$id, call. = FALSE)
  }
  TRUE
}

xml_escape <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

dot_escape <- function(x) gsub('"', '\\"', as.character(x), fixed = TRUE)

#' Export immutable artifact lineage
#'
#' @param lineage An artifact lineage object.
#' @param directory Output directory.
#' @param formats Any of `tsv`, `json`, `graphml`, and `dot`.
#' @return Named vector of written files.
#' @export
write_artifact_lineage <- function(lineage, directory,
                                   formats = c("tsv", "json", "graphml", "dot")) {
  validate_artifact_lineage(lineage)
  formats <- unique(match.arg(formats, c("tsv", "json", "graphml", "dot"), several.ok = TRUE))
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  nodes <- provenance_node_table(lineage$dag)
  edges <- provenance_edge_table(lineage$dag)
  written <- character()
  if ("tsv" %in% formats) {
    paths <- c(executions = file.path(directory, "lineage_executions.tsv"),
               artifacts = file.path(directory, "lineage_artifacts.tsv"),
               edges = file.path(directory, "lineage_edges.tsv"))
    data.table::fwrite(lineage_execution_table(lineage), paths[[1]], sep = "\t")
    data.table::fwrite(lineage_artifact_table(lineage), paths[[2]], sep = "\t")
    data.table::fwrite(edges, paths[[3]], sep = "\t")
    written <- c(written, paths)
  }
  if ("json" %in% formats) {
    path <- file.path(directory, "artifact_lineage.json")
    jsonlite::write_json(list(schema_version = lineage$schema_version,
                              digest = lineage$digest,
                              executions = lineage_execution_table(lineage),
                              artifacts = lineage_artifact_table(lineage), edges = edges),
                         path, pretty = TRUE, auto_unbox = TRUE, na = "null")
    written <- c(written, json = path)
  }
  if ("graphml" %in% formats) {
    path <- file.path(directory, "artifact_lineage.graphml")
    node_lines <- sprintf('    <node id="%s"><data key="label">%s</data><data key="kind">%s</data><data key="digest">%s</data></node>',
                          xml_escape(nodes$id), xml_escape(nodes$label), xml_escape(nodes$kind), xml_escape(nodes$digest))
    edge_lines <- sprintf('    <edge source="%s" target="%s"><data key="relation">%s</data><data key="artifact">%s</data></edge>',
                          xml_escape(edges$from), xml_escape(edges$to), xml_escape(edges$relation), xml_escape(edges$artifact))
    writeLines(c('<?xml version="1.0" encoding="UTF-8"?>',
                 '<graphml xmlns="http://graphml.graphdrawing.org/xmlns">',
                 '  <key id="label" for="node" attr.name="label" attr.type="string"/>',
                 '  <key id="kind" for="node" attr.name="kind" attr.type="string"/>',
                 '  <key id="digest" for="node" attr.name="digest" attr.type="string"/>',
                 '  <key id="relation" for="edge" attr.name="relation" attr.type="string"/>',
                 '  <key id="artifact" for="edge" attr.name="artifact" attr.type="string"/>',
                 '  <graph id="popgenVCF" edgedefault="directed">', node_lines, edge_lines,
                 '  </graph>', '</graphml>'), path)
    written <- c(written, graphml = path)
  }
  if ("dot" %in% formats) {
    path <- file.path(directory, "artifact_lineage.dot")
    node_lines <- sprintf('  "%s" [label="%s", shape=%s];', dot_escape(nodes$id),
                          dot_escape(nodes$label), ifelse(nodes$kind == "artifact", "box", "ellipse"))
    edge_lines <- sprintf('  "%s" -> "%s" [label="%s"];', dot_escape(edges$from),
                          dot_escape(edges$to), dot_escape(edges$relation))
    writeLines(c("digraph popgenVCF {", "  rankdir=LR;", node_lines, edge_lines, "}"), path)
    written <- c(written, dot = path)
  }
  normalizePath(written, winslash = "/", mustWork = TRUE)
}

#' Attach immutable lineage to a reproducible project
#'
#' @param project A `PopgenVCFProject`.
#' @param lineage A validated artifact lineage object.
#' @return Updated project with lineage embedded in provenance.
#' @export
set_project_artifact_lineage <- function(project, lineage) {
  validate_popgenvcf_project(project)
  validate_artifact_lineage(lineage)
  project$provenance$artifact_lineage <- lineage
  project$component_digests$artifact_lineage <- lineage$digest
  project
}
