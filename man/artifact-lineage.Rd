\name{artifact-lineage}
\alias{new_lineage_execution}
\alias{new_lineage_artifact}
\alias{new_artifact_lineage}
\alias{validate_artifact_lineage}
\alias{lineage_execution_table}
\alias{lineage_artifact_table}
\alias{artifact_lineage_dag}
\alias{verify_artifact_lineage}
\alias{write_artifact_lineage}
\alias{set_project_artifact_lineage}
\title{Immutable artifact lineage}
\usage{
new_lineage_execution(
  id,
  module,
  status = c("complete", "planned", "running", "failed", "skipped"),
  parameters = list(),
  software = list(),
  started_at = NA_character_,
  completed_at = NA_character_
)

new_lineage_artifact(
  id,
  module,
  name,
  type,
  format,
  producer,
  consumers = character(),
  path = NULL,
  object = NULL,
  metadata = list()
)

new_artifact_lineage(executions = list(), artifacts = list())

validate_artifact_lineage(lineage)

lineage_execution_table(lineage)

lineage_artifact_table(lineage)

artifact_lineage_dag(lineage, validate = TRUE)

verify_artifact_lineage(lineage, objects = list())

write_artifact_lineage(
  lineage,
  directory,
  formats = c("tsv", "json", "graphml", "dot")
)

set_project_artifact_lineage(project, lineage)
}
\arguments{
  \item{id}{Stable execution or artifact identifier.}
  \item{module}{Analysis module name.}
  \item{status}{Execution status.}
  \item{parameters, software}{Named execution metadata lists.}
  \item{started_at, completed_at}{Optional timestamps.}
  \item{name}{Stable artifact name.}
  \item{type}{Artifact type.}
  \item{format}{Artifact format.}
  \item{producer}{Identifier of the single producing execution.}
  \item{consumers}{Identifiers of consuming executions.}
  \item{path}{Existing artifact file path.}
  \item{object}{In-memory artifact when no file exists.}
  \item{metadata}{Named artifact metadata.}
  \item{executions}{List of \code{PopgenVCFLineageExecution} records.}
  \item{artifacts}{List of \code{PopgenVCFLineageArtifact} records.}
  \item{lineage}{A \code{PopgenVCFArtifactLineage}.}
  \item{validate}{Whether to validate before deriving the DAG.}
  \item{objects}{Named list of current in-memory objects indexed by artifact ID.}
  \item{directory}{Output directory.}
  \item{formats}{Export formats: TSV, JSON, GraphML, or DOT.}
  \item{project}{A \code{PopgenVCFProject}.}
}
\value{
Constructors return validated lineage objects. Table functions return data tables.
\code{artifact_lineage_dag()} returns a \code{PopgenVCFProvenanceDAG}.
\code{verify_artifact_lineage()} returns \code{TRUE}. The writer returns paths to
created files, and \code{set_project_artifact_lineage()} returns an updated project.
}
\description{
Creates immutable module-execution and artifact records, assigns SHA256 content
identities, validates producer and consumer relationships, derives a provenance
DAG, verifies current content, exports interoperable lineage files, and embeds the
lineage in portable reproducible projects.
}
\details{
Each artifact has exactly one producer and zero or more consumers. File artifacts
are hashed from bytes. In-memory artifacts are hashed from serialized R content.
Artifact and execution identifiers occupy separate namespaces, and a producer
cannot consume its own artifact.
}
