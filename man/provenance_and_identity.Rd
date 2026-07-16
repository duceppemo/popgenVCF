\name{provenance_and_identity}
\alias{new_provenance_node}
\alias{new_provenance_edge}
\alias{new_provenance_dag}
\alias{add_provenance_node}
\alias{add_provenance_edge}
\alias{provenance_node_table}
\alias{provenance_edge_table}
\alias{provenance_topological_order}
\alias{provenance_ancestors}
\alias{provenance_descendants}
\alias{validate_provenance_dag}
\alias{new_sample_identity}
\alias{validate_sample_identity}
\alias{sample_identity_table}
\alias{resolve_sample_identity}
\alias{sample_identity_groups}
\title{Provenance DAGs and canonical sample identity}
\description{
Construct, validate, traverse, and tabulate acyclic scientific provenance graphs,
and build auditable sample identity tables that preserve immutable VCF identifiers
while supporting public aliases and richer biological groupings.
}
\usage{
new_provenance_node(id, label = id,
  kind = c("input", "transformation", "analysis", "artifact", "report"),
  digest = NA_character_, parameters = list(), software = list(),
  status = c("complete", "planned", "running", "failed", "skipped"),
  started_at = NA_character_, completed_at = NA_character_)

new_provenance_edge(from, to,
  relation = c("derived_from", "consumes", "produces", "documents"),
  artifact = NA_character_)

new_provenance_dag(nodes = list(), edges = list())
add_provenance_node(dag, node)
add_provenance_edge(dag, edge)
provenance_node_table(dag)
provenance_edge_table(dag)
provenance_topological_order(dag)
provenance_ancestors(dag, id)
provenance_descendants(dag, id)
validate_provenance_dag(dag)

new_sample_identity(metadata)
validate_sample_identity(x)
sample_identity_table(x, ordered = FALSE)
resolve_sample_identity(identity, sample_ids)
sample_identity_groups(identity)
}
\arguments{
  \item{id}{Stable provenance node identifier.}
  \item{label}{Human-readable node label.}
  \item{kind}{Provenance node kind.}
  \item{digest}{Optional content or component digest.}
  \item{parameters}{Named parameter metadata.}
  \item{software}{Named software metadata.}
  \item{status}{Execution status.}
  \item{started_at, completed_at}{Optional timestamps.}
  \item{from, to}{Parent and child node identifiers.}
  \item{relation}{Lineage relationship.}
  \item{artifact}{Optional artifact identifier.}
  \item{nodes, edges}{Lists of provenance nodes and edges.}
  \item{dag}{A \code{PopgenVCFProvenanceDAG}.}
  \item{node, edge}{Object to add to a graph.}
  \item{metadata}{Metadata containing the immutable \code{sample} column and optional identity columns.}
  \item{x}{A sample identity table or provenance object, as appropriate.}
  \item{ordered}{Whether to apply explicit display ordering.}
  \item{identity}{Metadata or a canonical sample identity table.}
  \item{sample_ids}{Immutable VCF/GDS sample identifiers.}
}
\value{
Constructors return validated S3 objects. Table functions return data tables.
Traversal functions return character node identifiers or public sample names.
}
\details{
The \code{sample} column remains the immutable genotype join key. Optional
\code{alias}, \code{individual}, \code{family}, \code{replicate}, and
\code{display_order} columns provide public naming and biological grouping
without changing VCF/GDS identity.
}
