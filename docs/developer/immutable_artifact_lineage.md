# Immutable artifact lineage

`PopgenVCFArtifactLineage` links module executions and scientific artifacts through the validated provenance DAG introduced in Phase 6.2.

## Identity model

Each execution has a stable identifier, module, status, parameter digest, software metadata, and timestamps. Each artifact has a stable identifier, one producer, zero or more consumers, and a SHA256 content identity.

File artifacts are hashed from their bytes. In-memory objects are hashed from serialized R content. An artifact therefore refers to a specific immutable scientific output rather than merely a pathname or logical name.

## Lineage graph

The derived graph uses:

- execution nodes with kind `analysis`;
- artifact nodes with kind `artifact`;
- `produces` edges from the producer execution to an artifact;
- `consumes` edges from an artifact to every downstream execution.

The existing provenance DAG validator enforces acyclicity, unique nodes and edges, and the absence of dangling references.

## Verification

`verify_artifact_lineage()` rehashes file content or supplied in-memory objects and fails when content differs from the recorded SHA256. Missing object payloads and missing files are reported explicitly.

## Project bundles

`set_project_artifact_lineage()` embeds the complete lineage object in `project$provenance$artifact_lineage` and records the lineage digest in the project component digests. Portable `.popgenvcf` bundles therefore retain execution, artifact, and dependency identities without requiring the original working directory for graph inspection.

## Exports

`write_artifact_lineage()` writes:

- execution, artifact, and edge TSV tables;
- a machine-readable JSON document;
- GraphML for Cytoscape, Gephi, and compatible graph tools;
- DOT for Graphviz rendering.

The graph exports are derived from the immutable records and are never treated as the authoritative store.
