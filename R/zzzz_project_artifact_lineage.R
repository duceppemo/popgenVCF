# Compatibility override loaded after artifact_lineage.R and projects.R.
# The complete lineage object remains embedded in project.rds, while project.json
# receives only plain, stable tables that jsonlite can serialize portably.
set_project_artifact_lineage <- function(project, lineage) {
  validate_popgenvcf_project(project)
  validate_artifact_lineage(lineage)

  project$artifacts$artifact_lineage <- lineage
  project$component_digests$artifacts <- project_component_digests(project$artifacts)
  project$component_digests$artifact_lineage <- lineage$digest
  project$provenance$artifact_lineage <- list(
    schema_version = lineage$schema_version,
    digest = lineage$digest,
    execution_count = length(lineage$executions),
    artifact_count = length(lineage$artifacts),
    executions = lineage_execution_table(lineage),
    artifacts = lineage_artifact_table(lineage),
    edges = provenance_edge_table(lineage$dag)
  )
  project
}
